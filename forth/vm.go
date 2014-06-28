// Copyright 2011, 2013 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

package forth

import (
	"bufio"
	"fmt"
	"io"
)

// Cell matches the FORTH virtual machine cell type.
type Cell uint32

type (
	sCell  int32
	dCell  uint64
	sdCell int64
)

// String formats c as a zero-padded eight-digit lowercase
// hexadecimal number.
func (c Cell) String() string {
	var buf [8]byte
	for i := 7; i >= 0; i-- {
		buf[i] = "0123456789abcdef"[c&0x0f]
		c >>= 4
	}
	return string(buf[:])
}

// Instr represents a virtual machine instruction.
type Instr Cell

const (
	cellSize   = 4
	cellBits   = 32
	forthFalse = Cell(0)
	forthTrue  = ^forthFalse
)

// VM memory size in bytes
const MemSize = 0x40000

// VM implements a Forego FORTH virtual machine.
//
// Execution starts at address 0 and ends when a trap is raised.
// Possible causes for a trap are:
//	* BYE or EOF instruction runs
//	* stack overflow
//	* stack underflow
//	* illegal memory access (beyond end of memory)
//	* unaligned cell-level memory access
//	* zero division on one of:
//	  / MOD /MOD */ */MOD FM/MOD SM/REM UM/MOD UD/MOD
//	* illegal instruction
//	* I/O error on KEY or EMIT (except EOF)
// Cell-level memory access includes fetching and storing cells
// using instructions @ ! 2@ 2! +! as well as fetching the next
// instruction pointed to by the program counter.  Byte-level
// memory access is is provided by C@ and C! instructions.
// When a trap is raised other than by BYE or EOF, the program
// counter is reset to 0.
type VM struct {
	in            bufio.Reader  // input
	out           io.Writer     // output
	pc, lastpc    Cell          // program counter
	icell         Instr         // current instruction
	stack, rstack Stack         // data and return stacks
	debug         bool          // tracing enabled
	Mem           [MemSize]byte // memory
}

func (vm *VM) trace(format string, a ...interface{}) {
	if vm.debug {
		fmt.Fprintf(vm.out, format, a...)
	}
}

// memory ops with address checking
func (vm *VM) readByte(a Cell) (Cell, error) {
	if a >= MemSize {
		return 0, vm.newErrorAddr(IllegalAddress, a)
	}
	return Cell(vm.Mem[a]), nil
}

func (vm *VM) readCell(a Cell) (Cell, error) {
	if a >= MemSize {
		return 0, vm.newErrorAddr(IllegalAddress, a)
	} else if a%cellSize != 0 {
		return 0, vm.newErrorAddr(UnalignedAddress, a)
	}
	return Cell(vm.Mem[a])<<24 | Cell(vm.Mem[a+1])<<16 |
		Cell(vm.Mem[a+2])<<8 | Cell(vm.Mem[a+3]), nil
}

func (vm *VM) writeByte(a, v Cell) error {
	if a >= MemSize {
		return vm.newErrorAddr(IllegalAddress, a)
	}
	vm.Mem[a] = byte(v)
	return nil
}

func (vm *VM) writeCell(a, v Cell) error {
	if a >= MemSize {
		return vm.newErrorAddr(IllegalAddress, a)
	} else if a%cellSize != 0 {
		return vm.newErrorAddr(UnalignedAddress, a)
	}
	copy(vm.Mem[a:],
		[]byte{byte(v >> 24), byte(v >> 16), byte(v >> 8), byte(v)})
	return nil
}

func (vm *VM) primitive(param Cell) error {
	if param >= Cell(len(primitives)) {
		return IllegalInstruction
	}
	return primitives[param].f(vm)
}

func (vm *VM) call(param Cell) error {
	if err := vm.rstack.push(vm.pc); err != nil {
		return rstackError(err)
	}
	vm.pc = param
	return nil
}

func (vm *VM) jmp(param Cell) error {
	vm.pc = param
	return nil
}

func (vm *VM) jz(param Cell) error {
	c, err := vm.stack.pop()
	switch {
	case err != nil:
		return err
	case c == 0:
		vm.trace("jz: jumping\n")
		vm.pc = param
	default:
		vm.trace("jz: staying\n")
	}
	return nil
}

const (
	litSignBit = 24
	litNumMask = Cell(1<<(litSignBit+1) - 1)
)

// (0100) sssN  nnnn nnnn  nnnn nnnn  nnnn nnnn
// s...  = shift bits
// Nn... = number bits
// (N    = sign, extended left)
func (vm *VM) push(param Cell) error {
	s := param >> (litSignBit + 1)
	param &= litNumMask
	if param&(1<<litSignBit) != 0 {
		param |= ^litNumMask
	}
	return vm.stack.push(param << s)
}

// (0101) 0000  0000 00os  00ww wwww  00ff ffff
// o = operation (0: pick, 1: roll)
// s = stack (0: data stack, 1: rstack)
// w = width
// f = from
func (vm *VM) pickRoll(param Cell) error {
	if param&^0x00033f3f != 0 {
		return IllegalInstruction
	}
	var (
		r = param >> 16 & 1
		s = []*Stack{&vm.stack, &vm.rstack}[r]
		f = []func(*Stack, int, int) error{
			(*Stack).pick, (*Stack).roll,
		}[param>>17&1]
	)
	err := f(s, int(param>>8&0x3f), int(param&0x3f))
	if r != 0 {
		return rstackError(err)
	}
	return err
}

// iiii pppp  pppp pppp  pppp pppp  pppp pppp
var instructions = []func(vm *VM, param Cell) error{
	(*VM).primitive,
	(*VM).call,
	(*VM).jmp,
	(*VM).jz,
	(*VM).push,
	(*VM).pickRoll,
}

const (
	instrShift     = 28
	instrParamMask = (1 << instrShift) - 1
)

// Step rus the instruction pointed to by the program counter,
// advancing the latter.  Step returns an error of type Error if
// the instruction raises a VM trap, or nil.  In case a trap is
// raised other than by BYE or EOF, the program counter is reset
// to 0.
func (vm *VM) Step() error {
	if ic, err := vm.readCell(vm.pc); err != nil {
		return err
	} else {
		vm.icell = Instr(ic)
	}
	if vm.debug {
		vm.trace("@ %v: %v\n", vm.pc, vm.icell)
	}
	vm.lastpc = vm.pc
	op := vm.icell >> instrShift
	if op >= Instr(len(instructions)) {
		return vm.newError(IllegalInstruction)
	}
	vm.pc += cellSize
	err := instructions[op](vm, Cell(vm.icell&instrParamMask))
	if err != nil {
		if en, ok := err.(Errno); ok {
			if en != Bye && en != EOF {
				vm.pc = 0
			}
			return vm.newError(en)
		}
		_ = err.(*Error)
		return err
	}
	if vm.debug {
		vm.trace("%v  R: %v\n", vm.stack, vm.rstack)
	}
	_, err = vm.readCell(vm.pc)
	return err
}

// Run runs vm until a trap is raised.  Run returns nil if the
// trap was raised by the BYE instruction, and an Error
// describing the trap otherwise.
func (vm *VM) Run() error {
	var err error
	for err == nil {
		err = vm.Step()
	}
	if err.(*Error).Errno == Bye {
		return nil
	}
	return err
}

// NewVM creates a new VM whose KEY reads from in and EMIT
// writes to out.  The Mem of the new VM is initialized with the
// default kernel, and the program counter is set to 0.
func NewVM(in io.Reader, out io.Writer) *VM {
	vm := &VM{
		in:     *bufio.NewReader(in),
		out:    out,
		stack:  make([]Cell, 0, stackDepth),
		rstack: make([]Cell, 0, stackDepth),
		//debug: true,
	}
	for a := Cell(0); a < MemSize; a += cellSize {
		vm.writeCell(a, 0xdeadbeef)
	}
	copy(vm.Mem[:], kernel)
	vm.trace("hi\n")
	return vm
}

// String returns the lower case string representation of instr.
// Primitives are represented as bare FORTH words.  The PICK/ROLL
// instruction is formatted as "pick", "roll", "rpick" or
// "rroll", followed by the WIDTH and FROM parameters in
// hexadecimal, thereby distinguishing it from the PICK and ROLL
// primitives.  Other instructions are represented as "call",
// "jmp", "jz" or "push", followed by the parameter as an eight
// digit hexadecimal number.  For illegal instructions, the
// string "illegal instruction" is returned.
func (instr Instr) String() string {
	var (
		op = instr >> instrShift
		s  string
	)
	if op < Instr(len(instructions)) {
		s = []func(param Cell) string{
			func(param Cell) string {
				if param >= Cell(len(primitives)) {
					return ""
				}
				return primitives[param].name
			},
			func(param Cell) string {
				return "call " + param.String()
			},
			func(param Cell) string {
				return "jmp " + param.String()
			},
			func(param Cell) string {
				return "jz " + param.String()
			},
			func(param Cell) string {
				s := param >> (litSignBit + 1)
				param &= litNumMask
				if param&(1<<litSignBit) != 0 {
					param |= ^litNumMask
				}
				return "push " + (param << s).String()
			},
			func(param Cell) string {
				if param&^0x00033f3f != 0 {
					return ""
				}
				return fmt.Sprintf("%s%s %x %x",
					[]string{"", "r"}[param>>16&1],
					[]string{"pick", "roll"}[param>>17&1],
					int(param>>8&0x3f),
					int(param&0x3f))
			},
		}[op](Cell(instr & instrParamMask))
	}
	if s == "" {
		return "illegal instruction"
	}
	return s
}
