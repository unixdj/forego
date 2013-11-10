// Copyright 2011, 2013 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

package forth

import (
	"bufio"
	"fmt"
	"io"
)

type (
	Cell   uint32
	SCell  int32
	DCell  uint64
	SDCell int64
)

const (
	cellSize   = 4
	cellBits   = 32
	forthFalse = Cell(0)
	forthTrue  = ^forthFalse
	MemSize    = 0x40000
)

type VM struct {
	in            bufio.Reader
	out           io.Writer
	pc, lastpc    Cell // program counter
	stack, rstack vmStack
	debug         bool
	Mem           [MemSize]byte
}

// memory ops with address checking
func (vm *VM) readByte(a Cell) Cell {
	if a >= MemSize {
		panic(fmt.Sprintf("illegal address %08x", a))
	}
	return Cell(vm.Mem[a])
}

func (vm *VM) readCell(a Cell) Cell {
	if a >= MemSize {
		panic(fmt.Sprintf("illegal address %08x", a))
	} else if a%cellSize != 0 {
		panic(fmt.Sprintf("address %08x not aligned", a))
	}
	var v Cell
	for i := 0; i < cellSize; i++ {
		v = v<<8 | Cell(vm.Mem[a])
		a++
	}
	return v
}

func (vm *VM) writeByte(a Cell, v Cell) {
	if a >= MemSize {
		panic(fmt.Sprintf("illegal address %08x", a))
	}
	vm.Mem[a] = byte(v)
}

func (vm *VM) writeCell(a Cell, v Cell) {
	if a > MemSize-cellSize {
		panic(fmt.Sprintf("illegal address %08x", a))
	} else if a%cellSize != 0 {
		panic(fmt.Sprintf("address %08x not aligned", a))
	}
	copy(vm.Mem[a:],
		[]byte{byte(v >> 24), byte(v >> 16), byte(v >> 8), byte(v)})
}

func (vm *VM) readSlice(a, l Cell) []byte {
	if a >= MemSize || a+l > MemSize {
		panic(fmt.Sprintf("illegal address %08x", a))
	}
	return vm.Mem[a : a+l]
}

func (vm *VM) primitive(param Cell) {
	vm.trace("%02x: %s\n", param, primitives[param].name)
	if param >= Cell(len(primitives)) {
		panic(fmt.Sprintf("illegal instruction %08x", param))
	}
	primitives[param].f(vm)
}

func (vm *VM) call(param Cell) {
	vm.trace("call %08x\n", param)
	vm.rstack.push(vm.pc)
	vm.pc = param
}

func (vm *VM) jmp(param Cell) {
	vm.trace("jmp %08x\n", param)
	vm.pc = param
}

func (vm *VM) jz(param Cell) {
	vm.trace("jz %08x ", param)
	if vm.stack.pop() == 0 {
		vm.trace("(jumping)\n")
		vm.pc = param
	} else {
		vm.trace("(staying)\n")
	}
}

const (
	litSignBit = 24
	litNumMask = 1<<(litSignBit+1) - 1
)

// (0100) sssN  nnnn nnnn  nnnn nnnn  nnnn nnnn
// s...  = shift bits
// Nn... = number bits
// (N    = sign, extended left)
func (vm *VM) push(param Cell) {
	s := param >> (litSignBit + 1)
	param &= litNumMask
	if param&(1<<litSignBit) != 0 {
		param |= ^Cell(litNumMask)
	}
	vm.trace("push %08x\n", param<<s)
	vm.stack.push(param << s)
}

// (0101) 0000  0000 00os  00ww wwww  00ff ffff
// o = operation (0: pick, 1: roll)
// s = stack (0: data stack, 1: rstack)
// w = width
// f = from
func (vm *VM) pickRoll(param Cell) {
	vm.trace("%s%s %d %d\n",
		[]string{"", "r"}[param>>16&1],
		[]string{"pick", "roll"}[param>>17&1],
		int(param>>8&0x3f),
		int(param&0x3f))
	s := []*vmStack{&vm.stack, &vm.rstack}[param>>16&1]
	f := []func(*vmStack, int, int){(*vmStack).pick, (*vmStack).roll}[param>>17&1]
	f(s, int(param>>8&0x3f), int(param&0x3f))
}

// iiii pppp  pppp pppp  pppp pppp  pppp pppp
var instructions = []func(vm *VM, param Cell){
	(*VM).primitive,
	(*VM).call,
	(*VM).jmp,
	(*VM).jz,
	(*VM).push,
	(*VM).pickRoll,
}

const (
	instrPrimitive = iota << instrShift
	instrCall
	instrJmp
	instrJz
	instrPush
	instrPickRoll
)

const (
	instrShift     = 28
	instrParamMask = (1 << instrShift) - 1
	instrMask      = ^Cell(instrParamMask)
)

func (vm *VM) trace(format string, a ...interface{}) {
	if vm.debug {
		fmt.Fprintf(vm.out, format, a...)
	}
}

func (vm *VM) instr() {
	vm.lastpc = vm.pc
	c := vm.readCell(vm.pc)
	i := c & instrMask >> instrShift
	vm.trace("@ %08x: ", vm.pc)
	if i >= Cell(len(instructions)) {
		panic(fmt.Sprintf("illegal instruction %08x", c))
	}
	vm.pc += cellSize
	instructions[i](vm, c&instrParamMask)
	if vm.pc >= MemSize {
		panic(fmt.Sprintf("illegal address %08x", vm.pc))
	} else if vm.pc%cellSize != 0 {
		panic(fmt.Sprintf("address %08x not aligned", vm.pc))
	}
}

func (vm *VM) step() (ret bool) {
	defer func() {
		if r := recover(); r != nil {
			if s, ok := r.(string); ok {
				switch s {
				case "eof":
					fmt.Fprintf(vm.out, "  EOF\n")
					fallthrough
				case "bye":
					ret = false
					return
				}
			}
			fmt.Fprintf(vm.out, "\npanic: %s at %08x\nstack:", r, vm.lastpc)
			for _, v := range vm.stack {
				fmt.Fprintf(vm.out, " %08x", v)
			}
			fmt.Fprintf(vm.out, "\n")
			vm.pc = 0
			ret = false // ?
		}
	}()
	ret = true
	vm.instr()
	if vm.debug {
		for _, v := range vm.stack {
			vm.trace("%08x ", v)
		}
		vm.trace(" R:")
		for _, v := range vm.rstack {
			vm.trace(" %08x", v)
		}
		vm.trace("\n")
	}
	return true
}

func (vm *VM) Run() {
	for vm.step() {
	}
}

func NewVM(in io.Reader, out io.Writer) *VM {
	vm := &VM{
		in:     *bufio.NewReader(in),
		out:    out,
		stack:  make([]Cell, 0, stackDepth),
		rstack: make([]Cell, 0, stackDepth),
		//debug: true,
	}
	copy(vm.Mem[:], kernel)
	vm.trace("hi\n")
	return vm
}
