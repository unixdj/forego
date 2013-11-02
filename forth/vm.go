// Copyright 2011 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

package forth

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

type Cell uint32
const cellSize = 4

const (
	forthFalse = Cell(0)
	forthTrue = ^forthFalse
	stackDepth = 32
	ramSize = 0x10000	// should be enough for everyone
	A_funcpad = ramSize - 2 * cellSize
)

// Word flags
const (
	immediate	= 0x80
	variable	= 0x40
	all		= 0xc0
)

type vmStack []Cell

type VM struct {
	in		bufio.Reader
	out		io.Writer
	pc, lastpc	Cell		// program counter
	stack, rstack	vmStack
	debug		bool
	Mem		[ramSize]byte
}

// stack basics
func (s *vmStack) depth() Cell {
	return Cell(len(*s))
}

func (s *vmStack) clear() {
	*s = (*s)[:0]
}

func (s *vmStack) need(down, up int) {
	if len(*s) < down {
		panic("stack underflow")
	}
	if len(*s) + up > cap(*s) {
		panic("stack overflow")
	}
}

func (s *vmStack) push(c Cell) {
	s.need(0, 1)
	*s = append(*s, c)
}

func (s *vmStack) pop() Cell {
	s.need(1, 0)
	var c Cell
	*s, c = (*s)[0 : len(*s)-1], (*s)[len(*s)-1]
	return c
}

func (s *vmStack) peek() Cell {
	s.need(1, 0)
	return (*s)[len(*s)-1]
}

// mnemonic for arguments: pick/roll <size> cells from depth <from>
func (s *vmStack) pick(size, from int) {
	s.need(from + size, size)
	*s = append(*s, (*s)[len(*s)-from-size : len(*s)-from]...)
}

func (s *vmStack) roll(size, from int) {
	s.need(from + size, 0)
	buf := make([]Cell, size)
	l := len(*s)
	copy(buf, (*s)[l-from-size : l-from])
	copy((*s)[l-from-size : l-size], (*s)[l-from : ])
	copy((*s)[l-size : ], buf)
}

// memory ops with address checking
func (vm *VM) readByte(a Cell) Cell {
	if a >= ramSize {
		panic(fmt.Sprintf("illegal address %08x", a))
	}
	return Cell(vm.Mem[a])
}

func (vm *VM) readCell(a Cell) Cell {
	if a >= ramSize {
		panic(fmt.Sprintf("illegal address %08x", a))
	} else if a % cellSize != 0 {
		panic(fmt.Sprintf("address %08x not aligned", a))
	}
	var v Cell
	for i := 0; i < cellSize; i++ {
		v = v << 8 | Cell(vm.Mem[a])
		a++
	}
	return v;
}

func (vm *VM) writeByte(a Cell, v Cell) {
	if a >= ramSize {
		panic(fmt.Sprintf("illegal address %08x", a))
	}
	vm.Mem[a] = byte(v)
}

func (vm *VM) writeCell(a Cell, v Cell) {
	if a > ramSize - cellSize {
		panic(fmt.Sprintf("illegal address %08x", a))
	} else if a % cellSize != 0 {
		panic(fmt.Sprintf("address %08x not aligned", a))
	}
	copy(vm.Mem[a:], []byte{ byte(v>>24), byte(v>>16), byte(v>>8), byte(v) })
}

func (vm *VM) readSlice(a, l Cell) []byte {
	if a >= ramSize || a + l > ramSize {
		panic(fmt.Sprintf("illegal address %08x", a))
	}
	return vm.Mem[a : a+l]
}

// primitives

func (vm *VM) unimplemented() {
	panic("unimplemented")
}

// nop ( -- )
func (vm *VM) nop() {
}

// bye ( -- )
func (vm *VM) bye() {
	panic("bye")
}

// eof ( -- )
func (vm *VM) eof() {
	panic("eof")
}

// exit ( -- ) ( R: nest-sys -- )
func (vm *VM) exit() {
	vm.pc = vm.rstack.pop()
}

// (abort) ( i*x -- )
func (vm *VM) abortHelper() {
	vm.stack.clear()
}

// (quit) ( R: i*x -- )
func (vm *VM) quitHelper() {
	vm.rstack.clear()
}

func disas(v Cell) string {
        var primitives = []string{
                "nop",
                "exit",
                "(abort)",
                "(quit)",
                // stack
                "pick",
                "roll",
                "depth",
                "drop",
                // 0x08
                "2drop",
                "?dup",
                "nip",
                "tuck",
                // rstack
                ">r",
                "r>",
                "r@",
                "rdrop",
                // 0x10
                // basic memory access
                "@",
                "!",
                "c@",
                "c!",
                // more memory access
                "2!",
                "2@",
                "+!",
                "erase",
                // 0x18
                "fill",
                "move",
                // comparison
                "=",
                "<>",
                "<",
                ">",
                "u<",
                "u>",
                // 0x20
                "0<",
                "0>",
                // logic
                "0=",
                "0<>",
                // bitwise logic
                "invert",
                "and",
                "or",
                "xor",
                // 0x28
                "lshift",
                "rshift",
                "2*",
                "2/",
                // arithmetics
                "1+",
                "1-",
                "+",
                "-",
                // 0x30
                "*",
                "/",
                "mod",
                "/mod",
                "*/",
                "*/mod",
                "m*",
                "um*",
                // 0x38
                "fm/mod",
                "sm/rem",
                "um/mod",
                "abs",
                "",
                "",
                "",
                "",
                // 0x40
                // io
                "key",
                "emit",
                // compiling!
                "(refill)",
                "(parse)",
                "(parse-word)",
                "(.)",
                "builtin-words",
                "trace",
                //
                "type",
                "execute",
                "builtin(find)",
                "(trynum)",
                "",
                "",
                "dump",
                "",
                // 0x50
                "bye",
                "eof",
        }

	switch v>>28 {
	case 0:
		if v < Cell(len(primitives)) {
			return primitives[v]
		}
	case 1:
		return fmt.Sprintf("call %08x", v&0x0fffffff)
	case 2:
		return fmt.Sprintf("jmp %08x", v&0x0fffffff)
	case 3:
		return fmt.Sprintf("jz %08x", v&0x0fffffff)
	case 4:
		v &= 0x0fffffff
		s := v >> (litSignBit + 1)
		v &= litNumMask
		if v & (1<<litSignBit) != 0 {
			v |= 0xff000000
		}
		return fmt.Sprintf("push %08x", v << s)
	case 5:
		if v &^ 0xf0033f3f == 0 {
			return fmt.Sprintf("%s %d from %d in %s",
				[]string{"pick", "roll"}[v>>17&1],
				int(v>>8 & 0x3f), int(v & 0x3f),
				[]string{"stack", "rstack"}[v>>16&1])
		}
	}
	return ""
}

func printable(c Cell) rune {
	if c >= 0x20 && c < 0x7f {
		return rune(c)
	}
	return '.'
}

// dump ( addr-a u -- )
func (vm *VM) dump() {
	l := vm.stack.pop() >> 2
	for a := vm.stack.pop(); l > 0 ; a, l = a+cellSize, l-1 {
		v := vm.readCell(a)
		fmt.Fprintf(vm.out, "%08x: %08x  %c%c%c%c  %s\n",
		   a, v, printable(v>>24 & 0xff), printable(v>>16 & 0xff),
		   printable(v>>8 & 0xff), printable(v & 0xff), disas(v))
	}
}

// (refill) ( addr -- u -1 | 0 )
func (vm *VM) refill() {
	tib := vm.stack.pop()
	line, _, err := vm.in.ReadLine()
	if err != nil {
		vm.stack.push(forthFalse)
		return
	}
	copy(vm.Mem[tib:], []byte(line))  // crash!
	vm.stack.push(Cell(len(line)))
	vm.stack.push(forthTrue)
}

func isdelim(b, delim Cell) bool {
	if delim == ' ' {
		return b <= 0x20
	}
	return b == delim
}

func (vm *VM) doParse(delim, source Cell) {
	ntib := vm.readCell(source)
	tib := vm.readCell(source + cellSize)
	toin := vm.readCell(source + 2*cellSize)
	vm.trace("ntib %08x, tib %08x, toin %08x; ", ntib, tib, toin)
	for delim == ' ' && toin < ntib && isdelim(vm.readByte(tib + toin), delim) {
		toin++
	}
	start := toin
	for toin < ntib && !isdelim(vm.readByte(tib + toin), delim) {
		toin++
	}
	vm.stack.push(tib + start)
	vm.stack.push(toin - start)
	if toin < ntib {
		toin++
	}
	vm.writeCell(source + 2*cellSize, toin)
	vm.trace("start %08x, toin %08x, word %s\n", start, toin, vm.readSlice(tib + start, toin - start))
}

// parse ( char "ccc<char>" -- c-addr u )
func (vm *VM) parse() {
	source := vm.stack.pop()
	vm.doParse(vm.stack.pop(), source)
}

// parse-word ( "<spaces>name" -- c-addr u )
func (vm *VM) parseWord() {
	source := vm.stack.pop()
	vm.doParse(' ', source)
}

func (vm *VM) putc(c Cell) {
	b := []byte { byte(c) }
	if _, err := vm.out.Write(b); err != nil {
		panic(err)
	}
}

func (vm *VM) emit () {
	vm.putc(vm.stack.pop())
}

func (vm *VM) _type() {
	l := vm.stack.pop()
	a := vm.stack.pop()
	for l > 0 {
		vm.putc(vm.readByte(a))
		a++
		l--
	}
}

// execute ( i*x xt -- j*x )
func (vm *VM) execute() {
	a := vm.stack.pop()
	if a & 1 != 0 {
		a--
		vm.stack.push(a + cellSize)
	}
	a = vm.readCell(a)
	// handle nop and exit
	switch a {
	case 0:
		return
	case 1:
		vm.exit()
		return
	}
	switch a & instrMask {
	default:		// primitive, push, pickRoll, illegal
		// <instruction> exit
		copy(vm.Mem[A_funcpad:], []byte{
			byte(a>>24), byte(a>>16), byte(a>>8), byte(a),
			0, 0, 0, 1,  // exit
		})
		a = A_funcpad
		fallthrough
	case instrCall:
		vm.call(a & instrParamMask)
	case instrJz:
		if vm.stack.pop() != 0 {
			return
		}
		fallthrough
	case instrJmp:
		vm.jmp(a & instrParamMask)
	}
}

func tolower(b byte) byte {
	if b >= 'A' && b <= 'Z' {
		return b + 'a'-'A'
	}
	return b
}

func weq(a, b []byte) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if tolower(a[i]) != tolower(b[i]) {
			return false
		}
	}
	return true
}

func align(a Cell) Cell {
	a += cellSize - 1
	return a - a % cellSize
}

// builtin(find) ( addr u -- addr u 0 | xt 1 | xt -1 )
func (vm *VM) find() {
	w := vm.stack.pop()
	fl := vm.stack.pop()
	fa := vm.stack.pop()
	fw := vm.readSlice(fa, fl)
	vm.trace("word %s\n", fw)
	for w := vm.readCell(w); w != 0; w = vm.readCell(w) {
		l := vm.readByte(w)
		w++
		vm.trace("@%08x: ", w)
		vm.trace("%s <=> %s\n", fw, vm.readSlice(w, l & 0x1f))
		if weq(fw, vm.readSlice(w, l & 0x1f)) {
			w = align(w + l & 0x1f + cellSize)
			if l & 0x40 != 0 {
				w |= 1
			}
			vm.stack.push(w)
			if l & immediate != 0 {
				vm.stack.push(1)
			} else {
				vm.stack.push(forthTrue)
			}
			return
		}
		w = align(w + l & 0x1f)
	}
	vm.stack.push(fa)
	vm.stack.push(fl)
	vm.stack.push(forthFalse)
}

// builtin-words ( addr -- )
func (vm *VM) words() {
	for w := vm.readCell(vm.stack.pop()); w != 0; w = vm.readCell(w) {
		l := vm.readByte(w) & 0x1f
		w++;
		fmt.Fprintf(vm.out, "%-16s", vm.readSlice(w, l))
		w = align(w + l)
	}
	fmt.Fprintf(vm.out, "\n")
}

// trace ( flag -- )
func (vm *VM) setTrace() {
	vm.debug = vm.stack.pop() != 0
}

func (vm *VM) trynumber() {
	base := vm.stack.pop()
	l := vm.stack.pop()
	a := vm.stack.pop()
	s := vm.readSlice(a, l)
	if n, err := strconv.ParseInt(string(s), int(base), cellSize * 8 + 1); err == nil {
		vm.stack.push(Cell(n))
		vm.stack.push(forthTrue)
		return
	}
	vm.stack.push(a)
	vm.stack.push(l)
	vm.stack.push(forthFalse)
}

// @ ( a-addr -- x )
func (vm *VM) fetch() {
	vm.stack.push(vm.readCell(vm.stack.pop()))
}

// c@ ( c-addr -- char )
func (vm *VM) cFetch() {
	vm.stack.push(vm.readByte(vm.stack.pop()))
}

// 2@ ( a-addr -- x1 x2 )
func (vm *VM) twoFetch() {
	a := vm.stack.pop()
	vm.stack.push(vm.readCell(a + cellSize))
	vm.stack.push(vm.readCell(a))
}

// ! ( x a-addr -- )
func (vm *VM) store() {
	a := vm.stack.pop()
	vm.writeCell(a, vm.stack.pop())
}

// c! ( char c-addr -- )
func (vm *VM) cStore() {
	a := vm.stack.pop()
	vm.writeByte(a, vm.stack.pop())
}

// 2! ( x1 x2 a-addr -- )
func (vm *VM) twoStore() {
	a := vm.stack.pop()
	vm.writeCell(a, vm.stack.pop())
	vm.writeCell(a + cellSize, vm.stack.pop())
}

// +! ( n|u a-addr -- )
func (vm *VM) plusStore() {
	a := vm.stack.pop()
	vm.writeCell(a, vm.readCell(a) + vm.stack.pop())
}

func forthBool(b bool) Cell {
	if b {
		return forthTrue
	}
	return forthFalse
}

// = ( x1 x2 -- flag )
func (vm *VM) equals() {
	vm.stack.push(forthBool(vm.stack.pop() == vm.stack.pop()))
}

// <> ( x1 x2 -- flag )
func (vm *VM) notEquals() {
	vm.stack.push(forthBool(vm.stack.pop() != vm.stack.pop()))
}

// < ( n1 n2 -- flag )
func (vm *VM) lessThan() {
	c := int(vm.stack.pop())
	vm.stack.push(forthBool(int(vm.stack.pop()) < c))
}

// > ( n1 n2 -- flag )
func (vm *VM) greaterThan() {
	c := int(vm.stack.pop())
	vm.stack.push(forthBool(int(vm.stack.pop()) > c))
}

// u< ( u1 u2 -- flag )
func (vm *VM) uLessThan() {
	c := vm.stack.pop()
	vm.stack.push(forthBool(vm.stack.pop() < c))
}

// u> ( u1 u2 -- flag )
func (vm *VM) uGreaterThan() {
	c := vm.stack.pop()
	vm.stack.push(forthBool(vm.stack.pop() > c))
}

// 0< ( n -- flag )
func (vm *VM) zeroLess() {
	vm.stack.push(forthBool(int(vm.stack.pop()) < 0))
}

// 0> ( n -- flag )
func (vm *VM) zeroGreater() {
	vm.stack.push(forthBool(int(vm.stack.pop()) > 0))
}

// 0<> ( x -- flag )
func (vm *VM) zeroNotEquals() {
	vm.stack.push(forthBool(vm.stack.pop() != forthFalse))
}

// 0= ( x -- flag )
func (vm *VM) zeroEquals() {
	vm.stack.push(forthBool(vm.stack.pop() == forthFalse))
}

// invert ( x1 -- x2 )
func (vm *VM) invert() {
	vm.stack.push(^vm.stack.pop())
}

// and ( x1 x2 -- x3 )
func (vm *VM) and() {
	vm.stack.push(vm.stack.pop() & vm.stack.pop())
}

// or ( x1 x2 -- x3 )
func (vm *VM) or() {
	vm.stack.push(vm.stack.pop() | vm.stack.pop())
}

// xor ( x1 x2 -- x3 )
func (vm *VM) xor() {
	vm.stack.push(vm.stack.pop() ^ vm.stack.pop())
}

// lshift ( x1 u -- x2 )
func (vm *VM) lShift() {
	c := vm.stack.pop()
	vm.stack.push(vm.stack.pop() << c)
}

// rshift ( x1 u -- x2 )
func (vm *VM) rShift() {
	c := vm.stack.pop()
	vm.stack.push(vm.stack.pop() >> c)
}

// 2* ( x1 -- x2 )
func (vm *VM) twoStar() {
	vm.stack.push(vm.stack.pop() << 1)
}

// 2/ ( x1 -- x2 )
func (vm *VM) twoSlash() {
	vm.stack.push(Cell(int(vm.stack.pop()) >> 1))
}

// 1+ ( n1|u1 -- n2|u2 )
func (vm *VM) onePlus() {
	vm.stack.push(vm.stack.pop() + 1)
}

// 1- ( n1|u1 -- n2|u2 )
func (vm *VM) oneMinus() {
	vm.stack.push(vm.stack.pop() - 1)
}

// + ( n1|u1 n2|u2 -- n3|u3 )
func (vm *VM) plus() {
	vm.stack.push(vm.stack.pop() + vm.stack.pop())
}

// - ( n1|u1 n2|u2 -- n3|u3 )
func (vm *VM) minus() {
	c := vm.stack.pop()
	vm.stack.push(vm.stack.pop() - c)
}

// * ( n1|u1 n2|u2 -- n3|u3 )
func (vm *VM) star() {
	vm.stack.push(vm.stack.pop() * vm.stack.pop())
}

// / ( n1 n2 -- n3 )
func (vm *VM) slash() {
	c := int(vm.stack.pop())
	if c == 0 {
		panic("zero division")
	}
	vm.stack.push(Cell(int(vm.stack.pop()) / c))
}

// mod ( n1 n2 -- n3 )
func (vm *VM) mod() {
	c := int(vm.stack.pop())
	if c == 0 {
		panic("zero division")
	}
	vm.stack.push(Cell(int(vm.stack.pop()) % c))
}

// /mod ( n1 n2 -- n3 n4 )
func (vm *VM) slashMod() {
	b := int(vm.stack.pop())
	a := int(vm.stack.pop())
	if b == 0 {
		panic("zero division")
	}
	vm.stack.push(Cell(a % b))
	vm.stack.push(Cell(a / b))
}

// */ ( n1 n2 n3 -- n4 )
func (vm *VM) starSlash() {
	c := int64(vm.stack.pop())
	if c == 0 {
		panic("zero division")
	}
	p := int64(vm.stack.pop()) * int64(vm.stack.pop())
	vm.stack.push(Cell(p / c))
}

// */mod ( n1 n2 n3 -- n4 n5 )
func (vm *VM) starSlashMod() {
	c := int64(vm.stack.pop())
	if c == 0 {
		panic("zero division")
	}
	p := int64(vm.stack.pop()) * int64(vm.stack.pop())
	vm.stack.push(Cell(p % c))
	vm.stack.push(Cell(p / c))
}

// (.) ( n u -- )
func (vm *VM) dot() {
	base := int(vm.stack.pop())
	fmt.Fprintf(vm.out, " %s ", strconv.FormatInt(int64(int(vm.stack.pop())), base))
}

// pick ( xu ... x1 x0 u -- xu ... x1 x0 xu )
func (vm *VM) pick() {
	vm.stack.pick(1, int(vm.stack.pop()))
}

// roll ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
func (vm *VM) roll() {
	vm.stack.roll(1, int(vm.stack.pop()))
}

// dup  pick(1, 0)
// over pick(1, 1)
// swap roll(1, 1)
// rot  roll(1, 2)
// -rot roll(2, 1)

// depth ( -- +n )
func (vm *VM) depth() {
	vm.stack.push(vm.stack.depth())
}

// drop ( x -- )
func (vm *VM) drop() {
	vm.stack.pop()
}

// 2drop ( x1 x2 -- )
func (vm *VM) twoDrop() {
	vm.stack.pop()
	vm.stack.pop()
}

// ?dup ( x -- 0 | x x )
func (vm *VM) questionDup() {
	c := vm.stack.peek()
	if c != 0 {
		vm.stack.push(c)
	}
}

// nip ( x1 x2 -- x2 )
func (vm *VM) nip() {
	vm.stack.roll(1, 1)  // swap
	vm.stack.pop()       // drop
}

// tuck ( x1 x2 -- x2 x1 x2 )
func (vm *VM) tuck() {
	vm.stack.pick(1, 0)  // dup
	vm.stack.roll(2, 1)  // -rot
}

// >r ( x -- ) ( R:  -- x )
func (vm *VM) toR() {
	vm.rstack.push(vm.stack.pop())
}

// r> ( -- x ) ( R:  x -- )
func (vm *VM) rFrom() {
	vm.stack.push(vm.rstack.pop())
}

// r@ ( -- x ) ( R:  x -- x )
func (vm *VM) rFetch() {
	vm.stack.push(vm.rstack.peek())
}

// rdrop ( R: x -- )
func (vm *VM) rDrop() {
	vm.rstack.pop()
}

var primitives = []struct{ name string; f func(*VM) } {
	{ "nop",	(*VM).nop },
	{ "exit",	(*VM).exit },
	{ "(abort)",	(*VM).abortHelper },
	{ "(quit)",	(*VM).quitHelper },
	// stack
	{ "pick",	(*VM).pick },
	{ "roll",	(*VM).roll },
	{ "depth",	(*VM).depth },
	{ "drop",	(*VM).drop },
	// 0x08
	{ "2drop",	(*VM).twoDrop },
	{ "?dup",	(*VM).questionDup },
	{ "nip",	(*VM).nip },
	{ "tuck",	(*VM).tuck },
	// rstack
	{ ">r",		(*VM).toR },
	{ "r>",		(*VM).rFrom },
	{ "r@",		(*VM).rFetch },
	{ "rdrop",	(*VM).rDrop },
	// 0x10
	// basic memory access
	{ "@",		(*VM).fetch },
	{ "!",		(*VM).store },
	{ "c@",		(*VM).cFetch },
	{ "c!",		(*VM).cStore },
	// more memory access
	{ "2!",		(*VM).twoStore },
	{ "2@",		(*VM).twoFetch },
	{ "+!",		(*VM).plusStore },
	{ "erase",	(*VM).unimplemented },
	// 0x18
	{ "fill",	(*VM).unimplemented },
	{ "move",	(*VM).unimplemented },
	// comparison
	{ "=",		(*VM).equals },
	{ "<>",		(*VM).notEquals },
	{ "<",		(*VM).lessThan },
	{ ">",		(*VM).greaterThan },
	{ "u<",		(*VM).uLessThan },
	{ "u>",		(*VM).uGreaterThan },
	// 0x20
	{ "0<",		(*VM).zeroLess },
	{ "0>",		(*VM).zeroGreater },
	// logic
	{ "0=",		(*VM).zeroEquals },
	{ "0<>",	(*VM).zeroNotEquals },
	// bitwise logic
	{ "invert",	(*VM).invert },
	{ "and",	(*VM).and },
	{ "or",		(*VM).or },
	{ "xor",	(*VM).xor },
	// 0x28
	{ "lshift",	(*VM).lShift },
	{ "rshift",	(*VM).rShift },
	{ "2*",		(*VM).twoStar },
	{ "2/",		(*VM).twoSlash },
	// arithmetics
	{ "1+",		(*VM).onePlus },
	{ "1-",		(*VM).oneMinus },
	{ "+",		(*VM).plus },
	{ "-",		(*VM).minus },
	// 0x30
	{ "*",		(*VM).star },
	{ "/",		(*VM).slash },
	{ "mod",	(*VM).mod },
	{ "/mod",	(*VM).slashMod },
	{ "*/",		(*VM).starSlash },
	{ "*/mod",	(*VM).starSlashMod },
	{ "m*",		(*VM).unimplemented },
	{ "um*",	(*VM).unimplemented },
	// 0x38
	{ "fm/mod",	(*VM).unimplemented },
	{ "sm/rem",	(*VM).unimplemented },
	{ "um/mod",	(*VM).unimplemented },
	{ "abs",	(*VM).unimplemented },
	{ "",		(*VM).unimplemented },
	{ "",		(*VM).unimplemented },
	{ "",		(*VM).unimplemented },
	{ "",		(*VM).unimplemented },
	// 0x40
	// io
	{ "key",	(*VM).unimplemented },
	{ "emit",	(*VM).emit },
	// compiling!
	{ "(refill)",	(*VM).refill },
	{ "(parse)",	(*VM).parse },
	{ "(parse-word)",	(*VM).parseWord },
	{ "(.)",		(*VM).dot },
	{ "builtin-words",	(*VM).words },
	{ "trace",	(*VM).setTrace },
	//
	{ "type",	(*VM)._type },
	{ "execute",	(*VM).execute },
	{ "builtin(find)",	(*VM).find },
	{ "(trynum)",	(*VM).trynumber },
	{ "",		(*VM).unimplemented },
	{ "",		(*VM).unimplemented },
	{ "dump",	(*VM).dump },
	{ "",		(*VM).unimplemented },
	// 0x50
	{ "bye",	(*VM).bye },
	{ "eof",	(*VM).eof },
}

func (vm *VM) primitive(param Cell) {
	vm.trace("%02x: %s\n", param, primitives[param].name)
	if (param >= Cell(len(primitives))) {
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
	litNumMask = 1 << (litSignBit + 1) - 1
	litSameBits = ^Cell(1<<litSignBit - 1)
	litShiftMask = instrParamMask &^ litNumMask
	litShiftShift = litSignBit + 1
	litMaxShift = 1 << (instrShift - litShiftShift) - 1
)

// (0100) sssN  nnnn nnnn  nnnn nnnn  nnnn nnnn
// s...  = shift bits
// Nn... = number bits
// (N    = sign, extended left)
func (vm *VM) push(param Cell) {
	s := param >> (litSignBit + 1)
	vm.trace("push: param %08x, shift %d => ", param, s)
	param &= litNumMask
	if param & (1<<litSignBit) != 0 {
		param |= ^Cell(litNumMask)
	}
	vm.trace("%08x\n", param << s)
	vm.stack.push(param << s)
}

// (0101) 0000  0000 00os  00ww wwww  00ff ffff
// o = operation (0: pick, 1: roll)
// s = stack (0: data stack, 1: rstack)
// w = width
// f = from
func (vm *VM) pickRoll(param Cell) {
	vm.trace("%s %d from %d in %s\n",
		[]string{"pick", "roll"}[param>>17&1],
		int(param>>8 & 0x3f),
		int(param & 0x3f),
		[]string{"stack", "rstack"}[param>>16&1])
	s := []*vmStack{ &vm.stack, &vm.rstack }[param>>16 & 1]
	f := []func(*vmStack, int, int){ (*vmStack).pick, (*vmStack).roll }[param>>17 & 1]
	f(s, int(param>>8 & 0x3f), int(param & 0x3f))
}

// iiii pppp  pppp pppp  pppp pppp  pppp pppp
var instructions = []func(vm *VM, param Cell) {
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
	instrShift = 28
	instrParamMask = (1<<instrShift) - 1
	instrMask = ^Cell(instrParamMask)
)

func (vm *VM) trace(format string, a ...interface{}) {
	if vm.debug {
		fmt.Fprintf(vm.out, format, a...)
	}
}

func (vm *VM) step() (ret bool) {
	defer func () {
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
			for _, v := range(vm.stack) {
				fmt.Fprintf(vm.out, " %08x", v)
			}
			fmt.Fprintf(vm.out, "\n")
			vm.pc = 0
			ret = false	// ?
		}
	}()
	ret = true
	vm.lastpc = vm.pc
	c := vm.readCell(vm.pc)
	i := c & instrMask >> instrShift
	vm.trace("@ %08x: ", vm.pc)
//	fmt.Fprintf(vm.out, "@ %08x: %08x;  s:", vm.pc, c)
//	for _, v := range(vm.stack) {
//		fmt.Fprintf(vm.out, " %08x", v)
//	}
//	fmt.Fprintf(vm.out, "\n")
	if i >= Cell(len(instructions)) {
		panic(fmt.Sprintf("illegal instruction %08x", c))
	}
	vm.pc += cellSize
	instructions[i](vm, c & instrParamMask)
	for _, v := range(vm.stack) {
		vm.trace(" %08x", v)
	}
	vm.trace("\n")
	return true
}

func (vm *VM) Run() {
	for vm.step() { }
	/*
	for vm.getword() != "" {
		vm.doword()
		vm.out.Write([]byte("    ok\n"))
	}
	*/
}

func NewVM(in io.Reader, out io.Writer) *VM {
	vm := &VM{
		in: *bufio.NewReader(io.MultiReader(strings.NewReader(fmt.Sprintf("%d %d evaluate\n", ramSize / 2, len(softcore))), in)),
		out: out,
		stack: make([]Cell, 0, stackDepth),
		rstack: make([]Cell, 0, stackDepth),
		//debug: true,
	}
	copy(vm.Mem[:], kernel)

	vm.trace("hi\n")

	copy(vm.Mem[ramSize / 2 : ], softcore)

	return vm
}
