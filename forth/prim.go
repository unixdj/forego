// Copyright 2011, 2013 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

package forth

import "io"

func (vm *VM) unimplemented() {
	panic("unimplemented")
}

// nop ( -- )
func (vm *VM) nop() {
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

// pick ( xu ... x1 x0 u -- xu ... x1 x0 xu )
func (vm *VM) pick() {
	vm.stack.pick(1, int(vm.stack.pop()))
}

// roll ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
func (vm *VM) roll() {
	vm.stack.roll(1, int(vm.stack.pop()))
}

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
	vm.stack.roll(1, 1) // swap
	vm.stack.pop()      // drop
}

// tuck ( x1 x2 -- x2 x1 x2 )
func (vm *VM) tuck() {
	vm.stack.pick(1, 0) // dup
	vm.stack.roll(2, 1) // -rot
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

// @ ( a-addr -- x )
func (vm *VM) fetch() {
	vm.stack.push(vm.readCell(vm.stack.pop()))
}

// ! ( x a-addr -- )
func (vm *VM) store() {
	a := vm.stack.pop()
	vm.writeCell(a, vm.stack.pop())
}

// c@ ( c-addr -- char )
func (vm *VM) cFetch() {
	vm.stack.push(vm.readByte(vm.stack.pop()))
}

// c! ( char c-addr -- )
func (vm *VM) cStore() {
	a := vm.stack.pop()
	vm.writeByte(a, vm.stack.pop())
}

// 2@ ( a-addr -- x1 x2 )
func (vm *VM) twoFetch() {
	a := vm.stack.pop()
	vm.stack.push(vm.readCell(a + cellSize))
	vm.stack.push(vm.readCell(a))
}

// 2! ( x1 x2 a-addr -- )
func (vm *VM) twoStore() {
	a := vm.stack.pop()
	vm.writeCell(a, vm.stack.pop())
	vm.writeCell(a+cellSize, vm.stack.pop())
}

// +! ( n|u a-addr -- )
func (vm *VM) plusStore() {
	a := vm.stack.pop()
	vm.writeCell(a, vm.readCell(a)+vm.stack.pop())
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
	c := SCell(vm.stack.pop())
	vm.stack.push(forthBool(SCell(vm.stack.pop()) < c))
}

// > ( n1 n2 -- flag )
func (vm *VM) greaterThan() {
	c := SCell(vm.stack.pop())
	vm.stack.push(forthBool(SCell(vm.stack.pop()) > c))
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
	vm.stack.push(forthBool(SCell(vm.stack.pop()) < 0))
}

// 0> ( n -- flag )
func (vm *VM) zeroGreater() {
	vm.stack.push(forthBool(SCell(vm.stack.pop()) > 0))
}

// 0= ( x -- flag )
func (vm *VM) zeroEquals() {
	vm.stack.push(forthBool(vm.stack.pop() == forthFalse))
}

// 0<> ( x -- flag )
func (vm *VM) zeroNotEquals() {
	vm.stack.push(forthBool(vm.stack.pop() != forthFalse))
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
	vm.stack.push(Cell(SCell(vm.stack.pop()) >> 1))
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
	c := SCell(vm.stack.pop())
	if c == 0 {
		panic("zero division")
	}
	vm.stack.push(Cell(SCell(vm.stack.pop()) / c))
}

// mod ( n1 n2 -- n3 )
func (vm *VM) mod() {
	c := SCell(vm.stack.pop())
	if c == 0 {
		panic("zero division")
	}
	vm.stack.push(Cell(SCell(vm.stack.pop()) % c))
}

// /mod ( n1 n2 -- n3 n4 )
func (vm *VM) slashMod() {
	b := SCell(vm.stack.pop())
	a := SCell(vm.stack.pop())
	if b == 0 {
		panic("zero division")
	}
	vm.stack.push(Cell(a % b))
	vm.stack.push(Cell(a / b))
}

// */ ( n1 n2 n3 -- n4 )
func (vm *VM) starSlash() {
	c := SDCell(SCell(vm.stack.pop()))
	if c == 0 {
		panic("zero division")
	}
	p := SDCell(SCell(vm.stack.pop())) * SDCell(SCell(vm.stack.pop()))
	vm.stack.push(Cell(p / c))
}

// */mod ( n1 n2 n3 -- n4 n5 )
func (vm *VM) starSlashMod() {
	c := SDCell(SCell(vm.stack.pop()))
	if c == 0 {
		panic("zero division")
	}
	p := SDCell(SCell(vm.stack.pop())) * SDCell(SCell(vm.stack.pop()))
	vm.stack.push(Cell(p % c))
	vm.stack.push(Cell(p / c))
}

// m* ( n1 n2 -- d )
func (vm *VM) mStar() {
	vm.stack.pushd(DCell(SDCell(SCell(vm.stack.pop())) *
		SDCell(SCell(vm.stack.pop()))))
}

// um* ( u1 u2 -- ud )
func (vm *VM) umStar() {
	vm.stack.pushd(DCell(vm.stack.pop()) * DCell(vm.stack.pop()))
}

// fm/mod ( d1 n1 -- n2 n3 )
func (vm *VM) fmSlashMod() {
	// (a - (a<0 ? b-1 : 0)) / b
	b := SDCell(SCell(vm.stack.pop()))
	if b == 0 {
		panic("zero division")
	}
	a := SDCell(SCell(vm.stack.popd()))
	q := a / b
	if (a%b != 0) && ((a < 0) != (b < 0)) {
		q--
	}
	vm.stack.push(Cell(a - (b * q)))
	vm.stack.push(Cell(q))
}

// sm/rem ( d1 n1 -- n2 n3 )
func (vm *VM) smSlashRem() {
	b := SDCell(SCell(vm.stack.pop()))
	if b == 0 {
		panic("zero division")
	}
	a := SDCell(vm.stack.popd())
	vm.stack.push(Cell(a % b))
	vm.stack.push(Cell(a / b))
}

// um/mod ( ud n2 -- n3 n4 )
func (vm *VM) umSlashMod() {
	b := DCell(vm.stack.pop())
	if b == 0 {
		panic("zero division")
	}
	a := vm.stack.popd()
	vm.stack.push(Cell(a % b))
	vm.stack.push(Cell(a / b))
}

// negate ( n1 -- n2 )
func (vm *VM) negate() {
	vm.stack.push(Cell(-SCell(vm.stack.pop())))
}

// ud+ ( ud1 ud2 -- ud3 )
func (vm *VM) udPlus() {
	vm.stack.pushd(vm.stack.popd() + vm.stack.popd())
}

// ud- ( ud1 ud2 -- ud3 )
func (vm *VM) udMinus() {
	d := vm.stack.popd()
	vm.stack.pushd(vm.stack.popd() - d)
}

// ud* ( ud1 ud2 -- ud3 )
func (vm *VM) udStar() {
	vm.stack.pushd(vm.stack.popd() * vm.stack.popd())
}

// ud/mod ( ud1 ud2 -- ud3 ud4 )
func (vm *VM) udSlashMod() {
	b := vm.stack.popd()
	if b == 0 {
		panic("zero division")
	}
	a := vm.stack.popd()
	vm.stack.pushd(a % b)
	vm.stack.pushd(a / b)
}

// key ( -- char )
func (vm *VM) key() {
	switch b, err := vm.in.ReadByte(); err {
	case nil:
		vm.stack.push(Cell(b))
	case io.EOF:
		vm.stack.push(Cell(forthTrue))
	default:
		panic(err)
	}
}

// emit ( char -- )
func (vm *VM) emit() {
	b := []byte{byte(vm.stack.pop())}
	if _, err := vm.out.Write(b); err != nil {
		panic(err)
	}
}

// trace ( flag -- )
func (vm *VM) setTrace() {
	vm.debug = vm.stack.pop() != 0
}

// bye ( -- )
func (vm *VM) bye() {
	panic("bye")
}

// eof ( -- )
func (vm *VM) eof() {
	panic("eof")
}

var primitives = []struct {
	name string
	f    func(*VM)
}{
	{"nop", (*VM).nop},
	{"exit", (*VM).exit},
	{"(abort)", (*VM).abortHelper},
	{"(quit)", (*VM).quitHelper},
	// stack
	{"pick", (*VM).pick},
	{"roll", (*VM).roll},
	{"depth", (*VM).depth},
	{"drop", (*VM).drop},
	// 0x08
	{"2drop", (*VM).twoDrop},
	{"?dup", (*VM).questionDup},
	{"nip", (*VM).nip},
	{"tuck", (*VM).tuck},
	// rstack
	{">r", (*VM).toR},
	{"r>", (*VM).rFrom},
	{"r@", (*VM).rFetch},
	{"rdrop", (*VM).rDrop},
	// 0x10
	// basic memory access
	{"@", (*VM).fetch},
	{"!", (*VM).store},
	{"c@", (*VM).cFetch},
	{"c!", (*VM).cStore},
	// more memory access
	{"2!", (*VM).twoStore},
	{"2@", (*VM).twoFetch},
	{"+!", (*VM).plusStore},
	{"", (*VM).unimplemented},
	// 0x18
	{"", (*VM).unimplemented},
	{"", (*VM).unimplemented},
	// comparison
	{"=", (*VM).equals},
	{"<>", (*VM).notEquals},
	{"<", (*VM).lessThan},
	{">", (*VM).greaterThan},
	{"u<", (*VM).uLessThan},
	{"u>", (*VM).uGreaterThan},
	// 0x20
	{"0<", (*VM).zeroLess},
	{"0>", (*VM).zeroGreater},
	// logic
	{"0=", (*VM).zeroEquals},
	{"0<>", (*VM).zeroNotEquals},
	// bitwise logic
	{"invert", (*VM).invert},
	{"and", (*VM).and},
	{"or", (*VM).or},
	{"xor", (*VM).xor},
	// 0x28
	{"lshift", (*VM).lShift},
	{"rshift", (*VM).rShift},
	{"2*", (*VM).twoStar},
	{"2/", (*VM).twoSlash},
	// arithmetics
	{"1+", (*VM).onePlus},
	{"1-", (*VM).oneMinus},
	{"+", (*VM).plus},
	{"-", (*VM).minus},
	// 0x30
	{"*", (*VM).star},
	{"/", (*VM).slash},
	{"mod", (*VM).mod},
	{"/mod", (*VM).slashMod},
	{"*/", (*VM).starSlash},
	{"*/mod", (*VM).starSlashMod},
	{"m*", (*VM).mStar},
	{"um*", (*VM).umStar},
	// 0x38
	{"fm/mod", (*VM).fmSlashMod},
	{"sm/rem", (*VM).smSlashRem},
	{"um/mod", (*VM).umSlashMod},
	{"negate", (*VM).negate},
	{"ud+", (*VM).udPlus},
	{"ud-", (*VM).udMinus},
	{"ud*", (*VM).udStar},
	{"ud/mod", (*VM).udSlashMod},
	// 0x40
	// io
	{"key", (*VM).key},
	{"emit", (*VM).emit},
	// compiling!
	{"", (*VM).unimplemented},
	{"", (*VM).unimplemented},
	{"", (*VM).unimplemented},
	{"", (*VM).unimplemented},
	{"", (*VM).unimplemented},
	{"trace", (*VM).setTrace},
	//
	{"", (*VM).unimplemented},
	{"", (*VM).unimplemented},
	{"", (*VM).unimplemented},
	{"", (*VM).unimplemented},
	{"", (*VM).unimplemented},
	{"", (*VM).unimplemented},
	{"", (*VM).unimplemented},
	{"", (*VM).unimplemented},
	// 0x50
	{"bye", (*VM).bye},
	{"eof", (*VM).eof},
}
