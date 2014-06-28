// Copyright 2011, 2013 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

package forth

import "io"

func (vm *VM) unimplemented() error {
	return IllegalInstruction
}

// nop ( -- )
func (vm *VM) nop() error {
	return nil
}

// exit ( -- ) ( R: nest-sys -- )
func (vm *VM) exit() error {
	var err error
	vm.pc, err = vm.rstack.pop()
	return rstackError(err)
}

// (abort) ( i*x -- )
func (vm *VM) abortHelper() error {
	vm.stack.clear()
	return nil
}

// (quit) ( R: i*x -- )
func (vm *VM) quitHelper() error {
	vm.rstack.clear()
	return nil
}

// pick ( xu ... x1 x0 u -- xu ... x1 x0 xu )
func (vm *VM) pick() error {
	c, err := vm.stack.pop()
	if err != nil {
		return err
	}
	return vm.stack.pick(1, int(c))
}

// roll ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
func (vm *VM) roll() error {
	c, err := vm.stack.pop()
	if err != nil {
		return err
	}
	return vm.stack.roll(1, int(c))
}

// depth ( -- +n )
func (vm *VM) depth() error {
	return vm.stack.push(vm.stack.depth())
}

// drop ( x -- )
func (vm *VM) drop() error {
	_, err := vm.stack.pop()
	return err
}

// 2drop ( x1 x2 -- )
func (vm *VM) twoDrop() error {
	_, _, err := vm.stack.pop2()
	return err
}

// ?dup ( x -- 0 | x x )
func (vm *VM) questionDup() error {
	c, err := vm.stack.peek()
	if c == 0 {
		return err
	}
	return vm.stack.push(c)
}

// nip ( x1 x2 -- x2 )
func (vm *VM) nip() error {
	if err := vm.stack.roll(1, 1); err != nil { // swap
		return err
	}
	vm.stack.pop() // drop
	return nil
}

// tuck ( x1 x2 -- x2 x1 x2 )
func (vm *VM) tuck() error {
	if err := vm.stack.need(2, 1); err != nil {
		return err
	}
	vm.stack.pick(1, 0) // dup
	vm.stack.roll(2, 1) // -rot
	return nil
}

// >r ( x -- ) ( R:  -- x )
func (vm *VM) toR() error {
	c, err := vm.stack.pop()
	if err != nil {
		return err
	}
	return rstackError(vm.rstack.push(c))
}

// r> ( -- x ) ( R:  x -- )
func (vm *VM) rFrom() error {
	c, err := vm.rstack.pop()
	if err != nil {
		return rstackError(err)
	}
	return vm.stack.push(c)
}

// r@ ( -- x ) ( R:  x -- x )
func (vm *VM) rFetch() error {
	c, err := vm.rstack.peek()
	if err != nil {
		return rstackError(err)
	}
	return vm.stack.push(c)
}

// rdrop ( R: x -- )
func (vm *VM) rDrop() error {
	_, err := vm.rstack.pop()
	return rstackError(err)
}

// @ ( a-addr -- x )
func (vm *VM) fetch() error {
	c, err := vm.stack.pop()
	if err != nil {
		return err
	}
	if c, err = vm.readCell(c); err != nil {
		return err
	}
	return vm.stack.push(c)
}

// ! ( x a-addr -- )
func (vm *VM) store() error {
	c, a, err := vm.stack.pop2()
	if err != nil {
		return err
	}
	return vm.writeCell(a, c)
}

// c@ ( c-addr -- char )
func (vm *VM) cFetch() error {
	c, err := vm.stack.pop()
	if err != nil {
		return err
	}
	if c, err = vm.readByte(c); err != nil {
		return err
	}
	return vm.stack.push(c)
}

// c! ( char c-addr -- )
func (vm *VM) cStore() error {
	c, a, err := vm.stack.pop2()
	if err != nil {
		return err
	}
	return vm.writeByte(a, c)
}

// 2@ ( a-addr -- x1 x2 )
func (vm *VM) twoFetch() error {
	a, err := vm.stack.pop()
	if err != nil {
		return err
	}
	c, err := vm.readCell(a + cellSize)
	if err != nil {
		return err
	}
	vm.stack.push(c) // will succeed
	if c, err = vm.readCell(a); err != nil {
		return err
	}
	return vm.stack.push(c)
}

// 2! ( x1 x2 a-addr -- )
func (vm *VM) twoStore() error {
	if err := vm.stack.need(3, 0); err != nil {
		return err
	}
	c, a, _ := vm.stack.pop2()
	if err := vm.writeCell(a, c); err != nil {
		return err
	}
	c, _ = vm.stack.pop()
	return vm.writeCell(a+cellSize, c)
}

// +! ( n|u a-addr -- )
func (vm *VM) plusStore() error {
	c, a, err := vm.stack.pop2()
	if err != nil {
		return err
	}
	v, err := vm.readCell(a)
	if err != nil {
		return err
	}
	return vm.writeCell(a, v+c)
}

func flag(b bool) Cell {
	if b {
		return forthTrue
	}
	return forthFalse
}

func (vm *VM) unaryOp(op func(c Cell) Cell) error {
	c, err := vm.stack.pop()
	if err != nil {
		return err
	}
	return vm.stack.push(op(c))
}

func (vm *VM) binaryOp(op func(x, y Cell) Cell) error {
	x, y, err := vm.stack.pop2()
	if err != nil {
		return err
	}
	return vm.stack.push(op(x, y))
}

// = ( x1 x2 -- flag )
func (vm *VM) equals() error {
	return vm.binaryOp(func(x, y Cell) Cell { return flag(x == y) })
}

// <> ( x1 x2 -- flag )
func (vm *VM) notEquals() error {
	return vm.binaryOp(func(x, y Cell) Cell { return flag(x != y) })
}

// < ( n1 n2 -- flag )
func (vm *VM) lessThan() error {
	return vm.binaryOp(func(x, y Cell) Cell {
		return flag(sCell(x) < sCell(y))
	})
}

// > ( n1 n2 -- flag )
func (vm *VM) greaterThan() error {
	return vm.binaryOp(func(x, y Cell) Cell {
		return flag(sCell(x) > sCell(y))
	})
}

// u< ( u1 u2 -- flag )
func (vm *VM) uLessThan() error {
	return vm.binaryOp(func(x, y Cell) Cell { return flag(x < y) })
}

// u> ( u1 u2 -- flag )
func (vm *VM) uGreaterThan() error {
	return vm.binaryOp(func(x, y Cell) Cell { return flag(x > y) })
}

// 0< ( n -- flag )
func (vm *VM) zeroLess() error {
	return vm.unaryOp(func(c Cell) Cell { return flag(sCell(c) < 0) })
}

// 0> ( n -- flag )
func (vm *VM) zeroGreater() error {
	return vm.unaryOp(func(c Cell) Cell { return flag(sCell(c) > 0) })
}

// 0= ( x -- flag )
func (vm *VM) zeroEquals() error {
	return vm.unaryOp(func(c Cell) Cell { return flag(c == forthFalse) })
}

// 0<> ( x -- flag )
func (vm *VM) zeroNotEquals() error {
	return vm.unaryOp(func(c Cell) Cell { return flag(c != forthFalse) })
}

// invert ( x1 -- x2 )
func (vm *VM) invert() error {
	return vm.unaryOp(func(c Cell) Cell { return ^c })
}

// and ( x1 x2 -- x3 )
func (vm *VM) and() error {
	return vm.binaryOp(func(x, y Cell) Cell { return x & y })
}

// or ( x1 x2 -- x3 )
func (vm *VM) or() error {
	return vm.binaryOp(func(x, y Cell) Cell { return x | y })
}

// xor ( x1 x2 -- x3 )
func (vm *VM) xor() error {
	return vm.binaryOp(func(x, y Cell) Cell { return x ^ y })
}

// lshift ( x1 u -- x2 )
func (vm *VM) lShift() error {
	return vm.binaryOp(func(x, y Cell) Cell { return x << y })
}

// rshift ( x1 u -- x2 )
func (vm *VM) rShift() error {
	return vm.binaryOp(func(x, y Cell) Cell { return x >> y })
}

// 2* ( x1 -- x2 )
func (vm *VM) twoStar() error {
	return vm.unaryOp(func(c Cell) Cell { return c << 1 })
}

// 2/ ( x1 -- x2 )
func (vm *VM) twoSlash() error {
	return vm.unaryOp(func(c Cell) Cell { return Cell(sCell(c) / 2) })
}

// 1+ ( n1|u1 -- n2|u2 )
func (vm *VM) onePlus() error {
	return vm.unaryOp(func(c Cell) Cell { return c + 1 })
}

// 1- ( n1|u1 -- n2|u2 )
func (vm *VM) oneMinus() error {
	return vm.unaryOp(func(c Cell) Cell { return c - 1 })
}

// + ( n1|u1 n2|u2 -- n3|u3 )
func (vm *VM) plus() error {
	return vm.binaryOp(func(x, y Cell) Cell { return x + y })
}

// - ( n1|u1 n2|u2 -- n3|u3 )
func (vm *VM) minus() error {
	return vm.binaryOp(func(x, y Cell) Cell { return x - y })
}

// * ( n1|u1 n2|u2 -- n3|u3 )
func (vm *VM) star() error {
	return vm.binaryOp(func(x, y Cell) Cell { return x * y })
}

// / ( n1 n2 -- n3 )
func (vm *VM) slash() error {
	x, y, err := vm.stack.pop2()
	switch {
	case err != nil:
		return err
	case y == 0:
		return ZeroDivision
	}
	return vm.stack.push(Cell(sCell(x) / sCell(y)))
}

// mod ( n1 n2 -- n3 )
func (vm *VM) mod() error {
	x, y, err := vm.stack.pop2()
	switch {
	case err != nil:
		return err
	case y == 0:
		return ZeroDivision
	}
	return vm.stack.push(Cell(sCell(x) % sCell(y)))
}

// /mod ( n1 n2 -- n3 n4 )
func (vm *VM) slashMod() error {
	x, y, err := vm.stack.pop2()
	switch {
	case err != nil:
		return err
	case y == 0:
		return ZeroDivision
	}
	vm.stack.push(Cell(sCell(x) % sCell(y))) // will succeed
	return vm.stack.push(Cell(sCell(x) / sCell(y)))
}

// */ ( n1 n2 n3 -- n4 )
func (vm *VM) starSlash() error {
	if err := vm.stack.need(3, 0); err != nil {
		return err
	}
	c, _ := vm.stack.pop()
	if c == 0 {
		return ZeroDivision
	}
	a, b, _ := vm.stack.pop2()
	return vm.stack.push(Cell(
		sdCell(sCell(a)) * sdCell(sCell(b)) / sdCell(sCell(c))))
}

// */mod ( n1 n2 n3 -- n4 n5 )
func (vm *VM) starSlashMod() error {
	if err := vm.stack.need(3, 0); err != nil {
		return err
	}
	c, _ := vm.stack.pop()
	if c == 0 {
		return ZeroDivision
	}
	a, b, _ := vm.stack.pop2()
	vm.stack.push(Cell( // will succeed
		sdCell(sCell(a)) * sdCell(sCell(b)) % sdCell(sCell(c))))
	return vm.stack.push(Cell(
		sdCell(sCell(a)) * sdCell(sCell(b)) / sdCell(sCell(c))))
}

// m* ( n1 n2 -- d )
func (vm *VM) mStar() error {
	a, b, err := vm.stack.pop2()
	if err != nil {
		return err
	}
	return vm.stack.pushd(dCell(sdCell(sCell(a)) * sdCell(sCell(b))))
}

// um* ( u1 u2 -- ud )
func (vm *VM) umStar() error {
	a, b, err := vm.stack.pop2()
	if err != nil {
		return err
	}
	return vm.stack.pushd(dCell(a) * dCell(b))
}

// fm/mod ( d1 n1 -- n2 n3 )
func (vm *VM) fmSlashMod() error {
	// (a - (a<0 ? b-1 : 0)) / b
	if err := vm.stack.need(3, 0); err != nil {
		return err
	}
	b, _ := vm.stack.pop()
	if b == 0 {
		return ZeroDivision
	}
	a, _ := vm.stack.popd()
	q := sdCell(a) / sdCell(sCell(b))
	if (sdCell(a)%sdCell(sCell(b)) != 0) &&
		((sdCell(a) < 0) != (sdCell(sCell(b)) < 0)) {
		q--
	}
	vm.stack.push(Cell(sdCell(a) - (sdCell(a) * q))) // will succeed
	return vm.stack.push(Cell(q))
}

// sm/rem ( d1 n1 -- n2 n3 )
func (vm *VM) smSlashRem() error {
	if err := vm.stack.need(3, 0); err != nil {
		return err
	}
	b, _ := vm.stack.pop()
	if b == 0 {
		return ZeroDivision
	}
	a, _ := vm.stack.popd()
	vm.stack.push(Cell(sdCell(a) % sdCell(sCell(b)))) // will succeed
	return vm.stack.push(Cell(sdCell(a) / sdCell(sCell(b))))
}

// um/mod ( ud n2 -- n3 n4 )
func (vm *VM) umSlashMod() error {
	if err := vm.stack.need(3, 0); err != nil {
		return err
	}
	b, _ := vm.stack.pop()
	if b == 0 {
		return ZeroDivision
	}
	a, _ := vm.stack.popd()
	vm.stack.push(Cell(a % dCell(b))) // will succeed
	return vm.stack.push(Cell(a / dCell(b)))
}

// negate ( n1 -- n2 )
func (vm *VM) negate() error {
	return vm.unaryOp(func(c Cell) Cell { return Cell(-sCell(c)) })
}

// ud+ ( ud1 ud2 -- ud3 )
func (vm *VM) udPlus() error {
	a, b, err := vm.stack.pop2d()
	if err != nil {
		return err
	}
	return vm.stack.pushd(a + b)
}

// ud- ( ud1 ud2 -- ud3 )
func (vm *VM) udMinus() error {
	a, b, err := vm.stack.pop2d()
	if err != nil {
		return err
	}
	return vm.stack.pushd(a - b)
}

// ud* ( ud1 ud2 -- ud3 )
func (vm *VM) udStar() error {
	a, b, err := vm.stack.pop2d()
	if err != nil {
		return err
	}
	return vm.stack.pushd(a * b)
}

// ud/mod ( ud1 ud2 -- ud3 ud4 )
func (vm *VM) udSlashMod() error {
	a, b, err := vm.stack.pop2d()
	switch {
	case err != nil:
		return err
	case b == 0:
		return ZeroDivision
	}
	vm.stack.pushd(a % b) // will succeed
	return vm.stack.pushd(a / b)
}

// key ( -- char )
func (vm *VM) key() error {
	switch b, err := vm.in.ReadByte(); err {
	case nil:
		return vm.stack.push(Cell(b))
	case io.EOF:
		return vm.stack.push(forthTrue)
	default:
		return vm.newIOError(err)
	}
}

// emit ( char -- )
func (vm *VM) emit() error {
	c, err := vm.stack.pop()
	if err != nil {
		return err
	}
	if _, err = vm.out.Write([]byte{byte(c)}); err != nil {
		return vm.newIOError(err)
	}
	return nil
}

// trace ( flag -- )
func (vm *VM) setTrace() error {
	c, err := vm.stack.pop()
	if err != nil {
		return err
	}
	vm.debug = c != 0
	return nil
}

// bye ( -- )
func (vm *VM) bye() error {
	return Bye
}

// eof ( -- )
func (vm *VM) eof() error {
	return EOF
}

var primitives = []struct {
	name string
	f    func(*VM) error
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
