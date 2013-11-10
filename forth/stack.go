// Copyright 2011, 2013 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

package forth

const stackDepth = 32

type vmStack []Cell

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
	if len(*s)+up > cap(*s) {
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
	*s, c = (*s)[0:len(*s)-1], (*s)[len(*s)-1]
	return c
}

func (s *vmStack) pushd(d DCell) {
	s.push(Cell(d))
	s.push(Cell(d) >> cellBits)
}

func (s *vmStack) popd() DCell {
	d := DCell(s.pop()) << cellBits
	return d | DCell(s.pop())
}

func (s *vmStack) peek() Cell {
	s.need(1, 0)
	return (*s)[len(*s)-1]
}

// mnemonic for arguments: pick/roll <size> cells from depth <from>
func (s *vmStack) pick(size, from int) {
	s.need(from+size, size)
	*s = append(*s, (*s)[len(*s)-from-size:len(*s)-from]...)
}

func (s *vmStack) roll(size, from int) {
	s.need(from+size, 0)
	var buf [stackDepth]Cell
	l := len(*s)
	copy(buf[:], (*s)[l-from-size:l-from])
	copy((*s)[l-from-size:l-size], (*s)[l-from:])
	copy((*s)[l-size:], buf[:size])
}
