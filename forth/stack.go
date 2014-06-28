// Copyright 2011, 2013 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

package forth

const stackDepth = 32

// Stack represents a virtual machine stack, growing up.
type Stack []Cell

// String formats s as a space-separated series of Cells.
func (s Stack) String() string {
	if len(s) == 0 {
		return ""
	}
	var ret = s[0].String()
	for _, v := range s[1:] {
		ret += " " + v.String()
	}
	return ret
}

func (s *Stack) depth() Cell {
	return Cell(len(*s))
}

func (s *Stack) clear() {
	*s = (*s)[:0]
}

func (s *Stack) need(down, up int) error {
	switch {
	case len(*s) < down:
		return StackUnderflow
	case len(*s)+up > cap(*s):
		return StackOverflow
	default:
		return nil
	}
}

func (s *Stack) push(c Cell) error {
	if err := s.need(0, 1); err != nil {
		return err
	}
	*s = append(*s, c)
	return nil
}

func (s *Stack) pop() (Cell, error) {
	if err := s.need(1, 0); err != nil {
		return 0, err
	}
	var c Cell
	*s, c = (*s)[:len(*s)-1], (*s)[len(*s)-1]
	return c, nil
}

func (s *Stack) pushd(d dCell) error {
	if err := s.push(Cell(d)); err != nil {
		return err
	}
	return s.push(Cell(d) >> cellBits)
}

func (s *Stack) pop2() (Cell, Cell, error) {
	if err := s.need(2, 0); err != nil {
		return 0, 0, err
	}
	var x, y Cell
	*s, x, y = (*s)[:len(*s)-2], (*s)[len(*s)-2], (*s)[len(*s)-1]
	return x, y, nil
}

func (s *Stack) popd() (dCell, error) {
	x, y, err := s.pop2()
	return dCell(y)<<cellSize | dCell(x), err
}

func (s *Stack) pop2d() (dCell, dCell, error) {
	if err := s.need(4, 0); err != nil {
		return 0, 0, err
	}
	x := dCell((*s)[len(*s)-4]) | dCell((*s)[len(*s)-3]<<cellSize)
	y := dCell((*s)[len(*s)-2]) | dCell((*s)[len(*s)-1]<<cellSize)
	*s = (*s)[:len(*s)-4]
	return x, y, nil
}

func (s *Stack) peek() (Cell, error) {
	if err := s.need(1, 0); err != nil {
		return 0, err
	}
	return (*s)[len(*s)-1], nil
}

// mnemonic for arguments: pick/roll <size> cells from depth <from>
func (s *Stack) pick(size, from int) error {
	if err := s.need(from+size, size); err != nil {
		return err
	}
	*s = append(*s, (*s)[len(*s)-from-size:len(*s)-from]...)
	return nil
}

func (s *Stack) roll(size, from int) error {
	if err := s.need(from+size, 0); err != nil {
		return err
	}
	var buf [stackDepth]Cell
	l := len(*s)
	copy(buf[:], (*s)[l-from-size:l-from])
	copy((*s)[l-from-size:l-size], (*s)[l-from:])
	copy((*s)[l-size:], buf[:size])
	return nil
}
