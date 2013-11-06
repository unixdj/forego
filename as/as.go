// Copyright 2013 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

const (
	cellSize = 4
	cellBits = 32
)

type (
	Cell uint32

	dict map[string]Cell

	instr struct {
		c Cell
		s string
	}

	unres struct {
		a Cell   // address
		s string // unresolved symbol
	}

	parser struct {
		l int     // line number
		s string  // source line
		a Cell    // address
		d dict    // dictionary of labels
		u []unres // unresolved
		i []instr // compiled instructions
	}
)

var (
	parseError       = errors.New("parse error")
	labelExistsError = errors.New("label already exists")

	primitives = []string{
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
		"",
		// 0x18
		"",
		"",
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
		"negate",
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
		"builtin-type",
		"builtin-execute",
		"builtin(find)",
		"builtin(trynum)",
		"",
		"",
		"builtin-dump",
		"",
		// 0x50
		"bye",
		"eof",
	}

	revPrimitives = func() map[string]Cell {
		rp := make(map[string]Cell)
		for k, v := range primitives {
			if v != "" {
				rp[v] = Cell(k)
			}
		}
		return rp
	}()
)

func (p *parser) defLabel(lbl string) error {
	if _, ok := p.d[lbl]; ok {
		return labelExistsError
	}
	p.d[lbl] = p.a
	return nil
}

func (p *parser) parseNum(num string) (Cell, error) {
	if num == "." {
		return p.a + cellSize, nil
	}
	if c, ok := p.d[num]; ok {
		return c, nil
	}
	n, err := strconv.ParseUint(num, 0x10, 32)
	return Cell(n), err
}

func (p *parser) store(c Cell) {
	p.i = append(p.i, instr{c, p.s})
	p.a += cellSize
	p.s = "\\\n" // for tri-instruction PUSH etc
}

func (p *parser) storeUnresolved(c Cell, s string) {
	p.u = append(p.u,
		unres{
			a: p.a,
			s: s,
		})
	p.store(c)
}

func (p *parser) resolve() []string {
	var syms []string
	for _, v := range p.u {
		if c, ok := p.d[v.s]; ok {
			p.i[v.a>>2].c |= c
		} else {
			syms = append(syms, v.s)
		}
	}
	return syms
}

const (
	instrShift     = 28
	instrParamMask = (1 << instrShift) - 1
	instrMask      = ^Cell(instrParamMask)
	litSignBit     = 24
	litNumMask     = 1<<(litSignBit+1) - 1
	litSameBits    = ^Cell(1<<litSignBit - 1)
	litShiftMask   = instrParamMask &^ litNumMask
	litShiftShift  = litSignBit + 1
	litMaxShift    = 1<<(instrShift-litShiftShift) - 1
)

func (p *parser) bytes(f []string) error {
	var b = make([]byte, 0, 16)
	for _, s := range f {
		switch s[0] {
		case '\'':
			for _, c := range s[1:] {
				if c == '\'' {
					break
				}
				b = append(b, byte(c))
			}
		default:
			n, err := strconv.ParseUint(s, 0x10, 8)
			if err != nil {
				return err
			}
			b = append(b, byte(n))
		}
	}
	return p.storeBytes(b)
}

func (p *parser) storeBytes(b []byte) error {
	for len(b) > 0 {
		var c Cell
		for k, v := range b {
			if k == cellSize {
				break
			}
			c |= Cell(v) << ((cellBits - 8) - uint(k)<<3)
		}
		p.store(c)
		if len(b) <= cellSize {
			break
		}
		b = b[cellSize:]
	}
	return nil
}

func (p *parser) cell(num string) error {
	n, err := p.parseNum(num)
	if err != nil {
		p.storeUnresolved(0, num)
		return nil
	}
	p.store(n)
	return nil
}

func (p *parser) literal(num string) error {
	n, err := p.parseNum(num)
	if err != nil {
		p.storeUnresolved(0x40000000, num)
		return nil
	}

	// The rest from vm.go
	for s := litMaxShift; s >= 0; s-- {
		if n&(1<<uint(s)-1) == 0 {
			bits := litSameBits << uint(s)
			switch n & bits {
			case 0, bits:
				p.store(0x40000000 |
					Cell(s)<<litShiftShift |
					n>>uint(s)&litNumMask)
				return nil
			}
			break
		}
	}
	// can't store number in one instruction, do the "or"
	p.store(0x40000000 | n&(litNumMask>>1))
	p.store(0x40000000 | litMaxShift<<litShiftShift | n>>litMaxShift)
	p.store(0x00000026)
	return nil
}

func (p *parser) pickRoll(f []string) error {
	if len(f) != 3 {
		return parseError
	}
	var i Cell = 0x50000000
	switch f[0] {
	//case "PICK":
	case "RPICK":
		i |= 0x10000
	case "ROLL":
		i |= 0x20000
	case "RROLL":
		i |= 0x30000
	}

	switch j, err := p.parseNum(f[1]); {
	case err != nil:
		return err
	case j >= 0x40:
		return parseError
	default:
		i |= j << 8
	}

	switch j, err := p.parseNum(f[2]); {
	case err != nil:
		return err
	case j >= 0x40:
		return parseError
	default:
		i |= j
	}
	p.store(i)
	return nil
}

func (p *parser) jmp(f []string) error {
	var i Cell
	switch f[0] {
	case "call":
		i = 0x10000000
	case "jmp":
		i = 0x20000000
	case "jz":
		i = 0x30000000
	}

	n, err := p.parseNum(f[1])
	if err != nil {
		p.storeUnresolved(i, f[1])
		return nil
	}
	p.store(i | n)
	return nil
}

func (p *parser) primitive(s string) error {
	if c, ok := revPrimitives[s]; ok {
		p.store(c)
		return nil
	}
	return parseError
}

func (p *parser) genPrimitives(prevLabel string) error {
	var b [32]byte
	if prevLabel == "" {
		prevLabel = "0"
	}
	lastAddr, err := p.parseNum(prevLabel)
	if err != nil {
		return err
	}
	for i, v := range primitives {
		if v == "" {
			continue
		}
		thisAddr := p.a
		b[0] = byte(len(v))
		copy(b[1:], v)
		p.s = ".B " + strconv.FormatInt(int64(len(v)), 16) +
			" '" + v + "'\n"
		p.storeBytes(b[:len(v)+1])
		p.s = "^\n"
		p.store(lastAddr)
		p.s = "\t" + v + "\n"
		p.store(Cell(i))
		lastAddr = thisAddr
	}
	p.d["w/(last-primitive)"] = lastAddr
	return nil
}

func (p *parser) doLine() error {
	f := strings.Fields(p.s)
	if len(f) == 0 {
		return nil
	}
	switch f[0] {
	case `\`:
		return nil
	case ".B":
		if len(f) < 2 {
			return parseError
		}
		return p.bytes(f[1:])
	case ".C":
		if len(f) != 2 {
			return parseError
		}
		return p.cell(f[1])
	case ".L": // label
		if len(f) != 2 {
			return parseError
		}
		return p.defLabel(f[1])
	case ".PRIMITIVES":
		if len(f) != 2 {
			return parseError
		}
		return p.genPrimitives(f[1])
	case "PUSH":
		if len(f) != 2 {
			return parseError
		}
		return p.literal(f[1])
	case "PICK", "ROLL", "RPICK", "RROLL":
		return p.pickRoll(f)
	case "call", "jmp", "jz":
		if len(f) != 2 {
			return parseError
		}
		return p.jmp(f)
	default:
		if len(f) != 1 {
			return parseError
		}
		return p.primitive(f[0])
	}
	return parseError
}

func (p *parser) dump() {
	fmt.Print(`// Autogenerated!  Shall not be edited.

package forth

var kernel = []byte{
`)
	for k, v := range p.i {
		fmt.Printf("\t%#02x, %#02x, %#02x, %#02x, // %04x %s",
			(v.c>>24)&0xff, (v.c>>16)&0xff,
			(v.c>>8)&0xff, v.c&0xff, k*cellSize, v.s)
	}
	fmt.Print(`}
`)
}

func main() {
	var p = &parser{}
	p.d = make(dict)
	in := bufio.NewReader(os.Stdin)
loop:
	for {
		var err error
		p.l++
		switch p.s, err = in.ReadString('\n'); err {
		case nil:
		case io.EOF:
			break loop
		default:
			log.Fatalln(p.l, err)
		}
		if err = p.doLine(); err != nil {
			log.Fatalln(p.l, err)
		}
	}
	if syms := p.resolve(); len(syms) != 0 {
		log.Fatalf("unresolved symbols: %s\n", syms)
	}
	p.dump()
}
