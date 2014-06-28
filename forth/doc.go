// Copyright 2011, 2013 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

// Package forth implements the Forego virtual machine.
//
// The Forego VM is a big-endian FORTH machine with 32-bit cells
// having two 32-cell stacks and 256 KB of memory.
//
// Each VM instruction occupies a 32-bit cell.  The most
// significant 4 bits contain the opcode, which leaves 28 bits
// for parameters.  The default kernel makes instructions
// accessible using compiling words.  Forego has six
// instructions, one of which is further subdivided into
// "primitives", described further below.
//
//  0: primitive
//	FORTH compiling word: PRIMITIVE,
//	FORTH defining word: PRIMITIVE
//	Bit field: (0000) 0000  0000 0000  0000 0000  0ppp pppp
//	p...  = primitive
//	Run primitive p from the list below.
//
//  1: call
//	FORTH compiling word: CALL,
//	Bit field: (0001) aaaa  aaaa aaaa  aaaa aaaa  aaaa aaaa
//	a...  = address
//	Call the address a, pushing the address of the next cell
//	to rstack.
//
//  2: jmp
//	FORTH compiling word: JMP,
//	Bit field: (0010) aaaa  aaaa aaaa  aaaa aaaa  aaaa aaaa
//	a...  = address
//	Jump to the address a.
//
//  3: jz
//	FORTH compiling word: JZ,
//	Bit field: (0011) aaaa  aaaa aaaa  aaaa aaaa  aaaa aaaa
//	a...  = address
//	Pop a cell off the stack.  If the cell value is zero,
//	jump to the address a.
//
//  4: push
//	FORTH compiling word: PUSH,
//	also accessible via: LITERAL
//	Bit field: (0100) sssN  nnnn nnnn  nnnn nnnn  nnnn nnnn
//	s...  = shift
//	Nn... = number
//	(N    = sign, extended left)
//	Push n, shifted left s bits, to the stack, propagating
//	the sign bit N to the left.
//
//  5: pick/roll
//	FORTH compiling words: PICK, ROLL, RPICK, RROLL,
//	Bit field: (0101) 0000  0000 00os  00ww wwww  00ff ffff
//	o     = operation (0: pick, 1: roll)
//	s     = stack (0: data stack, 1: rstack)
//	w...  = width
//	f...  = from
//	Pick or roll w cells from depth f in stack or rstack.
//
// Primitives are simple instructions operating on the stacks
// or providing low-level services, intended to be called
// directly from FORTH code.  The default kernel exports the
// primitives as FORTH words.  Most instructions operate as
// described in DPANS, others are commented.
//
//	Opcode	FORTH name
//
//	0	NOP      ( -- )				\ do nothing
//	1	EXIT                 ( R: nest-sys -- )
//	2	(ABORT)  ( i*x -- )			\ clear stack
//	3	(QUIT)               ( R: i*x -- )	\ clear rstack
//	4	PICK     ( xu ... x0 u -- xu ... x0 xu )
//	5	ROLL     ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
//	6	DEPTH    ( -- +n )
//	7	DROP     ( x -- )
//	8	2DROP    ( x1 x2 -- )
//	9	?DUP     ( x -- 0 | x x )
//	A	NIP      ( x1 x2 -- x2 )
//	B	TUCK     ( x1 x2 -- x2 x1 x2 )
//	C	>R       ( x -- )    ( R: -- x )
//	D	R>       ( -- x )    ( R: x -- )
//	E	R@       ( -- x )    ( R: x -- x )
//	F	RDROP                ( R: x -- )
//
//	10	@        ( a-addr -- x )
//	11	!        ( x a-addr -- )
//	12	C@       ( c-addr -- char )
//	13	C!       ( char c-addr -- )
//	14	2!       ( x1 x2 a-addr -- )
//	15	2@       ( a-addr -- x1 x2 )
//	16	+!       ( n|u a-addr -- )
//
//	1A	=        ( x1 x2 -- flag )
//	1B	<>       ( x1 x2 -- flag )
//	1C	<        ( n1 n2 -- flag )
//	1D	>        ( n1 n2 -- flag )
//	1E	U<       ( u1 u2 -- flag )
//	1F	U>       ( u1 u2 -- flag )
//
//	20	0<       ( n -- flag )
//	21	0>       ( n -- flag )
//	22	0=       ( x -- flag )
//	23	0<>      ( x -- flag )
//	24	INVERT   ( x1 -- x2 )
//	25	AND      ( x1 x2 -- x3 )
//	26	OR       ( x1 x2 -- x3 )
//	27	XOR      ( x1 x2 -- x3 )
//	28	LSHIFT   ( x1 u -- x2 )
//	29	RSHIFT   ( x1 u -- x2 )
//	2A	2*       ( x1 -- x2 )
//	2B	2/       ( x1 -- x2 )
//	2C	1+       ( n1|u1 -- n2|u2 )
//	2D	1-       ( n1|u1 -- n2|u2 )
//	2E	+        ( n1|u1 n2|u2 -- n3|u3 )
//	2F	-        ( n1|u1 n2|u2 -- n3|u3 )
//
//	30	*        ( n1|u1 n2|u2 -- n3|u3 )
//	31	/        ( n1|u1 n2|u2 -- n3|u3 )
//	32	MOD      ( n1 n2 -- n3 )
//	33	/MOD     ( n1 n2 -- n3 n4 )
//	34	*/       ( n1 n2 n3 -- n4 )
//	35	*/MOD    ( n1 n2 n3 -- n4 n5 )
//	36	M*       ( n1 n2 -- d )
//	37	UM*      ( u1 u2 -- ud )
//	38	FM/MOD   ( d1 n1 -- n2 n3 )
//	39	SM/REM   ( d1 n1 -- n2 n3 )
//	3A	UM/MOD   ( d1 n1 -- n2 n3 )
//	3B	NEGATE   ( n1 -- n2 )
//	3C	UD+      ( ud1 ud1 -- ud3 )		\ unsigned double cell
//	3D	UD-      ( ud1 ud1 -- ud3 )		\ arithmetics
//	3E	UD*      ( ud1 ud1 -- ud3 )		\
//	3F	UD/MOD   ( ud1 ud1 -- ud3 ud4 )		\
//
//	40	KEY      ( -- char | -1 )
//	41	EMIT     ( char -- )
//
//	47	TRACE    ( flag -- )			\ set/reset VM tracing
//
//	50	BYE      ( -- )
//	51	EOF      ( -- )				\ raise EOF trap
package forth
