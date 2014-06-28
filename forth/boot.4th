\ Copyright 2013 Vadim Vygonets. All rights reserved.
\ Use of this source code is governed by the Bugroff
\ license that can be found in the LICENSE file.

\ ***************************************************************
\ This file bootstraps a running Forth VM image.  It does so by
\ compiling words and microcode to the other half of memory (the
\ begining of the upper half if curretly running in the lower
\ half, and address 0 otherwise), then switching to the new
\ microcode and compiling more words.  To generate Go source file
\ containing the kernel, evaluate this file twice, then say:

\	0 here dump2go bye

\ This can be done from stdin:

\	( cat forth/boot.4th forth/boot.4th ; \
\		echo cr 0 here dump2go ) | ./forego \
\		| grep -v ^ok: >forth/kern.go

\ or by using the EVALUATE word, which avoids grepping.  This is
\ what bootstrap does, so you can just run:

\	./bootstrap/bootstrap <forth/boot.4th >forth/kern.go

\ ***************************************************************
\ use the other half of the memory
hex
here fffe0000 and 20000 xor constant target

: blank-target target cell+ 1000 0 ?do 0 over ! cell+ loop drop ; blank-target

\ ***************************************************************
\ aliases and helpers

' here >body value there
alias (old-words) (words)
: [[ 0 state ! ; immediate
: ]] -1 state ! ;
alias ;; ; immediate

: patch-abort
   target if 0 to here target jmp, then
;

\ this is used instead of quit to handle running under evaluate
: do-quit  ( xt -- )
   (quit) [ target 10 + ] literal >r
   source-id if
     source >in @ rot over + -rot - rot @ 10000000 xor >r else
     drop then
;

\ ***************************************************************
\ start compiling to new space

target 80 + to here

\ ***************************************************************
\ Now we have to be really careful to compile new words so the
\ resulting bytecode contains only primitives and new words, but
\ to avoid runing new compiling words.
\ At this point postpone doesn't work on non-immediate words.

\ primitives

0 primitive nop		( -- )
1 primitive exit	( -- )  ( R: nest-sys -- )
2 primitive (abort)	( i*x -- )
3 primitive (quit)	( R: i*x -- )
4 primitive pick	( xu ... x1 x0 u -- xu ... x1 x0 xu )
5 primitive roll	( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
6 primitive depth	( -- +n )
7 primitive drop	( x -- )
8 primitive 2drop	( x1 x2 -- )
9 primitive ?dup	( x -- 0 | x x )
a primitive nip		( x1 x2 -- x2 )
b primitive tuck	( x1 x2 -- x2 x1 x2 )
c primitive >r		( x -- )  ( R: -- x )
d primitive r>		( -- x )  ( R: x -- )
e primitive r@		( -- x )  ( R: x -- x )
f primitive rdrop	( R: x -- )

10 primitive @		( a-addr -- x )
11 primitive !		( x a-addr -- )
12 primitive c@		( c-addr -- char )
13 primitive c!		( char c-addr -- )
14 primitive 2!		( x1 x2 a-addr -- )
15 primitive 2@		( a-addr -- x1 x2 )
16 primitive +!		( n|u a-addr -- )

1a primitive =		( x1 x2 -- flag )
1b primitive <>		( x1 x2 -- flag )
1c primitive <		( n1 n2 -- flag )
1d primitive >		( n1 n2 -- flag )
1e primitive u<		( u1 u2 -- flag )
1f primitive u>		( u1 u2 -- flag )

20 primitive 0<		( n -- flag )
21 primitive 0>		( n -- flag )
22 primitive 0=		( x -- flag )
23 primitive 0<>	( x -- flag )
24 primitive invert	( x1 -- x2 )
25 primitive and	( x1 x2 -- x3 )
26 primitive or		( x1 x2 -- x3 )
27 primitive xor	( x1 x2 -- x3 )
28 primitive lshift	( x1 u -- x2 )
29 primitive rshift	( x1 u -- x2 )
2a primitive 2*		( x1 -- x2 )
2b primitive 2/		( x1 -- x2 )
2c primitive 1+		( n1|u1 -- n2|u2 )
2d primitive 1-		( n1|u1 -- n2|u2 )
2e primitive +		( n1|u1 n2|u2 -- n3|u3 )
2f primitive -		( n1|u1 n2|u2 -- n3|u3 )

30 primitive *		( n1|u1 n2|u2 -- n3|u3 )
31 primitive /		( n1|u1 n2|u2 -- n3|u3 )
32 primitive mod	( n1 n2 -- n3 )
33 primitive /mod	( n1 n2 -- n3 n4 )
34 primitive */		( n1 n2 n3 -- n4 )
35 primitive */mod	( n1 n2 n3 -- n4 n5 )
36 primitive m*		( n1 n2 -- d )
37 primitive um*	( u1 u2 -- ud )
38 primitive fm/mod	( d1 n1 -- n2 n3 )
39 primitive sm/rem	( d1 n1 -- n2 n3 )
3a primitive um/mod	( ud n1 -- n2 n3 )
3b primitive negate	( n1 -- n2 )
3c primitive ud+	( ud1 ud2 -- ud3 )
3d primitive ud-	( ud1 ud2 -- ud3 )
3e primitive ud*	( ud1 ud2 -- ud3 )
3f primitive ud/mod	( ud1 ud2 -- ud3 ud4 )

40 primitive key	( -- char | -1 )
41 primitive emit	( char -- )

47 primitive trace	( flag -- )

50 primitive bye	( -- )
51 primitive eof	( -- )

\ one-instruction words
:1; abort target      jmp,
:1; quit  target 10 + jmp,

\ stack manipulation
:1; dup   1 0 pick,
:1; over  1 1 pick,
:1; swap  1 1 roll,
:1; rot   1 2 roll,
:1; -rot  2 1 roll,
:1; 2dup  2 0 pick,
:1; 2over 2 2 pick,
:1; 2swap 2 2 roll,
:1; 2rot  2 4 roll,
:1; 2-rot 4 2 roll,

\ basics
: <=  >  0= ;
: >=  <  0= ;
: u<= u> 0= ;
: u>= u< 0= ;

: within  ( test low high -- flag )  over - -rot - u> ;

alias char+ 1+
:1; chars 0 ,	\ alias wouldn't work because nop doesn't compile,
: cell+ 4 + ;
: cells 2 lshift ;

: cr a emit ;

: type  ( c-addr u -- )
   0 ?do
     dup c@ emit char+ loop
  drop
;

: cmove  ( c-addr1 c-addr2 u -- )
   0 ?do
     over c@ over c! char+ swap char+ swap loop
   2drop
;

: cmove>  ( c-addr1 c-addr2 u -- )
   dup >r + swap r@ + swap r>
   0 ?do
     swap 1- tuck c@ swap 1- tuck c! loop
   2drop
;

: move  ( addr1 addr2 u -- )
   0 2over u< nip if cmove> else cmove then ;

\ ***************************************************************
\ execute

: decode-push  ( x -- u )
   dup 00ffffff and
   over 01000000 and if ff000000 or then
   swap 19 rshift lshift
;

: execute  ( i*x xt -- j*x )
   dup 1 and if 1- dup cell+ swap then			\ variable
   @ ?dup if								\ nop
     dup        1 = if             drop      rdrop         exit then	\ exit
     dup f0000000 and
     dup 10000000 = if xor                         >r      exit then	\ call
     dup 20000000 = if xor                   rdrop >r      exit then	\ jmp
     dup 30000000 = if xor swap if drop else rdrop >r then exit then	\ jz
     dup 40000000 = if xor decode-push                     exit then	\ push
     drop [ here 2 cells + ] literal ! [ 0 , ]		\ prim/pick/roll -> pad
   then
;

\ ***************************************************************
\ compiler basics

variable state

: ] -1 state ! ;
: [ 0 state ! ; immediate

0 value here
variable (words)

variable base

: hex 10 base ! ;
: decimal a base ! ;
hex

0 value source-id

create (die-#tiben) 6 cells allot

: (source)  ( -- a-addr )
   (die-#tiben) source-id 1+ c * + ;

: source (source) 2@ ;
: >in (source) 8 + ;

: aligned  ( addr -- a-addr )
   3 + fffffffc and ;

: align  ( -- )
   here aligned to here ;

: ,  ( x -- )
   here tuck ! cell+ to here ;

: c,  ( char -- )
   here tuck c! char+ to here ;

: s,  ( addr u -- )
   tuck here swap cmove here + to here ;

\ ***************************************************************
\ assembler

\ (0000) 0000  0000 0000  0000 0000  0iii iiii
\ primitive,  ( u -- )
alias primitive, ,

\ (00xx) aaaa  aaaa aaaa  aaaa aaaa  aaaa aaaa
\ call, jmp, jz,  ( a-addr -- )
: call, 10000000 or , ;
: jmp,  20000000 or , ;
: jz,   30000000 or , ;

\ (0100) uuuN  nnnn nnnn  nnnn nnnn  nnnn nnnn
\ u...  = shift bits
\ Nn... = number bits
\ (N    = sign, extended left)
\ stores n>>u
: push,  ( n u -- )  \ Run-time: ( -- n )
   dup 19 lshift -rot rshift fe000000 invert and or 40000000 or ,
;

\ (0101) 0000  0000 00os  00ww wwww  00ff ffff
\ o = operation (0: pick, 1: roll)
\ s = stack (0: data stack, 1: rstack)
\ w = width
\ f = from
\ pick, roll, rpick, rroll,  ( width from -- )
: pick,  50000000 or swap 8 lshift or , ;
: rpick, 00010000 or pick, ;
: roll,  00020000 or pick, ;
: rroll, 00030000 or pick, ;

\ ***************************************************************
\ parser

: /string   ( c-addr1 u1 n -- c-addr2 u2 ) tuck - -rot + swap ;
: 1/string  ( c-addr1 u1 -- c-addr2 u2 )   1- swap char+ swap ;

: <%  ( -- c-addr u )
   source >in @ /string
;

: (wscan)  ( c-addr1 u1 xt -- c-addr2 u2 )
   >r begin dup while
     over c@ bl r@ execute while
       1/string
   repeat then
   rdrop
;

: %space  ( c-addr1 u1 -- c-addr2 u2 )  ['] <= (wscan) ;
: %word   ( c-addr1 u1 -- c-addr2 u2 )  ['] >  (wscan) ;

: %>char  ( c-addr1 u1 char -- c-addr2 u2 )
   >r begin dup while
     over c@
     dup a <> while
     dup r@ <> while
       drop 1/string
   repeat then
     drop
   then rdrop
;

: save>in  ( c-addr -- )
   source drop - >in !
;

: >%<  ( c-addr u -- c-addr u )
   over save>in
;

: scanned  ( c-addr1 -- c-addr2 u )
   source drop >in @ +
   tuck -
;

: %>  ( c-addr1 u1 -- c-addr2 u2 )
   over scanned
   2swap if 1+ then
   save>in
;

: parse-name  ( "<spaces>name<space>" -- c-addr u )
   <% %space >%< %word %>
;

: parse  ( char "ccc<char>" -- c-addr u )
   <% rot %>char %>
;

: tolower  ( char1 -- char2 )
   dup [char] A [[ char Z 1+ ]] literal within if
     [[ char a char A - ]] literal + then
;

: >digit  ( char -- u | -1 )
   dup [char] 0 [[ char 9 1+ ]] literal within if
     [char] 0 - else
   tolower dup [char] a [[ char z 1+ ]] literal within if
     [[ char a a - ]] literal - else
   drop -1 then then
;

: >number  ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
   begin dup while
     over c@ >digit
     dup base @ u< while
       -rot 2>r
       >r base @ us>d ud* r> us>d ud+
       2r> 1/string
   repeat
     drop
   then
;

: >num  ( c-addr u -- n|u -1 | 0 )
   over c@ [char] - = if
     1/string -1 else
     1 then
   -rot 0 0 2swap
   >number nip nip ?dup if
     2drop drop 0 else
     * -1 then
;

: weq  ( addr1 u1 addr2 u2 -- flag )
   2 pick <> if drop 2drop 0 exit then
   swap 0 ?do
     over c@ tolower over c@ tolower <> if unloop 2drop 0 exit then
     swap char+ swap char+ loop
   2drop -1
;

: (find)  ( addr u -- addr u 0 | xt 1 | xt -1 )
   2>r (words)
   begin @ ?dup while
     dup c@ swap 1+ over 1f and 2dup + aligned -rot 2r@ weq if
       cell+ swap 6 rshift tuck 1 and or
       swap 2 and 1- 2rdrop exit then
     nip repeat
   2r> 0
;

: (notfound)
   cr type s"  ?  " type abort ;

: accept  ( c-addr +n1 -- +n2 )
   over -rot
   begin dup while
     key
     dup a <> while
     dup 1+ while
       2 pick
       c!
       1/string
   repeat
     eof then
     drop then
   drop swap -
;

: set-source  ( a-addr u -- )
   (source) 2! 0 >in !
;

: refill  ( -- flag )
   source-id if 0 exit then
   3fe00 dup 200 accept set-source
   -1
;

\ ***************************************************************
\ compiler

\ word structure:
\	flags|namelen	byte
\	name		byte{1,31}
\	align?		byte{0,3}
\	prev		Cell
\	codeword	Cell
\	data?		Cell*
\	code?		Cell*
\ flags:
\	0x80		immediate
\	0x40		variable
\	0x20		unused/reserved
\	0x1f		len(name)
\ a :noname word doesn't have anything before codeword, and
\ normally doesn't have data either.
\ codeword can be 0 (nop), a call to code or anything else.
\ a word defined with does> has in its codeword a call pointing
\ elsewhere.

\ xt (execution token) is the address of the codeword,
\ "or"ed with 1 if the variable flag is set.

: literal  ( x -- )
   8 begin ?dup while
     1-
     2dup 1 swap lshift 1- and while
   repeat
     2dup ff000000 swap lshift tuck and
     ?dup if
     over = while then
       drop push, exit
     then 2drop
   then
   dup 0 push,  7 push,  [[ ' or @ ]] literal primitive,
; immediate

: compile,  \ Execution: ( xt -- )
   dup 1 and if dup aligned postpone literal 1- then
   @ ?dup if , then
;

: lineproc
   begin parse-name ?dup while
     (find) ?dup if
       1- if
       state @ 0= while then
         execute else
         compile, then
     else
       2dup >num if
         nip nip
         state @ if postpone literal then
       else (notfound) then
     then
   repeat
   drop
;

: evaluate  ( i*x c-addr u -- j*x )
   -1 to source-id
   set-source lineproc
   0 to source-id
;

: callthis, here cell+ call, ;

variable (:cw)

: :noname  ( -- xt colon-sys )
   here dup (:cw) ! callthis, 0 ]
;

: (:def)  ( u1 c-addr u2 -- colon-sys )
   here >r
   rot over or c, s, align
   (words) @ ,
   here (:cw) !
   r>
;

: (:)  ( u "<spaces>name" -- colon-sys )  parse-name (:def) ;
: (;)  ( colon-sys -- )                   (words) ! ;

: ;code  ( colon-sys -- )
   postpone [ ?dup if (;) then
; immediate

: ;  ( colon-sys -- )
   ['] exit compile, postpone ;code
; immediate

: :  ( "<spaces>name" -- colon-sys )
   0 (:) callthis, ]
;;

\ ***************************************************************
\ microcode

there @ to here
target there !

]]

\ abort (4 cells)
(abort) 0 to source-id

\ quit
begin (quit) refill while
  lineproc
  state @ 0= if s" ok: " type then
repeat
s"   evaluating?!  " type
abort

[[

patch-abort

\ ***************************************************************
\ set new dictionary head to the value of old head
(old-words) @ (words) !

\ unlink dictionaries.  we will have no comments beyond this
\ point.  after execute we will have self-containment.

' evaluate ' do-quit
0 ' nop 4 - !
execute

: immediate (words) @ dup c@ 80 or swap c! ;
: \ a parse 2drop ; immediate

\ ***************************************************************
\ we have self-containment!  and \ comments.
\ ( comments ) need [char] needs [postpone] needs ' needs if.

\ flow control and some helpers

: if here 0 jz, ; immediate
: then dup @ here or swap ! ; immediate

: '  \ ( "<spaces>name" -- xt )
   parse-name (find) if exit then (notfound) ;

: postpone parse-name (find) 1- if
   [ ' literal compile, ' compile, ] literal then compile, ; immediate
\ what is this i don't even

: ['] ' postpone literal ; immediate
: char parse-name drop c@ ;
: [char] char postpone literal ; immediate
: ( [char] ) parse 2drop ; immediate

\ ***************************************************************
\ more compiler

: :1; 0 (:) (;) ;
: primitive :1; primitive, ;
: alias 0 (:) ' compile, (;) ;
: (create) 40 (:) (;) ;
: create (create) 0 , ;
: variable create 0 , ;
: value (create) ['] @ compile, , ;

: count  dup char+ swap c@ ;
: :>s  count 1f and ;
: recurse (:cw) @ compile, ; immediate
: does> r> 10000000 or (:cw) @ ! ;

: constant
   value immediate
   does>  @  state @ if  postpone literal then
;

0    constant false
0 0= constant true
20   constant bl
4    constant cell

\ ***************************************************************
\ flow control

alias cs-pick pick
alias cs-roll roll

: begin here ; immediate
: again jmp, ; immediate
: until jz,  ; immediate
: ahead here 0 jmp, ; immediate

: else   postpone ahead swap postpone then ; immediate
: while  postpone if swap ; immediate
: repeat postpone again postpone then ; immediate

: >body fffffffc and cell+ ;
: to ' >body state @ if postpone literal postpone ! else ! then ; immediate
: allot here + aligned to here ;

: 2>r    postpone swap postpone >r postpone >r  ; immediate
: 2r>    postpone r> postpone r> postpone swap  ; immediate
: 2rdrop postpone rdrop postpone rdrop          ; immediate
: 2r@    2 0 rpick, postpone 2r>                ; immediate
alias i r@
: j      1 2 rpick, postpone r>                 ; immediate

: (do) postpone begin postpone 2>r ;
: do 0 (do) ; immediate
: ?do 0 postpone 2dup postpone <> postpone if (do) ; immediate
: unloop postpone 2rdrop ; immediate
: (loop)
   postpone 2dup postpone =
   postpone until
   begin ?dup while postpone then repeat
   postpone 2drop
;
: loop  postpone 2r> postpone 1+ (loop) ; immediate
: +loop postpone 2r> postpone rot postpone + (loop) ; immediate

\ fucking leave, how does it work?
: leave
   0 begin over while swap >r 1+ repeat
   postpone ahead swap
   begin ?dup while r> swap 1- repeat
; immediate

: case 0 ; immediate
: of 1+ >r postpone over postpone = postpone if postpone drop r> ; immediate
: endof >r postpone else r> ; immediate
: endcase postpone drop 0 ?do postpone then loop ; immediate

: fill -rot 0 ?do 2dup c! 1+ loop 2drop ;

\ ***************************************************************
\ string output

: space bl emit ;
: spaces 0 ?do space loop ;

: sliteral  \ Compilation: ( c-addr1 u -- )  Run-time: ( -- c-addr2 u )
   tuck here -rot 0 call,
   s, align
   postpone then ['] r> compile, postpone literal
; immediate

: s"  \ Compilation: ( "ccc<quote>" -- )  Run-time: ( -- c-addr u )
   [char] " parse postpone sliteral
; immediate

: ."  \ Compilation: ( "ccc<quote>" -- )  Run-time: ( -- )
   postpone s" postpone type
; immediate

: s(  ( "ccc<paren>" -- c-addr u )
   [char] ) parse
; immediate

: .(  ( "ccc<paren>" -- )
   postpone s( type
; immediate

\ ***************************************************************
\ number output

: min 2dup > if swap then drop ;
: max 2dup < if swap then drop ;
: abs dup 0< if negate then ;

: s>d  dup 0< if -1 else 0 then ;
0 constant us>d
alias d>s drop
alias ud>s drop

variable (#pad)

44 allot
here constant (end#pad)

: <#  ( -- )
   (end#pad) (#pad) !
;

: hold  ( char -- )
   (#pad) @ 1- tuck c! (#pad) !
;

: sign  ( n -- )
   0< if [char] - hold then
;

: #  ( ud1 -- ud2 )
   base @ us>d ud/mod 2swap ud>s
   dup a < if [char] 0 else [ char A a - ] literal then +
   hold
;

: #s  ( ud1 -- ud2 )
   begin # 2dup or 0= until
;

: #>  ( xd -- c-addr u )
   2drop (#pad) @ (end#pad) over -
;

: holds  ( c-addr u -- )
   tuck + swap 0 ?do 1- dup c@ hold loop drop
;

: typer  ( c-addr u n -- )
   over - 0 max spaces type
;

: <.>   ( n u1 -- c-addr u2 )          us>d <# #s  rot sign    #> ;
: (s.)  ( n -- )            dup abs    <.>                        type ;
: (u.)  ( u -- )            0   swap   <.>                        type ;
: .r    ( n1 n2 -- )   swap dup abs    <.>                    rot typer ;
: u.r   ( u n -- )          0   rot    <.>                    rot typer ;
: u.x   ( u -- )       base @ swap hex                            (u.) base ! ;
: h.x   ( u -- )       base @ swap hex us>d <# # # # #s        #> type base ! ;
: .x    ( u -- )       base @ swap hex us>d <# # # # # # # # # #> type base ! ;
: .     ( n -- )       (s.) space ;
: u.    ( u -- )       (u.) space ;
: ?     ( a-addr -- )  @ . ;

: .s  depth 1- -1 swap ?do i pick . -1 +loop cr ;

: #go  ( ud1 -- ud2 )
   s" , " holds # # s" 0x" holds
;

: .go  ( x -- )
   base @ swap hex
   us>d <# #go #go #go #go #> type
   base !
;

: holdp  ( char -- )
   dup 7f < if
   dup bl < while then
     drop [char] .
   then hold
;

: #p  ( ud1 -- ud2 )
   ud>s dup ff and holdp  8 rshift us>d
;

: .p  ( x -- )
   us>d <# #p #p #p #p #> type
;

\ ***************************************************************
\ disassembler and dump

: dis-pickroll  ( x -- c-addr u )
   dup 0ffcc0c0 and if drop 0 0 exit then
   base @ swap hex
   us>d <#
   over          3f and us>d #s 2drop  bl hold
   over 8 rshift 3f and us>d #s 2drop  bl hold
   over 00020000 and if
     s" roll" else
     s" pick" then
   holds
   over 00010000 and if
     [char] r hold then
   bl hold  bl hold
   #>
   rot base !
;

: dis-primitive  ( x -- c-addr u )
   >r [ ' bye cell+ ] literal
   begin ?dup while
     :>s 2dup + aligned dup cell+ @ r@ <> while
     nip nip @
   repeat
     drop <# 2dup holds bl hold bl hold #> else
     0 0 then
   rdrop
;

: dis  ( x -- )
   dup 80 u<      if                 dis-primitive type exit then
   dup f0000000 and
   dup 10000000 = if xor ."   call "               h.x  exit then
   dup 20000000 = if xor ."   jmp  "               h.x  exit then
   dup 30000000 = if xor ."   jz   "               h.x  exit then
   dup 40000000 = if xor ."   push " decode-push   u.x  exit then
   dup 50000000 = if xor             dis-pickroll  type exit then
   2drop
;

: dumpcell2go  ( a-addr -- a-addr )
   dup @
   9 emit  dup .go  ." // "  over h.x  bl emit bl emit  dup .p  dis  cr
;

: dumpcell  ( a-addr -- a-addr )
   dup .x ." : " dup @ .x cr
;

: (dump)  ( a-addr u xt -- )
   -rot
   2 rshift 0 ?do
     over execute
     cell+
   loop
   2drop
;

: dump  ( a-addr u -- )
   ['] dumpcell (dump)
;

: dump2go  ( a-addr u -- )
   ." // Autogenerated!  Shall not be edited." cr cr
   ." package forth" cr cr
   ." var kernel = []byte{" cr
   ['] dumpcell2go (dump)
   [char] } emit cr
;

: dumplast (words) @ here over - dump ;

: words  ( -- )
   (words) begin
     @ ?dup while
     :>s 2dup type a 2dup mod - spaces + aligned repeat
;

\ ***************************************************************
\ environmental queries

: (>env)
   parse-name dup c, s, align
   dup 1+ 0 do , loop
;

create (env)
               ff 1 (>env) /COUNTED-STRING
               44 1 (>env) /HOLD
                0 1 (>env) /PAD
               20 1 (>env) ADDRESS-UNIT-BITS
                0 1 (>env) FLOORED
               ff 1 (>env) MAX-CHAR
7fffffff ffffffff 2 (>env) MAX-D
         7fffffff 1 (>env) MAX-N
         ffffffff 1 (>env) MAX-U
ffffffff ffffffff 2 (>env) MAX-UD
               20 1 (>env) RETURN-STACK-CELLS
               20 1 (>env) STACK-CELLS
               -1 1 (>env) CORE
                  0 c, \ align

: environment?
   (env) begin count dup while
     2over 2over weq if
       2swap 2drop + aligned dup @ 0 ?do
         cell+ dup @ swap loop
       drop -1 exit then
     + aligned dup @ 1+ cells +
   repeat
   2drop 2drop 0
;

decimal
