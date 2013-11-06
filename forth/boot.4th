\ Copyright 2013 Vadim Vygonets. All rights reserved.
\ Use of this source code is governed by the Bugroff
\ license that can be found in the LICENSE file.

\ ***************************************************************
\ This file bootstraps a running Forth VM image.  It does so by
\ compiling words and microcode to the other half of memory (the
\ begining of the upper half if curretly running in the lower
\ half, and address 0 otherwise), the switching to the new
\ microcode and compilig more words.  To generate Go source file
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
: [[ 0 to state ; immediate
: ]] -1 to state ;
alias ;; ; immediate

: patch-abort
   target if 0 to here target jmp, then
;

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

0 primitive nop
1 primitive exit
2 primitive (abort)
3 primitive (quit)
4 primitive pick
5 primitive roll
6 primitive depth
7 primitive drop
8 primitive 2drop
9 primitive ?dup
a primitive nip
b primitive tuck
c primitive >r
d primitive r>
e primitive r@
f primitive rdrop

10 primitive @
11 primitive !
12 primitive c@
13 primitive c!
14 primitive 2!
15 primitive 2@
16 primitive +!

1a primitive =
1b primitive <>
1c primitive <
1d primitive >
1e primitive u<
1f primitive u>

20 primitive 0<
21 primitive 0>
22 primitive 0=
23 primitive 0<>
24 primitive invert
25 primitive and
26 primitive or
27 primitive xor
28 primitive lshift
29 primitive rshift
2a primitive 2*
2b primitive 2/
2c primitive 1+
2d primitive 1-
2e primitive +
2f primitive -

30 primitive *
31 primitive /
32 primitive mod
33 primitive /mod
34 primitive */
35 primitive */mod
36 primitive m*
37 primitive um*
38 primitive fm/mod
39 primitive sm/rem
3a primitive um/mod
3b primitive negate

40 primitive key
41 primitive emit
42 primitive (refill)

47 primitive trace

4b primitive builtin(trynum)

50 primitive bye
51 primitive eof

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
: <= > 0= ;
: >= < 0= ;

alias char+ 1+
:1; chars 0 ,	\ alias wouldn't work because nop doesn't compile,
: cell+ 4 + ;
: cells 2 lshift ;

: cr a emit ;
: type 0 ?do dup c@ emit char+ loop drop ;

\ ***************************************************************
\ execute

: decode-push
   dup 00ffffff and
   over 01000000 and if ff000000 or then
   swap 19 rshift 7 and lshift
;

: execute  ( i*x xt -- j*x )
   dup 1 and if 1- dup cell+ swap then
   @ ?dup if								\ nop
     dup 1 = if drop rdrop exit then					\ exit
     dup f0000000 and
     dup 10000000 = if xor >r exit then					\ call
     dup 20000000 = if xor rdrop >r exit then				\ jmp
     dup 30000000 = if xor swap if drop else rdrop >r then exit then	\ jz
     dup 40000000 = if drop decode-push exit then			\ push
     drop [ here 2 cells + ] literal ! [ 0 , ]		\ prim/pick/roll -> pad
   then
;

\ ***************************************************************
\ compiler basics

0 value state

: ] -1 to state ;
: [ 0 to state ; immediate

0 value here
variable (words)

variable base

: hex 10 base ! ;
: decimal a base ! ;
hex

0 value source-id

create (die-#tiben) 6 cells allot

: (source)
   (die-#tiben) source-id 1+ c * + ;

: source (source) 2@ ;
: >in (source) 8 + ;

: aligned  ( addr -- a-addr )
   3 + fffffffc and exit ;

: align  ( -- )
   here aligned to here ;

: ,  ( x -- )
   here tuck ! cell+ to here ;

: c,  ( x -- )
   here tuck c! char+ to here ;

: s,  ( addr u -- )
   0 ?do dup c@ c, 1+ loop drop ;

\ ***************************************************************
\ assembler

\ (0000) 0000  0000 0000  0000 0000  0iii iiii
\ primitive,  ( u -- )
alias primitive, ,

\ call, jmp, jz,  ( addr -- )
\ (00xx) aaaa  aaaa aaaa  aaaa aaaa  aaaa aaaa
: call, 10000000 or , ;
: jmp,  20000000 or , ;
: jz,   30000000 or , ;

\ (0100) sssN  nnnn nnnn  nnnn nnnn  nnnn nnnn
\ s...  = shift bits (u)
\ Nn... = number bits (n)
\ (N    = sign, extended left)
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
\ parser and compiler

: (scan)  ( char xt c-addr c-addr -- c-addr )
   >r >r
   begin 2r@ > while
     2dup r@ c@ swap execute while
     r> 1+ >r repeat then
   2drop r> rdrop
;

: parse  ( char "ccc<char>" -- c-addr u )
   source over + >r >in @ +
   over bl = if  over swap ['] >= swap r@ (scan) then
   swap 2dup bl = if  ['] < else  ['] <> then
   swap r> (scan)
   dup source >r -
   dup r> < if  1+ then  >in !
   over -
;

: parse-word  ( "<spaces>name" -- c-addr u )
   bl parse
;

: tolower  ( char -- char )
   dup [char] A >= if dup [char] Z <= if
   [[ char a char A - ]] literal + then then
;

: literal  ( x -- )
   8 begin ?dup while
     1-
     2dup 1 swap lshift 1- and while
   repeat
     2dup ff000000 swap lshift tuck and
     ?dup if
     over xor while then
       drop push, exit
     then 2drop
   then
   dup 0 push,  7 push,  [[ ' or @ ]] literal primitive,
; immediate

: compile,  \ Execution: ( xt -- )
   dup 1 and if dup aligned postpone literal 1- then
   @ ?dup if , then
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

: lineproc
   begin parse-word dup while
     (find) ?dup if
       1- if
       state if
	 compile,
       else [[ swap ]] then
         execute then
     else
       base @ builtin(trynum) if
         state if postpone literal then
       else (notfound) then
     then
   repeat
   2drop
;

: refill  ( -- flag )
   source-id if 0 exit then
   fe00 (refill) if
     (source) ! 0 fe00 (source) cell+ 2! -1 else
     0 then
;

: evaluate  ( i*x c-addr u -- j*x )
   -1 to source-id
   (source) 2! 0 >in !
   lineproc
   0 to source-id
;

: callthis, here cell+ call, ;

: :noname  ( -- xt colon-sys )
   here callthis, 0 ]
;

: (:def)  ( c-addr u -- colon-sys )
   here >r
   rot over or c, s, align
   (words) @ ,
   r>
;

: (:) parse-word (:def) ;
: (;) (words) ! ;

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
  state 0= if s" ok: " type then
repeat
eof

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
   parse-word (find) if exit then (notfound) ;

: postpone parse-word (find) 1- if
   [ ' literal compile, ' compile, ] literal then compile, ; immediate
\ what is this i don't even

: ['] ' postpone literal ; immediate
: char parse-word drop c@ ;
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
: skipname :>s + aligned ;
: >codeword skipname cell+ ;
: does> r> 10000000 or (words) @ >codeword ! ;

: constant
   value immediate
   does>  @  state if  postpone literal then
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
: to ' >body state if postpone literal postpone ! else ! then ; immediate
: allot here + aligned to here ;

: 2>r    postpone swap postpone >r postpone >r  ; immediate
: 2r>    postpone r> postpone r> postpone swap  ; immediate
: 2rdrop postpone rdrop postpone rdrop          ; immediate
here 2 0 rpick,
: 2r@    [ swap ] literal compile, postpone 2r> ; immediate

alias i r@
here 1 2 rpick,
: j [ swap ] literal compile, postpone r> ; immediate

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
   postpone ahead here swap
   2over s, align
   postpone then postpone literal postpone literal drop
; immediate

: s"  \ Compilation: ( "ccc<quote>" -- )  Run-time: ( -- c-addr u )
   [char] " parse postpone sliteral
; immediate

: ."  \ Compilation: ( "ccc<quote>" -- )  Run-time: ( -- )
   postpone s" postpone type
; immediate

: .(
   [char] ) parse type ; immediate

\ ***************************************************************
\ number output

: s>d  dup 0< if -1 else 0 then ;
alias d>s drop

variable (#pad)

44 allot
here constant (end#pad)

: abs  ( n -- u )
   dup 0< if negate then
;

: <#  (end#pad) (#pad) ! ;

: hold  ( char -- )
   (#pad) @ 1- tuck c! (#pad) !
;

: sign  ( n -- )
   0< if [char] - hold then
;

: #  ( ud1 -- ud2 )
   base @ um/mod swap
   dup a < if [char] 0 else [ char A a - ] literal then +
   hold s>d
;

: #s  ( ud1 -- ud2 )
   begin # over 0= until
;

: #>  ( xd -- c-addr u )
   2drop (#pad) @ (end#pad) over -
;

: (s.)  ( n -- )       dup abs         0 <# #s  rot sign    #> type ;
: (u.)  ( u -- )                       0 <# #s              #> type ;
: h.x   ( u -- )       base @ swap hex 0 <# # # # #s        #> type base ! ;
: .x    ( u -- )       base @ swap hex 0 <# # # # # # # # # #> type base ! ;
: u.x   ( u -- )       base @ swap hex      (u.)                    base ! ;
: .     ( n -- )       (s.) space ;
: u.    ( u -- )       (u.) space ;
: ?     ( a-addr -- )  @ . ;

: .s  depth 1- -1 swap ?do i pick . -1 +loop cr ;

: b.go  ( char -- )
   bl hold  [char] , hold  # #  [char] x hold  [char] 0 hold
;

: .go  ( x -- )
   base @ swap hex
   0 <# b.go b.go b.go b.go #> type
   base !
;

: holdp  ( char -- )
   dup 7f < if
   dup bl < while then
     drop [char] .
   then hold
;

: .p  ( x -- )
   0 <#  4 0 ?do
     drop dup ff and holdp  8 rshift 0
   loop  #> type
;

\ ***************************************************************
\ disassembler and dump

: dis-pickroll  ( x -- a-addr u )
   dup 0ffcc0c0 and if drop 0 0 exit then
   0 <#
   over          3f and 0 #s 2drop  bl hold
   over 8 rshift 3f and 0 #s 2drop  bl hold
   over 00020000 and if
     [char] l hold  [char] l hold  [char] o hold  [char] r hold  else
     [char] k hold  [char] c hold  [char] i hold  [char] p hold  then
   over 00010000 and if
     [char] r hold then
   bl hold  bl hold
   #>
;

: dis-primitive
   >r [ ' bye cell+ ] literal
   begin ?dup while
     :>s 2dup + aligned dup cell+ @ r@ <> while
     nip nip @
   repeat
     drop else
     0 0 then
   rdrop
;

: dis  ( x -- )
   dup
   dup      80 u< if drop ."   "      dis-primitive type else
   f0000000 and
   dup 10000000 = if  xor ."   call " h.x else
   dup 20000000 = if  xor ."   jmp  " h.x else
   dup 30000000 = if  xor ."   jz   " h.x else
   dup 40000000 = if drop ."   push " decode-push u.x else
   dup 50000000 = if  xor             dis-pickroll type else
   2drop
   then then then then then then
;

: dumpcell2go  ( a-addr -- a-addr )
   9 emit dup @ dup .go ." // " over h.x
   bl emit bl emit
   dup .p dis
   cr
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
     :>s 2dup type a over a mod - spaces + aligned repeat
;

: min 2dup > if swap then drop ;
: max 2dup < if swap then drop ;

decimal
