// Copyright 2011 Vadim Vygonets. All rights reserved.
// Use of this source code is governed by the Bugroff
// license that can be found in the LICENSE file.

package forth

var softcore = `
1 2* 2* 2* 2* base !
here 013a0000 ,
(words) @ , (words) !
here 4 + 10000000 or ,
] here parse-word dup c, (s,) align
(words) @ , here 4 + 10000000 or , ] exit [
: ; [ ' exit literal ] compile, (words) ! [ ' [ compile, ]
exit [
dup c@ 80 or over c!
(words) !

: immediate (words) @ dup c@ 80 or swap c! ;

: \ a parse 2drop ; immediate

\ ok, here's a copy of the above:
\ : : here parse-word dup c, (s,) align (words) @ , here 4 + 10000000 or , ] ;
\ : ; postpone exit (words) ! postpone [ ; immediate

\ : literal (literal) ; immediate

\ Assembly
\ : primitive, , ;

\ call, jmp, jz, ( addr -- )
: call, 10000000 or , ;
: jmp,  20000000 or , ;
: jz,   30000000 or , ;

\ literal elsewhere

\ (0101) 0000  0000 00os  00ww wwww  00ff ffff
\ o = operation (0: pick, 1: roll)
\ s = stack (0: data stack, 1: rstack)
\ w = width
\ f = from

\ pick, ( width from -- )
: pick, 50000000 or swap 8 lshift or , ;
: roll, 00020000 or pick, ;
: rpick, 00010000 or pick, ;
: rroll, 00030000 or pick, ;

: if here 0 jz, ; immediate
: then dup @ f0000000 and here 0fffffff and or swap ! ; immediate

: postpone parse-word (find) 1- if
  [ ' literal compile, ' compile, ] literal then compile, ; immediate
\ what is this i don't even

: begin here ; immediate
: again jmp, ; immediate
: until jz, ; immediate
: ahead here 0 jmp, ; immediate

: else postpone ahead swap postpone then ; immediate
: while postpone if swap ; immediate
: repeat postpone again postpone then ; immediate

: 2>r postpone swap postpone >r postpone >r ; immediate
: 2r> postpone r> postpone r> postpone swap ; immediate
: 2r@ postpone r> postpone r@ postpone swap postpone dup postpone >r ; immediate
: 2rdrop postpone rdrop postpone rdrop ; immediate

: cr a emit ;
: stack depth begin ?dup while dup pick . 1- repeat cr ;

: i postpone r@ ; immediate
: (do) postpone begin postpone 2>r ;
: do 0 (do) ; immediate
: ?do 0 postpone 2dup postpone xor postpone if (do) ; immediate
: unloop postpone 2rdrop ; immediate
: loop postpone 2r> postpone 1+ postpone 2dup postpone =
  postpone until
  begin ?dup while postpone then repeat
  postpone 2drop
; immediate

\ fucking leave, how does it normally work?
: leave
   0 begin over while swap >r 1+ repeat
   postpone ahead swap
   begin ?dup while r> swap 1- repeat
; immediate

\ : foo 10 0 ?do i . i 5 = if leave then loop ;

: ['] ' postpone literal ; immediate

: char parse-word drop c@ ;
: [char] char postpone literal ; immediate
: ( [char] ) parse 2drop ; immediate

: cell+ 4 + ;
: cells 2 lshift ;
: >body fffffffc and cell+ ;  \ 1+ aligned

: decode dup 00ffffff and
  over 01000000 and if fe000000 or then
  swap 19 ( that's hex, btw) rshift 7 and lshift ;

: hex 10 base ! ;
: decimal a base ! ;

: bl 20 ;

: <= > 0= ;
: >= < 0= ;

: dumplast (words) @ here over - dump ;

\ Now we have loops and some basics.  time to define a more-real compiler.

\ : state d0 @ ; \ TODO
: to ' >body state if postpone literal postpone ! else ! then ; immediate
: allot here + aligned to here ;

\ : source-id ec @ ;
\ : source e0 source-id c * + 2@ ;
\ : >in e8 source-id c * + ;

: (scan) ( char xt c-addr c-addr -- c-addr )
   >r >r
   begin 2r@ > while
     2dup r@ c@ swap execute while
     r> 1+ >r repeat then
   2drop r> rdrop
;

: parse ( char "ccc<char>" -- c-addr u )
   source over + >r >in @ +
   over bl = if over swap ['] >= swap r@ (scan) then
   swap 2dup bl = if ['] < else ['] <> then
   swap r> (scan)
   dup source >r -
   dup r> < if 1+ then >in !
   over -
;

: parse-word ( "<spaces>name" -- c-addr u ) bl parse ;

: (:) >r here parse-word dup r> or c, (s,) align (words) @ , ;
: (;) (words) ! ;
: (create) 40 (:) (;) ;
: create (create) 0 , ;
: variable create 0 , ;
: value (create) ['] @ compile, , ;
: : 0 (:) here cell+ call, ] ;
: ; postpone exit (;) postpone [ ; immediate

: skipname dup c@ 1f and + 1+ aligned ;

: nextwordaddr dup c@ 1f and + 1+ aligned @ ;

: does> r> 10000000 or (words) @ skipname cell+ ! ;

: constant value immediate
   does> @ state if postpone literal then ;

: primitive 0 (:) , (;) ;
: alias 0 (:) ' compile, (;) ;

alias char+ 1+

: tolower ( char -- char )
   dup [char] A >= if dup [char] Z <= if
   [ char a char A - ] literal + then then ;

: weq ( addr1 u1 addr2 u2 -- flag )
   2 pick <> if drop 2drop 0 exit then
   swap 0 ?do
     over c@ tolower over c@ tolower <> if unloop 2drop 0 exit then
     swap char+ swap char+ loop
   2drop -1 ;

: (find) ( addr u -- addr u 0 | xt 1 | xt -1 )
   2>r (words) begin @ ?dup while
     dup c@ swap 1+ over 1f and 2dup + aligned -rot 2r@ weq if
       cell+ swap 6 rshift tuck 1 and or
       swap 2 and 1- 2rdrop exit then
     nip repeat
   2r> 0
;

decimal
`
