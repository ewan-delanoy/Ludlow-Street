(*

#use "/Users/ewandelanoy/Documents/OCaml/Ordinary/my_pervasives.ml";; 


*)



(* Directory setting starts here *)

Sys.chdir "/Users/ewandelanoy/Documents/OCaml/Ordinary/";;

(* Directory setting ends here *)


open Image;;
open Listennou;;
open Ennig;;
open Basics_on_small_primes;;


let hd=List.hd;;
let tl=List.tl;;
let rev=List.rev;;

let random_string m=
   let base=(Ennig.ennig 65 90)@(Ennig.ennig  97 122) in
   let temp1=Ennig.doyle(fun _->
   char_of_int(List.nth base (Random.int(List.length base)))) 1 m in
   Strung.implode temp1;;


let ps=Memoized.make(function n->
  let temp1=power_set(ennig 1 n) in
  let temp2=image(Tidel.safe_set)(temp1) in
Ordered_bare_set.diforchan(temp2));;


let isqrt n=int_of_float(floor(sqrt(float_of_int(n))));;

let binom n p=
  let temp1=Rational.big_product(doyle(Rational.of_int)(n-p+1)(n))
  and temp2=Rational.big_product(doyle(Rational.of_int)(2)(p)) in
  let temp3=Rational.div temp1 temp2 in
  Num.int_of_num(Rational.to_num(temp3));;
 
let isqrt y=int_of_float(floor(sqrt(float_of_int(y))));;
  
let square_free_part=Memoized.recursive(fun old_f n->
  match Basics_on_small_primes.multiset_factorization(n) with
  Multiset.M(l)->
    let temp0=List.filter(fun (p,ep)->(ep mod 2)=1)(l) in
    let temp1=List.rev_map(fun (p,ep)->p)(temp0) in
    List.fold_left ( * ) 1 temp1
 );;
  
 let round_float x n=
   let g=(10.)**(float_of_int n) in
   (floor(g*.x))/.g;;


let real_quotient aa bb=
   let (a,b)=(fun ()->
      if bb>0 then (aa,bb) else (-aa,-bb))() in
    if (a>0)||((a mod b)=0) then (a/b) else 
    let m=((-a)/b)+1 in -m;;

let real_modulo aa bb=aa-(bb*(real_quotient aa bb));;

let sfp=square_free_part;;
  
let is_a_square y=
 let x=isqrt(y) in
 x*x=y;;
  
let typical_string ()=
 let temp_l=doyle(string_of_int)(1)(200) in
 let temp_s="\n\n\n"^(String.concat("\n")(temp_l))^"\n\n\n" in
 print_string temp_s;;



let hi=List.length;;
let ol=Ordered.length;;
let rev=List.rev;;

let oup=Option.unpack;;
let ofo=Ordered.forget_order;;

let pfst= Positioned_php_token.fst;;
let psnd= Positioned_php_token.snd;;

let ps s=print_string("\n\n\n\n\n\n"^s^"\n\n\n\n\n");;


let ea opt1 opt2=
   if (opt1=None) then opt2=None else
   let (_,y1,z1)=Option.unpack opt1
   and (_,y2,z2)=Option.unpack opt2 in
   (y1,z1)=(y2,z2);;
   
let scp t s=Termite.parse (Termite.of_string t)
(Php_lexer.parse_string s);;   

let itv=Cull_string.interval;;

(* Module management code starts here *)

open German_pervasives;;

(*
if (German_wrapper.data()=[]) then German_wrapper.initialize();;
*)

(* Module management code ends here *)


