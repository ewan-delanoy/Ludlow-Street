(*

#use"Php_analizer/HRecognizer/nonatomic_hrecognizer.ml";;

*)

type t=
  Leaf of string*Atomic_hrecognizer.t
 |Chain of string*(t list)
 |Ordered_disjunction of string*(t list)
 |Star of string*t
 |Maybe of string*t
 |Avoider of string*(t*(string list))
 |Motionless of string*t
 |Disjunction_of_chains of string*(t list list);;

let name=function
  Leaf(s,_)->s
  |Chain(s,_)->s
  |Ordered_disjunction(s,_)->s
  |Star(s,_)->s
  |Maybe(s,_)->s
  |Avoider(s,_)->s
  |Motionless(s,_)->s
  |Disjunction_of_chains(s,_)->s;;

let leaf s x=Leaf(s,x);;
let chain s l=Chain(s,l);;
let ordered_disjunction s l= Ordered_disjunction(s,l);;
let star s l=Star(s,l);; 
let maybe s l=Maybe(s,l);; 
let avoider s (atm,l)=Avoider(s,(atm,l));; 
let motionless s l=Motionless(s,l);; 
let disjunction_of_chains s ll=Disjunction_of_chains(s,ll);;

type unveiled_data= Hrecognizer_casename.t * t list *
Atomic_hrecognizer.t option * string list option * (t list list);;

let unveil =((function
  Leaf(s,atm)->(Hrecognizer_casename.Leaf,[],Some atm,None,[])
 |Chain(_,l)->(Hrecognizer_casename.Chain,l,None,None,[])
 |Ordered_disjunction(_,l)->(Hrecognizer_casename.Ordered_disjunction,l,None,None,[])
 |Star(_,x)->(Hrecognizer_casename.Star,[x],None,None,[])
 |Maybe(_,x)->(Hrecognizer_casename.Maybe,[x],None,None,[])
 |Avoider(_,(x,l))->(Hrecognizer_casename.Avoider,[x],None,Some(l),[])
 |Motionless(_,x)->(Hrecognizer_casename.Motionless,[x],None,None,[])
 |Disjunction_of_chains(_,ll)->(Hrecognizer_casename.Ordered_disjunction,[],None,None,ll)):
 
  t -> unveiled_data);;
 
let write_as_list x=match x with
   Chain(_,l)->l
   |_->[x];;  


let print (x:t)="rn \""^(name x)^"\"";;

let print_out (fmt:Format.formatter) (x:t)=
   Format.fprintf fmt "@[%s@]" (print x);;