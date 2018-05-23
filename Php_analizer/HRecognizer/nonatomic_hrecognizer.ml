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
 
let write_as_chain_list x=match x with
   Chain(_,l)->l
   |_->[x];;  

let write_as_disjunction_list x=match x with
   Ordered_disjunction(_,l)->l
   |_->[x];;     

let write_as_disjunction_of_chains_list x=match x with
   Disjunction_of_chains(_,ll)->ll
   |Ordered_disjunction(_,l)->Image.image write_as_chain_list l
   |Chain(_,l)->[l]
   |_->[[x]];;     

let insert_into_disjunction_of_chains x y=
    (write_as_chain_list x)::(write_as_disjunction_of_chains_list y);;

let basic_replace l x=
  let name_for_x=name x in
   match Option.seek(fun y->name y=name_for_x) l with
    None->x
   |Some(y0)->y0;;

let replace_inside rep_data=
  let rep=basic_replace rep_data in 
  function
   Leaf(nahme,atm)->Leaf(nahme,atm)
  |Chain(nahme,l)->Chain(nahme,Image.image rep l)
  |Ordered_disjunction(nahme,l)->Ordered_disjunction(nahme,Image.image rep l)
  |Star(nahme,x)->Star(nahme,rep x)
  |Maybe(nahme,x)->Maybe(nahme,rep x)
  |Avoider(nahme,(x,l))->Avoider(nahme,(rep x,l))
  |Motionless(nahme,x)->Motionless(nahme,rep x)
  |Disjunction_of_chains(nahme,ll)->Disjunction_of_chains(nahme,
         Image.image (Image.image rep) ll);;

(*         
let present x=
    match x with
     Leaf(nahme,atm)->let atm_pname = Atomic_hrecognizer.prepare_ocaml_name atm in
                      let atm_name =  
                      "let "^nahme^"_rcgzr=N"^"onatomic_hrecognizer.L"^"eaf("
                      ^(Atomic_hrecognizer.prepare_ocaml_name atm)^");;"
    |Chain(nahme,l)->"let "^nahme^"_rcgzr=N"^"onatomic_hrecognizer.C"^"hain(["
                      ^(String.concat ";" (Image.image (fun r->
                       (name r)^"_rcgzr") l ))^"]);;"
    |Ordered_disjunction(nahme,l)->
                    "let "^nahme^"_rcgzr=N"^"onatomic_hrecognizer.O"^"rdered_disjunction(["
                    ^(String.concat ";" (Image.image (fun r->
                     (name r)^"_rcgzr") l ))^"]);;"  
    |Star(nahme,x)->"let "^nahme^"_rcgzr=N"^"onatomic_hrecognizer.S"^"tar("
                    ^nahme^","^(name x)^"_rcgzr);;"
    |Maybe(nahme,x)->"let "^nahme^"_rcgzr=N"^"onatomic_hrecognizer.M"^"aybe("
                    ^nahme^","^(name x)^"_rcgzr);;"
    |Avoider(nahme,(x,l))->"let "^nahme^"_rcgzr=N"^"onatomic_hrecognizer.A"^"voider("
                    ^nahme^",("^(name x)^"_rcgzr,["
                    ^(String.concat ";" (Image.image (fun s->"\""^s^"\"") l))
                    ^"]));;"
    |Motionless(nahme,x)->"let "^nahme^"_rcgzr=N"^"onatomic_hrecognizer.M"^"otionless("
                    ^(name x)^"_rcgzr);;"
    |Disjunction_of_chains(nahme,ll)->
                  let tempf=(fun l->
                  "["^(String.concat ";" (Image.image (fun r->
                   (name r)^"_rcgzr") l ))^"]"
                  ) in
                  "let "^nahme^"_rcgzr=N"^"onatomic_hrecognizer.D"^"isjunction_of_chains(["
                  ^(String.concat ";" (Image.image tempf ll ))^"]);;"
                 ;;
        
*)

let print (x:t)="rn \""^(name x)^"\"";;

let print_out (fmt:Format.formatter) (x:t)=
   Format.fprintf fmt "@[%s@]" (print x);;