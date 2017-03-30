(*

#use"Haag1/Haag2/please_test_me.ml";;

*)


module Mood=struct

type mylabel= A |B |C |D |E;;

type labelled_value={
   label:mylabel;
   value:int;
};;

let example={label=D;value=57;};;

end;;


let f x=match x.Mood.label with Mood.A|Mood.B|Mood.C->(1,x) 
|Mood.D->(2,Mood.example) |Mood.E->(3,Mood.example);;



(*

let f x=match x.Ocaml_gsyntax_item.category with
 Ocaml_gsyntax_category.Value                                                                          | Type                                                                          
  | Ocaml_gsyntax_category.Type
  | Ocaml_gsyntax_category.Exception->(1,x)
  | Ocaml_gsyntax_category.Module_opener->(2,x)
  | Ocaml_gsyntax_category.Module_closer->(3,x)
  | Ocaml_gsyntax_category.Module_inclusion->(4,x);;

module Mood=struct

type mytype= A |B |C |D |E;;

end;;

let f x=match x with Mood.A|Mood.B|Mood.C->(1,x) |Mood.D->(2,Mood.D) |Mood.E->(3,Mood.E);;


module Weak=struct
let e=6;;
end;;
*)

