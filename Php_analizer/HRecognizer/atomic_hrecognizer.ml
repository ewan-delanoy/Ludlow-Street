(*

#use"Php_analizer/HRecognizer/atomic_hrecognizer.ml";;

*)

type t=
  Constant of string
 |Later_constant of string
 |Star of char list
 |Star_outside of char list
 |Enclosed of char*char
 |Simple_quoted
 |Double_quoted;;

let constant s=Constant(s);;
let later_constant s=Later_constant(s);;
let star l=Star(l);;
let star_outside l=Star_outside(l);;
let enclosed (opener,closer)=Enclosed (opener,closer);;
let simple_quoted=Simple_quoted;;
let double_quoted=Double_quoted;;


let prepare_ocaml_name x=
 let modname="A"^"tomic_hrecognizer." in 
  match x with
 Constant(s)->Prepare_ocaml_name.for_labelled_elt
                Prepare_ocaml_name.for_string 
                (modname^"C"^"onstant")  s
|Later_constant(s)->
              Prepare_ocaml_name.for_labelled_elt
                Prepare_ocaml_name.for_string 
                (modname^"L"^"ater_constant")  s
|Star(l)->Prepare_ocaml_name.for_labelled_elt
                Prepare_ocaml_name.for_char_list
                (modname^"S"^"tar")  l
|Star_outside(l)->Prepare_ocaml_name.for_labelled_elt
                Prepare_ocaml_name.for_char_list
                (modname^"S"^"tar_outside")  l
|Enclosed(c1,c2)->Prepare_ocaml_name.for_labelled_pair
                (Prepare_ocaml_name.for_char,Prepare_ocaml_name.for_char)
                (modname^"S"^"tar_outside")  (c1,c2)
|Simple_quoted->Prepare_ocaml_name.for_string (modname^"Simple_quoted")
|Double_quoted->Prepare_ocaml_name.for_string (modname^"Simple_quoted");;

