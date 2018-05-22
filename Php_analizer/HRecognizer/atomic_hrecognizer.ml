(*

#use"Php_analizer/HRecognizer/atomic_hrecognizer.ml";;

*)


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

(*
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
|Star(l)->let temp=String.concat ";" (Image.image (fun c->"'"^(String.make 1 c)^"'") l) in
          "A"^"tomic_hrecognizer.S"^"tar(["^temp^"])"
|Star_outside(l)->let temp=String.concat ";" (Image.image (fun c->"'"^(String.make 1 c)^"'") l) in
          "A"^"tomic_hrecognizer.S"^"tar_outside(["^temp^"])"
|Enclosed(c1,c2)->"A"^"tomic_hrecognizer.E"^"nclosed('"^(String.make 1 c1)^"','"^(String.make 1 c2)^"')"
|Simple_quoted->"A"^"tomic_hrecognizer.Simple_quoted"
|Double_quoted->"A"^"tomic_hrecognizer.Double_quoted";;
*)
