(*

#use"debugger.ml";;
Used for debugging purposes only.


*)

let s1=Io.read_whole_file 
(Absolute_path.of_string "outside_comments_and_strings.ml");;
let s2=Cull_string.cobeginning 3301 s1;;

let z1=Read_ocaml_files.Private.read1 s2;;
let z2=Gparser_apply.apply Gparser_for_ocaml_language.main_prsr s2 1;;
let z3=Gparser_apply.apply Gparser_for_ocaml_language.elt_prsr s2 1;;
let z4=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_comment s2 1;;


(*

let amy1 ()=
 let _=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_value_making
"let peggy x=45;;" 1 in
failwith("I wanted to fail");;

amy1();;

*)




(*

German_wrapper.initialize();;


let act1=Compute_all_ocaml_items.caoi 
(German_wrapper.data());;


*)




