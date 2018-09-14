
(* 

#use"Makefile_makers/usual_coma_state.ml";;

*)

let usual_root=
                Root_directory.of_string 
                "/Users/ewandelanoy/Documents/OCaml/Ordinary";;
let backup_root=
                Root_directory.of_string 
                "/Users/ewandelanoy/Documents/OCaml/Githubbed_ocaml";;

let main_ref=Coma_state.empty_one
                usual_root
                backup_root;;