
(* 

#use"Makefile_makers/usual_coma_state.ml";;

*)

let usual_root=
                Root_directory.of_string 
                "/Users/ewandelanoy/Documents/OCaml/Idaho";;
let backup_root=
                Root_directory.of_string 
                "/Users/ewandelanoy/Documents/OCaml/Backup_for_Idaho";;

let main_ref=Coma_state.empty_one
                usual_root
                backup_root;;