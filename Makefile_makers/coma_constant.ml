
(* 

#use"Makefile_makers/coma_constant.ml";;

*)

let kept_up_to_date_but_not_registered=
  Subdirectory.of_string "Kept_up_to_date_but_not_registered";;

let left_out_of_updating=
    Subdirectory.of_string "Left_out_of_updating";;  

let old_and_hardly_reusable=
    Subdirectory.of_string "Old_and_hardly_reusable";;  

let temporary=
  Subdirectory.of_string "Temporary";;

let build_subdir=      Subdirectory.of_string "_build";;
let debug_build_subdir=Subdirectory.of_string "_debug_build";;  
let exec_build_subdir= Subdirectory.of_string "_exec_build";;  

let name_for_makefile="makefile";;
let name_for_targetfile="targetfile.ocaml_made";;
let name_for_merlinfile=".merlin";;

let name_for_loadingsfile="my_loadings.ml";;
let name_for_printersfile="my_printers.ml";;


let path_for_loadingsfile=
  (Subdirectory.connectable_to_subpath kept_up_to_date_but_not_registered)^name_for_loadingsfile;;
let path_for_printersfile=
  (Subdirectory.connectable_to_subpath kept_up_to_date_but_not_registered)^name_for_printersfile;;

let up_to_date_but_not_registered_files=
    [
       path_for_loadingsfile;
       path_for_printersfile;
    ];;

let name_for_debugged_file="debugged.ml";;
let path_for_debugged_file=name_for_debugged_file;; 

let name_for_debugged_module="debugged";;    

   

