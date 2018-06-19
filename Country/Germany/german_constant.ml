
(* 

#use"Country/Germany/german_constant.ml";;


*)


let root=Directory_name.of_string "/Users/ewandelanoy/Documents/OCaml/Ordinary";;
let dir_for_backup=Directory_name.of_string 
"/Users/ewandelanoy/Documents/OCaml/Githubbed_ocaml";;

let kept_up_to_date_but_not_registered=
  Subdirectory.of_string "Kept_up_to_date_but_not_registered";;

let not_registered_any_more=
    Subdirectory.of_string "Left_out_of_updating";;  

let old_and_hardly_reusable=
    Subdirectory.of_string "Old_and_hardly_reusable";;  

let temporary=
  Subdirectory.of_string "Temporary";;

let name_for_makefile="makefile";;
let name_for_targetfile="targetfile.ocaml_made";;
let name_for_merlinfile=".merlin";;

let name_for_diaryfile="diary.ml";;
let name_for_loadingsfile="my_loadings.ml";;
let name_for_printersfile="my_printers.ml";;

let path_for_diaryfile=
    (Subdirectory.connectable_to_subpath temporary)^name_for_diaryfile;;
let path_for_loadingsfile=
    (Subdirectory.connectable_to_subpath kept_up_to_date_but_not_registered)^name_for_loadingsfile;;
let path_for_printersfile=
    (Subdirectory.connectable_to_subpath kept_up_to_date_but_not_registered)^name_for_printersfile;;
  
let up_to_date_but_not_registered_files=
      [
         path_for_diaryfile;
         path_for_loadingsfile;
         path_for_printersfile;
      ];;



  
 
 
 
