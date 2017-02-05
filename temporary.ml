(*

#use"temporary.ml";;

*)


let cmd_for_tgt dir=German_command_for_ocaml_target.command_for_ocaml_target_in_dir;;
let ingr_for_tgt dir=German_ingredients_for_ocaml_target.ingredients_for_ocaml_target;;
let ingr_for_top dir=German_ingredients_for_ocaml_target.marked_ingredients_for_unprepared_toplevel;;

let force_modification_time_update dir=
  German_modify_modulesystem.force_modification_time_update;;