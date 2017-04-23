(*

#use"Country/Germany/german_update_copied_compiler.ml";;

*)

let prepare destdir=
  let l1=Alaskan_data.all_short_paths (German_wrapper.data()) in
  let l2=Image.image (
    fun ap->
      let s_ap=Absolute_path.to_string ap in
      Directory_name.cut_beginning German_constant.root s_ap
  ) (German_wrapper.outside_files()) in
  let main_diff=Prepare_dircopy_update.compute_diff (German_constant.root,l1@l2) destdir in
  Prepare_dircopy_update.commands_for_update destdir main_diff;;

(*
let prepare_pervasives_file destdir=
  let pervasives_file=Absolute_path.create_file(Directory_name.join destdir "my_pervasives.ml") in
  let rep_text="\n\nSys.chdir(\""^(Directory_name.to_string destdir)^"\");;\n\n" in
  Replace_inside.overwrite_between_markers_inside_file
  (Overwriter.of_string rep_text)   
  ("(* Directory setting starts here *)","(* Directory setting ends here *)")
  pervasives_file;;
*)

let prepare_special_file destdir filename=
  let the_file=Absolute_path.create_file(Directory_name.join destdir filename) in
  Replace_inside.replace_inside_file
  (Directory_name.to_string German_constant.root,Directory_name.to_string destdir)
  the_file;;


    
let ucc destdir=
  let _=Image.image Sys.command (prepare destdir) in
  let _=Image.image (prepare_special_file destdir)
    ["my_pervasives.ml";"my_printers.ml";"my_loadings.ml"]
   in 
  Alaskan_create_target_system.from_main_directory destdir None [];;
       
       






   
   
  