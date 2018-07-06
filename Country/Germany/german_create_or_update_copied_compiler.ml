(*

#use"Country/Germany/german_create_or_update_copied_compiler.ml";;

*)

let prepare destdir=
  let l1=Modify_md_list.all_short_paths (German_wrapper.data()) in
  let main_diff=Prepare_dircopy_update.compute_diff 
        (German_constant.root,l1) destdir in
  Prepare_dircopy_update.commands_for_update destdir main_diff;;

let file_for_backup="Country/Alaska/alaskan_backup_target_system.ml";;

let replacement_for_special_file destdir filename=
  if filename=file_for_backup
  then ("let github_after_backup=ref(true)"^Double_semicolon.ds,
        "let github_after_backup=ref(false)"^Double_semicolon.ds)
  else (Root_directory.without_trailing_slash German_constant.root,
        Root_directory.without_trailing_slash destdir);;

let prepare_special_file destdir filename=
  let the_file=Absolute_path.create_file(Root_directory.join destdir filename) in
  Replace_inside.replace_inside_file
  (replacement_for_special_file destdir filename)
  the_file;;

let init_dir=
    Subdirectory.connectable_to_subpath 
    (Coma_constant.kept_up_to_date_but_not_registered);;

let up_to_date_but_not_registered_files=
   [
      Coma_constant.path_for_loadingsfile;
      Coma_constant.path_for_printersfile;
   ];;

let ucc destdir=
  let knr=Subdirectory.without_trailing_slash(Coma_constant.kept_up_to_date_but_not_registered) in
  let s_dir=Root_directory.connectable_to_subpath destdir in 
  let _=Unix_command.uc ("mkdir -p "^s_dir^"_build") in
  let _=Unix_command.uc ("mkdir -p "^s_dir^knr) in
  let _=Image.image (
      fun s->Unix_command.uc("touch "^s_dir^s)
  ) up_to_date_but_not_registered_files in
  let _=Image.image Unix_command.uc (prepare destdir) in
  let _=Image.image (prepare_special_file destdir)
    (
      up_to_date_but_not_registered_files@
    ["Country/Germany/german_constant.ml";file_for_backup]
    ) 
   in 
  Alaskan_create_target_system.from_main_directory destdir;;
       
       






   
   
  