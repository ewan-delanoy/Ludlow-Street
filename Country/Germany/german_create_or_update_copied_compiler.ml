(*

#use"Country/Germany/german_create_or_update_copied_compiler.ml";;

*)

let prepare destdir=
  let l1=Md_list.all_short_paths (German_wrapper.data()) in
  let main_diff=Prepare_dircopy_update.compute_diff 
        (German_constant.root,l1) destdir in
  Prepare_dircopy_update.commands_for_update destdir main_diff;;

let file_for_backup="Country/Germany/german_backup_target_system.ml";;

let replacement_for_special_file destdir filename=
  if filename=file_for_backup
  then ("let github_after_backup=ref(true)"^Double_semicolon.ds,
        "let github_after_backup=ref(false)"^Double_semicolon.ds)
  else (Directory_name.without_trailing_slash German_constant.root,
        Directory_name.without_trailing_slash destdir);;

let prepare_special_file destdir filename=
  let the_file=Absolute_path.create_file(Directory_name.join destdir filename) in
  Replace_inside.replace_inside_file
  (replacement_for_special_file destdir filename)
  the_file;;

let init_dir=
    Subdirectory.connectable_to_subpath 
    (German_constant.kept_up_to_date_but_not_registered);;

let up_to_date_but_not_registered_files=
   [
      German_constant.path_for_loadingsfile;
      German_constant.path_for_printersfile;
   ];;

let ucc destdir=
  let s_dir=Directory_name.connectable_to_subpath destdir in 
  let _=Unix_command.uc ("mkdir -p "^s_dir^"_build") in
  let _=Image.image Unix_command.uc (prepare destdir) in
  let _=Image.image (prepare_special_file destdir)
    (
      up_to_date_but_not_registered_files@
    ["Country/Germany/german_constant.ml";file_for_backup]
    ) 
   in 
  Alaskan_create_target_system.from_main_directory destdir;;
       
       






   
   
  