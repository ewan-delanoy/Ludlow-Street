(*

#use"Country/Germany/german_forget_unregistered_file.ml";;

*)


let forget ap=
   let s_dir=Directory_name.connectable_to_subpath(German_constant.root) in
   let n_dir=String.length s_dir in
   let s_ap=Absolute_path.to_string ap in
   let subpath=Cull_string.cobeginning n_dir s_ap in
   let trash_dir=Subdirectory.without_trailing_slash
               (German_constant.old_and_hardly_reusable) in
   let new_subpath=(Current_date.current_date())^"_"^
         (Replace_inside.replace_inside_string ("/","_dir_") subpath) in
   let _=Unix_command.uc ("mkdir -p "^s_dir^trash_dir) in
   let _=Unix_command.uc ("touch "^s_dir^trash_dir^"/"^new_subpath) in
   let _=Unix_command.uc ("mv "^s_ap^" "^s_dir^trash_dir^"/"^new_subpath) in
   subpath;;


