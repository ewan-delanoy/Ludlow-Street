(*

#use"rename_endsubdirectory.ml";;


*)

exception Already_present_directory of string;;

let in_unix_world root_dir (old_subdir,new_esdname)=
   let s_root=Root_directory.connectable_to_subpath root_dir in
   let s_old_subdir=Subdirectory.without_trailing_slash old_subdir in
   let new_name=s_root^(Father_and_son.father s_old_subdir '/')^"/"^new_esdname in
   if Sys.file_exists(new_name)
   then raise(Already_present_directory(new_name))
   else 
   let container=Father_and_son.father  new_name '/' in
   let _=
   Unix_command.uc 
     ("mkdir -p "^container) in
   Unix_command.uc 
     ("mv "^s_root^s_old_subdir^" "^new_name) ;;

let re (old_subdir,new_esdname) s=
   let s_old_subdir=Subdirectory.without_trailing_slash old_subdir in
   if Substring.begins_with s s_old_subdir
   then let sub_s=Cull_string.cobeginning (String.length s_old_subdir) s in
        (Father_and_son.father s_old_subdir '/')^"/"^new_esdname^sub_s
   else s;;
   
let on_absolute_path root_dir (old_subdir,new_subdirname) ap=
  let s_old_subdir=Subdirectory.connectable_to_subpath old_subdir in
  let s_ap=Absolute_path.to_string ap in
  let old_fulldir=(Root_directory.connectable_to_subpath root_dir)^s_old_subdir in
  if Substring.begins_with s_ap old_fulldir
  then let sub_s=Cull_string.cobeginning (String.length old_fulldir) s_ap in
       Absolute_path.of_string(new_subdirname^sub_s)
  else ap;;   
   
   
   
(*

re (Subdirectory.of_string("Haag/Huug"),"Java") ("Haag/Huug/King/Jordan/and_co.ml");;

*)   