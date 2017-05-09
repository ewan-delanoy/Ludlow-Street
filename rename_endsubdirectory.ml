(*

#use"rename_endsubdirectory.ml";;

*)



let re (old_subdir,new_esdname) s=
   let s_old_subdir=Subdirectory.without_trailing_slash old_subdir in
   if Substring.begins_with s s_old_subdir
   then let sub_s=Cull_string.cobeginning (String.length s_old_subdir) s in
        (Father_and_son.father s_old_subdir '/')^"/"^new_esdname^sub_s
   else s;;
   
let on_absolute_path (old_subdir,new_subdirname) ap=
  let s_old_subdir=Subdirectory.connectable_to_subpath old_subdir in
  let s_ap=Absolute_path.to_string ap in
  let old_fulldir=(Directory_name.to_string(German_constant.root))^s_old_subdir in
  if Substring.begins_with s_ap old_fulldir
  then let sub_s=Cull_string.cobeginning (String.length old_fulldir) s_ap in
       Absolute_path.of_string(new_subdirname^sub_s)
  else ap;;   
   
(*

re (Subdirectory.of_string("Haag/Huug"),"Java") ("Haag/Huug/King/Jordan/and_co.ml");;

*)   