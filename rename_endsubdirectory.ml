(*

#use"rename_endsubdirectory.ml";;

*)



let re (old_subdir,new_esdname) s=
   let s_old_subdir=Subdirectory.without_trailing_slash old_subdir in
   if Substring.begins_with s s_old_subdir
   then let sub_s=Cull_string.cobeginning (String.length s_old_subdir) s in
        (Father_and_son.father s_old_subdir '/')^"/"^new_esdname^sub_s
   else s;;
   
(*

re (Subdirectory.of_string("Haag/Huug"),"Java") ("Haag/Huug/King/Jordan/and_co.ml");;

*)   