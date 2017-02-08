(*

#use"prepare_dircopy_update.ml";;

*)

type diff={
   recently_deleted : string list;
   recently_changed : string list;
   recently_created : string list;
};;

let recently_deleted x=x.recently_deleted;;
let recently_created x=x.recently_created;;
let recently_changed x=x.recently_changed;;

let veil (a,b,c)={
   recently_deleted =a;
   recently_changed =b;
   recently_created =c;

};;

let compute_deleted_in_diff sourcedir destdir=
   let s_sourcedir=Directory_name.to_string sourcedir
   and s_destdir=Directory_name.to_string destdir in
   let temp1=More_unix.quick_beheaded_complete_ls s_destdir in
   List.filter(
       fun s->(s<>"README")
              &&(not(Substring.begins_with s ".git/")) 
              &&(not(Sys.file_exists(s_sourcedir^s)))
   ) temp1;;
   
let compute_nondeleted_in_diff (sourcedir,l) destdir=
   let s_sourcedir=Directory_name.to_string sourcedir
   and s_destdir=Directory_name.to_string destdir in
   let created_accu=ref[]
   and changed_accu=ref[] in
   let _=Image.image(
   	  fun s->
   	    if (not(Sys.file_exists(s_destdir^s)))
   	    then created_accu:=s::(!created_accu)
   	    else 
   	    (
   	    let txt1=Io.read_whole_file
   	    (Absolute_path.of_string(s_sourcedir^s))
   	    and txt2=Io.read_whole_file
   	    (Absolute_path.of_string(s_destdir^s)) in
   	    if txt1<>txt2
   	    then changed_accu:=s::(!changed_accu)
   	    )
   ) l in
   (!created_accu,!changed_accu);;   
   
let display_diff x=
   let tempf=(fun msg l->
   "\n"::msg::(Image.image(fun w->"\t\t"^w) l)
   ) in
   let temp1=tempf "Deleted : " x.recently_deleted
   and temp2=tempf "Created : " x.recently_created
   and temp3=tempf "Changed : " x.recently_changed in
   let temp4=String.concat "\n" (temp1@temp2@temp3) in
   (print_string temp4;
    flush stdout);;
 
let summarize_short_path s=
   String.capitalize(Father_and_son.son (Father_and_son.invasive_father s '.') '/');;
 
let summarize_short_path_list l=
    let temp1=Image.image summarize_short_path l in
    Ordered.forget_order(Ordered_string.diforchan temp1);;
 
let explain_diff x=
   let tempf=(fun (msg,l)->
     if l=[]
     then None
     else Some(msg^" "^(String.concat "," l)^".")
   ) in
   let temp1=Option.filter_and_unpack tempf
   [
     "Deleted",summarize_short_path_list(x.recently_deleted);
     "Created",summarize_short_path_list(x.recently_created);
     "Modified",summarize_short_path_list(x.recently_changed);
   ] in
   if temp1=[] then "" else
   let temp2=(String.uncapitalize (List.hd temp1))::(List.tl temp1) in
   String.concat " " temp2;; 
 
let diff_is_empty x=
  (x.recently_deleted,x.recently_created,x.recently_changed)=
   ([],[],[]);;
  
let compute_diff (sourcedir,l) destdir=
   let (created,changed)=compute_nondeleted_in_diff (sourcedir,l) destdir in
   {
   	recently_deleted=compute_deleted_in_diff sourcedir destdir;
   	recently_created=created;
   	recently_changed=changed;
   };;
   
let greedy_list sourcedir=
   let source_ap=Absolute_path.of_string(Directory_name.to_string sourcedir) in
   let source_paths=More_unix.complete_ls_with_nondirectories_only source_ap in
   Image.image (fun ap->
   Directory_name.cut_beginning sourcedir (Absolute_path.to_string ap) ) source_paths;;
      
   
let compute_greedy_diff sourcedir destdir=
   compute_diff (sourcedir,greedy_list sourcedir) destdir;;
   
let commands_for_update diff=
   if diff_is_empty diff
   then []
   else 
   let destination_dir=German_constant.dir_for_backup in
   let s_destination=Directory_name.to_string destination_dir in
   let created_ones=diff.recently_created  in
   let temp2=Option.filter_and_unpack
   (fun fn->
     if String.contains fn '/'
     then let dn=Father_and_son.father fn '/' in
          Some("mkdir -p "^s_destination^dn)
     else None 
    )
   created_ones in
   let temp3=Ordered.forget_order(Ordered_string.diforchan temp2) in
   let s_source=Directory_name.to_string German_constant.root in
   let temp4=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^(Father_and_son.father fn '/')
   ) created_ones in
   let changed_ones=diff.recently_changed in
   let temp5=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^fn
   ) changed_ones in
   let temp7=Image.image(
      fun fn->
      "git rm "^fn
   ) (diff.recently_deleted) in
   (temp3@temp4@temp5@temp7);;  
   
   