(*

#use"prepare_dircopy_update.ml";;

*)

type diff={
   recently_deleted : string list;
   recently_created : string list;
   recently_changed : string list;
};;

let recently_deleted x=x.recently_deleted;;
let recently_created x=x.recently_created;;
let recently_changed x=x.recently_changed;;

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
   "\n"::msg::(Image.image(fun w->"\t\t"^w) x.recently_deleted)
   ) in
   let temp1=tempf "Deleted : " x.recently_deleted
   and temp2=tempf "Created : " x.recently_created
   and temp3=tempf "Changed : " x.recently_changed in
   let temp4=String.concat "\n" (temp1@temp2@temp3) in
   (print_string temp4;
    flush stdout);;
 
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
   
let compute_greedy_diff sourcedir destdir=
   let source_ap=Absolute_path.of_string(Directory_name.to_string sourcedir) in
   let source_paths=More_unix.complete_ls_with_nondirectories_only source_ap in
   let l=Image.image (fun ap->
   Directory_name.cut_beginning sourcedir (Absolute_path.to_string ap) ) source_paths in
   compute_diff (sourcedir,l) destdir;;
   
  
   
   