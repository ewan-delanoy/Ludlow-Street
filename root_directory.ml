(*

Subdirectories name, with the trailing slash removed.

#use"root_directory.ml";;

*)

let without_trailing_slash (Root_directory_t.R s)=s;;

exception Non_directory of string;;


let of_string s=
  let temp1=Tools_for_absolute_path.of_string s in
  if Sys.is_directory temp1
  then Root_directory_t.R(Tools_for_absolute_path.remove_trailing_slash temp1)
  else raise(Non_directory(s));;



exception Nonexistent_file of string;;

module Private=struct
let check_filename t=
  if Sys.file_exists t
  then t
  else raise(Nonexistent_file(t));;
end;;

let join (Root_directory_t.R s) w=Private.check_filename(s^"/"^w);;

let connectable_to_subpath (Root_directory_t.R s)=s^"/";;  

exception Cut_error of Root_directory_t.t*string;;

let cut_beginning (Root_directory_t.R s) w=
  let ns=String.length(s)
  and nw=String.length(w) in
  if (ns+1)>nw then raise(Cut_error(Root_directory_t.R s,w)) else
  if (String.sub w 0 (ns+1))<>(s^"/") then raise(Cut_error(Root_directory_t.R s,w)) else
  String.sub w (ns+1) (nw-ns-1);;

exception Failed_relocation of Absolute_path.t*Root_directory_t.t;;

let relocate ap new_dir=
    let old_path=Absolute_path.to_string ap in
    let fn=Father_and_son.son old_path '/' in 
    let new_path=(connectable_to_subpath new_dir)^fn in
    let i=Unix_command.uc("mv "^old_path^" "^new_path) in
    if i<>0
    then raise(Failed_relocation(ap,new_dir))
    else Absolute_path.of_string new_path;;

let force_join (Root_directory_t.R s) w=
      let t=s^"/"^w in
      if Sys.file_exists t
      then t
      else let _=Unix_command.uc("touch "^t) in
           t;;

