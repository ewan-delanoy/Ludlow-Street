(*

#use"half_dressed_module.ml";;

A module name, or a candidate for one. Can contain  slashes.
Should not contain dots.
Starts with an uncapitalized letter.
Designates a relative path.

*)

type t=HD of string*Directory_name.t;;

exception Inexistent_module of string;;
 
let of_string_and_root old_s dir=
        let s=Father_and_son.invasive_father old_s '.' in
        let s_dir=Directory_name.to_string dir in
	    if List.for_all (fun edg->not(Sys.file_exists(s_dir^s^edg)) ) Ocaml_ending.all_string_endings
	    then raise(Inexistent_module(s_dir^s))
	    else
	   HD(Father_and_son.invasive_father s '.',dir) ;;   
   
   
let to_string (HD (s,dir))=s;;

let unveil (HD (s,dir))=(s,dir);;

exception FileOutsideDirectory of Absolute_path.t*Directory_name.t;;


let of_path_and_root ap dir=
    if (not(Path_is_in_directory.path_is_in_directory ap dir))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Directory_name.to_string dir in
    let n_dir=String.length s_dir in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    HD(Father_and_son.invasive_father subpath '.',dir) ;;    
    
    
let undress (HD (s,dir))=
   try ((fun r->Naked_module.of_string(String.sub s (r+1) (String.length(s)-(r+1))) )
   (String.rindex s '/'))
   with
   _->Naked_module.of_string s;;

let is_optional (HD (s,dir))=
  if String.length(s)<9 then false else
  String.sub s 0 9="Optional/";;

let is_forgotten (HD (s,dir))=
  if String.length(s)<10 then false else
  String.sub s 0 10="Forgotten/";;

let is_remembered (HD (s,dir))=
  if String.length(s)<11 then false else
  String.sub s 0 11="Remembered/";;

let is_archived hd=(is_optional hd)||(is_forgotten hd)||(is_remembered hd);;

let is_executable (HD (s,dir))=
  let n=String.length s in
  if String.length(s)<10 then false else
  String.sub s (n-10) 10="executable";;


let optional_base_directory (HD (s,dir))=try (
   let r=String.rindex s '/' in
   let dir=Directory_name.of_string(String.sub s 0 r) in
   Some dir )
   with
   _->None;;

let subdirectory hm=
       let s_hm=to_string hm in
       let s_dir=Father_and_son.father s_hm '/' in
       Subdirectory.of_string s_dir;;

let root_directory (HD(s,dir))= dir;;

let module_name (HD (s,dir))=
  let t=Father_and_son.son s '/'  in
  (String.capitalize t);;

let ocaml_name (HD (s,dir))=
  "Half_dressed_module"^".of_string_and_index(\""^s^"\")("^
  (Directory_name.ocaml_name dir)^")";;    

let industrial_separator=Industrial_separator.new_separator ();;  

let archive x=
   let (s,dir)=unveil x in
   String.concat industrial_separator [s;Directory_name.to_string dir];;
  
let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator) s in
   let dir=Directory_name.of_string(List.nth l1 1) in
   HD(List.nth l1 0, dir);;
   
  
          
   
