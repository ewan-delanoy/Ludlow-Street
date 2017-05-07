(*

#use"half_dressed_module.ml";;

A module name, or a candidate for one. Can contain  slashes.
Should not contain dots.
Starts with an uncapitalized letter.
Designates a relative path.

*)

type t=GYQ of string*Directory_name.t;;

exception Inexistent_module of string;;
 
let of_string_and_root old_s dir=
        let s=Father_and_son.invasive_father old_s '.' in
        let s_dir=Directory_name.to_string dir in
	    if List.for_all (fun edg->not(Sys.file_exists(s_dir^s^edg)) ) Ocaml_ending.all_string_endings
	    then raise(Inexistent_module(s_dir^s))
	    else
	   GYQ(Father_and_son.invasive_father s '.',dir) ;;   
   
   
let to_string (GYQ (s,dir))=s;;

let unveil (GYQ (s,dir))=(s,dir);;

exception FileOutsideDirectory of Absolute_path.t*Directory_name.t;;


let of_path_and_root ap dir=
    if (not(Path_is_in_directory.path_is_in_directory ap dir))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Directory_name.to_string dir in
    let n_dir=String.length s_dir in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    GYQ(Father_and_son.invasive_father subpath '.',dir) ;;    
    
    
let naked_module (GYQ (s,dir))=
   try ((fun r->Naked_module.of_string(String.sub s (r+1) (String.length(s)-(r+1))) )
   (String.rindex s '/'))
   with
   _->Naked_module.of_string s;;

let is_optional (GYQ (s,dir))=
  if String.length(s)<9 then false else
  String.sub s 0 9="Optional/";;

let is_forgotten (GYQ (s,dir))=
  if String.length(s)<10 then false else
  String.sub s 0 10="Forgotten/";;

let is_remembered (GYQ (s,dir))=
  if String.length(s)<11 then false else
  String.sub s 0 11="Remembered/";;

let is_archived hm=(is_optional hm)||(is_forgotten hm)||(is_remembered hm);;

let is_executable (GYQ (s,dir))=
  let n=String.length s in
  if String.length(s)<10 then false else
  String.sub s (n-10) 10="executable";;


let optional_base_directory (GYQ (s,dir))=try (
   let r=String.rindex s '/' in
   let dir=Directory_name.of_string(String.sub s 0 r) in
   Some dir )
   with
   _->None;;

let subdirectory hm=
       let s_hm=to_string hm in
       let s_dir=Father_and_son.father s_hm '/' in
       Subdirectory.of_string s_dir;;

let root_directory (GYQ(s,dir))= dir;;

let module_name (GYQ (s,dir))=
  let t=Father_and_son.son s '/'  in
  (String.capitalize t);;

let ocaml_name (GYQ (s,dir))=
  "Half_dressed_module"^".of_string_and_index(\""^s^"\")("^
  (Directory_name.ocaml_name dir)^")";;    

let industrial_separator=Industrial_separator.new_separator ();;  

let archive x=
   let (s,dir)=unveil x in
   String.concat industrial_separator [s;Directory_name.to_string dir];;
  
let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator) s in
   let dir=Directory_name.of_string(List.nth l1 1) in
   GYQ(List.nth l1 0, dir);;
   
  
          
   
