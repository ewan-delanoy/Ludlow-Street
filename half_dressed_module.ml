(*

#use"half_dressed_module.ml";;

A module name, or a candidate for one. Can contain  slashes.
Should not contain dots.
Starts with an uncapitalized letter.
Designates a relative path.

*)


          
type t={
   bundle_main_dir : string;
   subdirectory    : string;
   naked_module     : string;
};;

let bundle_main_dir x=Root_directory.of_string(x.bundle_main_dir);;
let subdirectory x=Subdirectory.of_string(x.subdirectory);;
let naked_module  x=Naked_module.of_string(x.naked_module);;


exception Inexistent_module of string;;
 
let of_string_and_root old_s dir=
        let s=Father_and_son.invasive_father old_s '.' in
        let s_dir=Root_directory.without_trailing_slash dir in
      if List.for_all (fun edg->not(Sys.file_exists(s_dir^"/"^s^edg)) ) 
           Ocaml_ending.all_string_endings
	    then raise(Inexistent_module(s_dir^s))
	    else
	    {
	      bundle_main_dir = s_dir;
   		  subdirectory    =Father_and_son.father s '/';
          naked_module     =Father_and_son.son s '/';
	    };;  
   
let uprooted_version x=
   let sub=x.subdirectory in
   if sub=""
   then x.naked_module
   else sub^"/"^(x.naked_module);;

 

let to_shortened_string x=x.naked_module;;   

let unveil x=(uprooted_version x,bundle_main_dir x);;

exception FileOutsideDirectory of Absolute_path.t*Root_directory_t.t;;


let of_path_and_root ap dir=
   if (not(Substring.begins_with (Absolute_path.to_string ap)
         (Root_directory.connectable_to_subpath dir)))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Root_directory.without_trailing_slash dir in
    let n_dir=(String.length s_dir)+1 in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    let s=Father_and_son.invasive_father subpath '.' in
    {
	      bundle_main_dir = s_dir;
   		  subdirectory    =Father_and_son.father s '/';
          naked_module     =Father_and_son.son s '/';
    }  ;;    


let is_executable x=
  let s=uprooted_version x in 
  let n=String.length s in
  if String.length(s)<10 then false else
  String.sub s (n-10) 10="executable";;

let capitalized_module_name x=
  (String.capitalize_ascii x.naked_module);;
  
let rename_endsubdirectory (old_subdir,new_subdirname) x=
   {
	      bundle_main_dir = x.bundle_main_dir;
   		  subdirectory    = Subdirectory.without_trailing_slash(
   		                    Subdirectory.rename_endsubdirectory
   		                       (old_subdir,new_subdirname) 
   		                       (Subdirectory.of_string(x.subdirectory)));
          naked_module    = x.naked_module;
    }  ;;    
   

let industrial_separator=Industrial_separator.half_dressed_module;;  
let industrial_separator2=Industrial_separator.half_dressed_module_times_boolean;;  

let archive x=
   String.concat industrial_separator 
    [x.bundle_main_dir;x.subdirectory;x.naked_module];;
  
let unarchive s=
   let l1=Str.split (Str.regexp_string industrial_separator) s in
   {
	      bundle_main_dir = List.nth l1 0;
   		  subdirectory    = List.nth l1 1;
          naked_module    = List.nth l1 2;
    };;  
   
let archive_pair (x,is_compiled_correctly)=
  String.concat industrial_separator2 
  [archive x;string_of_bool is_compiled_correctly];;

let unarchive_pair s=
    let l1=Str.split (Str.regexp_string industrial_separator2) s in
    (unarchive(List.nth l1 0),
     bool_of_string(List.nth l1 1));;
   
