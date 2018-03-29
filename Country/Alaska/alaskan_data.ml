
(* 

#use"Country/Alaska/alaskan_data.ml";;


*)

let all_mlx_files mdata=
  List.flatten
  (Image.image Modulesystem_data.acolytes mdata);; 

let all_mlx_paths mdata=Image.image Mlx_ended_absolute_path.to_absolute_path 
  (all_mlx_files mdata);;  

let all_short_paths mdata=List.flatten(
  Image.image Modulesystem_data.short_paths mdata
);;


let compute_subdirectories_list mdata=
  let temp1=Image.image (
      fun md->
       let hm=Modulesystem_data.name md in
       Subdirectory.without_trailing_slash(Half_dressed_module.subdirectory hm)
  ) mdata in
  let temp2=Ordered_string.diforchan temp1 in
  let temp3=Ordered_string.forget_order temp2 in
  Image.image Subdirectory.of_string temp3;;


let default_toplevel main_toplevel_name mdata=
  let temp2=List.filter Modulesystem_data.is_not_optional mdata in
  let temp3=Image.image Modulesystem_data.name temp2 in
  let temp4=List.filter (fun hm->
     Half_dressed_module.to_string(hm)<>Debugged_name.debugger_name
  ) temp3
  in
  Ocaml_target.toplevel main_toplevel_name temp4;; 
 
let find_module_registration mdata hm=
  Option.seek(fun a->Modulesystem_data.name a=hm) mdata;;   

let default_targets main_toplevel_name mdata=
    let temp1=Image.image Ocaml_target.from_modulesystem_data mdata 
    and temp2=Image.image Modulesystem_data.name mdata in
    let temp3=List.flatten temp1  in
    temp3@[Ocaml_target.toplevel main_toplevel_name temp2]
    ;;

let industrial_separator=Industrial_separator.alaskan_data;;    

let archive mdata=
       Nonblank.make(String.concat industrial_separator 
       (Image.image Modulesystem_data.archive mdata));;
      
  
let unarchive s=
     let v1=Str.split (Str.regexp_string industrial_separator) (Nonblank.decode(s)) in
     Image.image Modulesystem_data.unarchive v1;;
       
  
      