(*

#use"Makefile_makers/toplevel_from_modulesystem.ml";;

*)


let toplevel_from_modulesystem fs=
  let temp1=Modulesystem.all_filedata fs in
  let temp2=List.filter Modulesystem_data.is_not_optional temp1 in
  let temp3=Image.image Modulesystem_data.name temp2 in
  let temp4=List.filter (fun hm->
     Half_dressed_module.to_string(hm)<>Debugger_name.debugger_name
  ) temp3
  in
  Ocaml_target.toplevel (Modulesystem.main_toplevel_name fs) temp4;; 
 
