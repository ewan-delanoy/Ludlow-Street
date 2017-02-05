
(* 

#use"Country/Alaska/alaskan_data.ml";;


*)




 


let default_toplevel main_toplevel_name mdata=
  let temp2=List.filter Modulesystem_data.is_not_optional mdata in
  let temp3=Image.image Modulesystem_data.name temp2 in
  let temp4=List.filter (fun hm->
     Half_dressed_module.to_string(hm)<>Debugger_name.debugger_name
  ) temp3
  in
  Ocaml_target.toplevel main_toplevel_name temp4;; 
 
let find_module_registration mdata hm=
  Option.find_it(fun a->Modulesystem_data.name a=hm) mdata;;   