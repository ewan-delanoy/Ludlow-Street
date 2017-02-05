 
(* 

#use"Makefile_makers/isidore_find_module_location.ml";;

*)


exception Inexistent_module of string;;

let in_modulesystem fs old_s=
  let s=Father_and_son.invasive_father old_s '.' in
  let temp1=Modulesystem.all_filedata fs in 
  let opt=Option.find_it
  	(fun fd->
  	   let hm=Modulesystem_data.name fd in
  	   let s_hm=Half_dressed_module.to_string hm in
  	   (Father_and_son.son s_hm '/')=s
  	) temp1 in
  match opt with
  None->raise(Inexistent_module(s)) 
  |Some(fd)->fd;;	   
  
let in_target_system fs old_s=
    in_modulesystem (Target_system.modulesystem fs) old_s;;