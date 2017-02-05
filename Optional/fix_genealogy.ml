
(* 


#use"Makefile_makers/fix_genealogy.ml";;


*)

let local_fix l fd=
  let anc=Check_genealogy.local_recomputation l fd in
  Modulesystem_data.fix_ancestors fd anc;;
  
let global_fix l=
   let rec tempf=(fun (graet,da_ober)->
     match da_ober with
     []->List.rev(graet)
     |fd::peurrest->
      let new_fd=local_fix graet fd in
      tempf(new_fd::graet,peurrest)
   ) in
   tempf([],l);;  

let fix ts=
  let fs=Target_system.modulesystem ts in
  let data=Modulesystem.all_filedata fs in
  let fixed_data=global_fix data in
  let dir=Modulesystem.root fs
  and s=Modulesystem.main_toplevel_name fs in
  let fixed_fs=Modulesystem.make(dir,fixed_data,s) in
  let fixed_ts=Target_system.make
   fixed_fs
   (Target_system.directories ts)
   (Target_system.up_to_date_targets ts) in
  fixed_ts;; 
   
   

 