
(* 


#use"Makefile_makers/modify_modulesystem.ml";;


*)


 
 exception Non_registered_file of Mlx_filename.t;;  
 exception Non_registered_module of Half_dressed_module.t;;  
 exception Abandoned_children of Mlx_filename.t*(Half_dressed_module.t list);;
 exception Derelict_children of Half_dressed_module.t*(Half_dressed_module.t list);;  
   
 let reset_inactivity_counts fs=
    let temp1=Image.image Modulesystem_data.reset_inactivity_count (Modulesystem.all_filedata fs) in 
    Modulesystem.make(
       Modulesystem.root fs,
       temp1,
       Modulesystem.main_toplevel_name fs
    );;  
   
 let unregister_mlx_file_and_keep_old_data fs mlxfile=
    let hm=Mlx_filename.half_dressed_core mlxfile in
    let desc=Modulesystem.descendants fs [hm] in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
         raise(Abandoned_children(mlxfile,temp1))
    else
    let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) (Modulesystem.all_filedata fs) in
    match opt with
     None->raise(Non_registered_file(mlxfile))
    |Some(dt)->
      let edg=Mlx_filename.ending mlxfile in
      if (not(Modulesystem_data.check_presence edg dt))
      then raise(Non_registered_file(mlxfile))
      else 
      let new_dt=Modulesystem_data.make_absence edg dt in
      if (Modulesystem_data.registered_endings new_dt)=[]
      then (dt,Modulesystem.make(Modulesystem.root fs,before@after,Modulesystem.main_toplevel_name fs ))
      else (dt,Modulesystem.make(Modulesystem.root fs,before@(new_dt::after),Modulesystem.main_toplevel_name fs ));;
    
   
 let unregister_mlx_file fs mlxfile=snd(unregister_mlx_file_and_keep_old_data fs mlxfile);;
   
 
 let unregister_module fs hm=
    let desc=Modulesystem.descendants fs [hm] in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
         raise(Derelict_children(hm,temp1))
    else
    let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) (Modulesystem.all_filedata fs) in
    if opt=None 
    then raise(Non_registered_module(hm))  
    else
    Modulesystem.make(Modulesystem.root fs,
    	before@after,Modulesystem.main_toplevel_name fs );;
    
   
  
exception Already_registered_file of Mlx_filename.t;;  
exception Overcrowding of Mlx_filename.t*(Ocaml_ending.t list);;
exception Bad_pair of Mlx_filename.t*Ocaml_ending.t;; 
exception Name_conflict of Half_dressed_module.t * Half_dressed_module.t;; 
   
 
 let register_mlx_file_without_treating_inactivity fs mlx_file =
   let hm=Mlx_filename.half_dressed_core mlx_file
   and ending=Mlx_filename.ending mlx_file in 
   let nm=Half_dressed_module.undress hm in
   let all_data=Modulesystem.all_filedata fs in
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Half_dressed_module.undress(Modulesystem_data.name dt)=nm) 
      all_data in
   if opt=None
   then  let old_info=Read_info_on_file_in_system.complete_info all_data mlx_file None in
         let info1=Modulesystem_data.make_presence ending old_info in
         (*
         if a mll or mly file is being registered, the ml will automatically be created,
         so let us anticipate by already adding a ml presence
         *)
         let info=(if List.mem ending [Ocaml_ending.mll;Ocaml_ending.mly]
         then Modulesystem_data.make_ml_present info1 else info1) in
         (Modulesystem.make(
         	Modulesystem.root fs,
         	before@[info],
         	Modulesystem.main_toplevel_name fs),info)  
   else
   let old_dt=Option.unpack(opt) in
   let old_name=Modulesystem_data.name old_dt in
   if (old_name<>hm)
   then raise(Name_conflict(old_name,hm))
   else 
   let edgs=Modulesystem_data.registered_endings old_dt in
   if List.length(edgs)>1
   then  raise(Overcrowding(mlx_file,edgs))
   else  
   if List.mem ending edgs
   then raise(Already_registered_file(mlx_file))
   else
   if (not(List.mem Ocaml_ending.ml (ending::edgs)))
   then raise(Bad_pair(mlx_file,List.hd edgs))
   else 
   let dt1=Read_info_on_file_in_system.complete_info all_data mlx_file None in
   let new_dt=Modulesystem_data.make_presence ending dt1 in
   if ending<>Ocaml_ending.ml
   then let final_list=before@(new_dt::after) in
         (Modulesystem.make(
         Modulesystem.root fs,final_list,Modulesystem.main_toplevel_name fs),
          new_dt)
   else 
   let temp3=List.rev(Modulesystem_data.direct_fathers new_dt) in
   if temp3=[]
   then let final_list=before@(new_dt::after) in
         (Modulesystem.make(
         Modulesystem.root fs,final_list,
         Modulesystem.main_toplevel_name fs),new_dt)
   else  
   let last_father=List.hd(List.rev(Modulesystem_data.direct_fathers new_dt)) in
   let (before1,opt1,after1)=Three_parts.select_center_element  (fun dt->
           (Modulesystem_data.name dt)=last_father) before in
   let lf1=Option.unpack opt1  in    
   let temp2=Image.image (Modulesystem_data.update_anclibdir new_dt all_data) (after1@after) in
   let final_list=before1@(lf1::new_dt::temp2) in
   (Modulesystem.make(
         Modulesystem.root fs,final_list,Modulesystem.main_toplevel_name fs),
   new_dt);;
   
  let register_mlx_file fs mlx_file =
      let (fs2,new_dt)=register_mlx_file_without_treating_inactivity fs mlx_file in
      let dir=Modulesystem.root fs2
      and l=Modulesystem.all_filedata fs2
      and mtn=Modulesystem.main_toplevel_name fs2 
      and hm=Modulesystem_data.name new_dt in
      let all_ancestors=hm::(Modulesystem_data.all_ancestors new_dt) in
      let new_l=Image.image(
        fun dt->
          if List.mem (Modulesystem_data.name dt) all_ancestors
          then dt
          else Modulesystem_data.increment_inactivity_count dt
      ) l in
      Modulesystem.make(dir,new_l,mtn)
      ;;
  
  
  let try_to_register_mlx_file fs mlx_file=
    try(Some(register_mlx_file fs mlx_file)) with _->None;;  

   let try_to_register_mlx_files fs mlx_files=
   let rec tempf=(fun
    (vfs,failures,yet_untreated)->
      match yet_untreated with
      []->(failures,vfs)
      |mlx::others->
      (
        match try_to_register_mlx_file vfs mlx with
        None->tempf(vfs,mlx::failures,others)
        |Some(nfs)->tempf(nfs,failures,others)
      )
   ) in
   tempf(fs,[],mlx_files);;
  
   
  exception NonoptDependingOnOpt of Half_dressed_module.t*(Half_dressed_module.t list);;  
  exception Nonregistered_module of Half_dressed_module.t;;  
  
   
  let reposition fs hm (l_before,l_after)=
   let (before,opt,after)=Three_parts.select_center_element
   (fun dt->
      Modulesystem_data.name dt=hm) (Modulesystem.all_filedata fs) in
   if opt=None
   then raise(Nonregistered_module(hm))
   else 
   let info=Option.unpack opt 
   and amputated_fs=Modulesystem.make 
   (Modulesystem.root fs,before@after,Modulesystem.main_toplevel_name fs) in
   Arrange_positions_in_modulesystem.insert_data amputated_fs info (l_before,l_after);;  

exception Non_existent_mtime of Mlx_filename.t;;

let force_modification_time_update fs mlx=
   let hm=Mlx_filename.half_dressed_core mlx
   and edg=Mlx_filename.ending mlx in
   let (before,opt,after)=Three_parts.select_center_element  (fun dt->
      Modulesystem_data.name dt=hm) 
      (Modulesystem.all_filedata fs) in
   if opt=None
   then raise(Non_existent_mtime(mlx))
   else 
   let dt=Option.unpack opt in
   let dir=Modulesystem.root fs in
   let file=(Directory_name.to_string dir)^(Mlx_filename.to_string mlx) in
   let old_val=Modulesystem_data.modification_time dt edg 
   and new_val=(Unix.stat file).Unix.st_mtime  in
   if old_val=new_val
   then fs
   else let new_dt=Modulesystem_data.force_modification_time dt edg new_val in
        Modulesystem.make(Modulesystem.root fs,before@(new_dt::after),
        Modulesystem.main_toplevel_name fs);;

    
let recompute_module_info fs hm=
   let l=Modulesystem.all_filedata fs in
   let (before,_,after)=Three_parts.select_center_element(
      fun md->Modulesystem_data.name md=hm
   ) l in
  let new_md=Read_info_on_file_in_system.recompute_complete_info_for_module
       l hm in 
  Modulesystem.make(Modulesystem.root fs,before@(new_md::after),
        Modulesystem.main_toplevel_name fs)
   ;;
   

     
   
   