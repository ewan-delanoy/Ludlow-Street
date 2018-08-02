
(* 

#use"Makefile_makers/modify_md_list.ml";;


*)

let empty_one=Md_list_t.M(Small_array.of_list []);;

let find_module_registration (Md_list_t.M mdata) hm=
  Small_array.seek(fun a->Modulesystem_data.name a=hm) mdata;;   

module Private=struct

let debuggable_targets_from_ancestor_data dt=
        let hm=Modulesystem_data.name dt in
        if Modulesystem_data.mll_present dt
        then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
             [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
        else 
        if Modulesystem_data.mly_present dt
        then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
             [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
        else
        if Modulesystem_data.ml_present dt
        then 
             let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
             [ml_target;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
        else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
             [mli_target;Ocaml_target.cmi hm];;    
    
let immediate_ingredients_for_debuggable hm=
        [Ocaml_target.dcmo hm;Ocaml_target.debuggable hm];;  
    
end;;  

let debuggable_targets_from_ancestors wmdata ancestors=
    let temp1=Image.image (fun hm2->
           let opt2=find_module_registration wmdata hm2 in
           let dt2=Option.unpack opt2 in
           Private.debuggable_targets_from_ancestor_data dt2
         ) ancestors in
    Preserve_initial_ordering.preserve_initial_ordering temp1;;

let find_needed_data_for_file (Md_list_t.M mdata) fn=
      let temp1=Look_for_module_names.names_in_file fn in
      let selecter=(fun info->
        let hm=Modulesystem_data.name info in
        let name=Half_dressed_module.naked_module hm in
        if List.mem name temp1
        then Some(info)
        else None
      ) in
      Small_array.filter_and_unpack selecter mdata;; 

let find_needed_data wmdata mlx=
      let fn=Mlx_ended_absolute_path.to_path mlx in
      find_needed_data_for_file wmdata fn;;         

let ingredients_for_debuggable wmdata hm=
      let (Md_list_t.M mdata)=wmdata in
      let mlfile=Mlx_ended_absolute_path.join hm Ocaml_ending.Ml in
      let genealogy=find_needed_data wmdata mlfile in
      let dirfath=Image.image (Modulesystem_data.name) genealogy in
      let temp1=Image.image 
             (fun t->Tidel.diforchan(Modulesystem_data.all_ancestors t)) 
             genealogy in
       let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
       let tempf=(fun t->
                 let nam_t=Modulesystem_data.name t in
                 if Tidel.elfenn nam_t temp2
                 then Some(nam_t)
                 else None) in
       let allanc=Small_array.filter_and_unpack tempf mdata in
      (debuggable_targets_from_ancestors wmdata allanc)
      @(Private.immediate_ingredients_for_debuggable hm);; 

let all_modules (Md_list_t.M mdata)=
  Small_array.image Modulesystem_data.name mdata;; 


let usual_targets (Md_list_t.M mdata)=
  let temp1=Small_array.image Ocaml_target.from_modulesystem_data mdata in
  List.flatten temp1;;

let industrial_separator=Industrial_separator.alaskan_data;; 

let archive (Md_list_t.M mdata)=
    Nonblank.make(String.concat industrial_separator 
    (Small_array.image Modulesystem_data.archive mdata));;
     
let unarchive s=
    let v1=Str.split (Str.regexp_string industrial_separator) (Nonblank.decode(s)) in
    Md_list_t.M(
      Small_array.of_list(  
    Image.image Modulesystem_data.unarchive v1));;

exception Non_existent_mtime of Mlx_ended_absolute_path.t;;

let force_modification_time root_dir wmdata mlx=
      let (Md_list_t.M mdata)=wmdata in
      let hm=Mlx_ended_absolute_path.half_dressed_core mlx
      and edg=Mlx_ended_absolute_path.ending mlx in
      let idx=
        (try Small_array.leftmost_index_of_property_in
          (fun dt->
         Modulesystem_data.name dt=hm) mdata with 
        _->raise(Non_existent_mtime(mlx)) )in
      let dt=Small_array.get mdata idx   in
      let file=(Root_directory.connectable_to_subpath root_dir)^
               (Mlx_ended_absolute_path.to_string mlx) in
      let old_val=Modulesystem_data.modification_time dt edg 
      and new_val=(Unix.stat file).Unix.st_mtime  in
      if old_val!=new_val
      then wmdata
      else let new_dt=Modulesystem_data.force_modification_time dt edg new_val in
            let _=(Small_array.set mdata idx new_dt) in 
            Md_list_t.M mdata;;

let everyone_except_the_debugger wmdata=
        let (Md_list_t.M mdata)=wmdata in
        let temp3=Small_array.image Modulesystem_data.name mdata in
        let temp4=List.filter (fun hm->
           Half_dressed_module.uprooted_version(hm)<>
            Coma_constant.name_for_debugged_module
        ) temp3 in
        temp4;;      
        
exception Non_registered_module of Half_dressed_module.t;;  
exception Derelict_children of Half_dressed_module.t*(Half_dressed_module.t list);;  
           
            
let unregister_module_on_monitored_modules wmdata hm=
  let (Md_list_t.M mdata)=wmdata in
  let desc=List.filter(
      fun dt->List.mem hm (Modulesystem_data.all_ancestors dt)
  ) (Small_array.to_list mdata) in
   if desc<>[]
   then let temp1=Image.image Modulesystem_data.name desc in
        raise(Derelict_children(hm,temp1))
   else
   let idx=
    (try Small_array.leftmost_index_of_property_in
      (fun dt->
     Modulesystem_data.name dt=hm) mdata with 
    _->raise(Non_registered_module(hm)) )in
   let dt=Small_array.get mdata idx in 
   let _=Small_array.remove_item_at_index mdata idx in
   let acolytes=Modulesystem_data.acolytes dt  in
   let short_paths=Image.image Mlx_ended_absolute_path.short_path acolytes in
   (Md_list_t.M(mdata),short_paths);;     
                    

exception Non_registered_file of Mlx_ended_absolute_path.t;;  
exception Abandoned_children of Mlx_ended_absolute_path.t*(Half_dressed_module.t list);;
                      
                     
let unregister_mlx_file_on_monitored_modules wmdata mlxfile=
    let (Md_list_t.M mdata)=wmdata in
    let hm=Mlx_ended_absolute_path.half_dressed_core mlxfile in
    let desc=List.filter(
          fun dt->List.mem hm (Modulesystem_data.all_ancestors dt)
  ) (Small_array.to_list mdata) in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
          raise(Abandoned_children(mlxfile,temp1))
    else
        let idx=
        (try Small_array.leftmost_index_of_property_in
          (fun dt->
        Modulesystem_data.name dt=hm) mdata with 
        _->raise(Non_registered_file(mlxfile)) ) in
        let dt=Small_array.get mdata idx in 
        let edg=Mlx_ended_absolute_path.ending mlxfile in
        if (not(Modulesystem_data.check_presence edg dt))
        then raise(Non_registered_file(mlxfile))
        else 
        let new_dt=Modulesystem_data.make_absence edg dt in
        if (Modulesystem_data.registered_endings new_dt)=[]
        then let _=Small_array.remove_item_at_index mdata idx in
             Md_list_t.M mdata
        else let _=Small_array.set mdata idx new_dt in
             Md_list_t.M mdata;;
            


let compute_subdirectories_list wmdata=
  let (Md_list_t.M mdata)=wmdata in
  let temp1=Small_array.image (
      fun md->
      let hm=Modulesystem_data.name md in
        Subdirectory.without_trailing_slash(Half_dressed_module.subdirectory hm)
    ) mdata in
    let temp2=Ordered_string.diforchan temp1 in
    let temp3=Ordered_string.forget_order temp2 in
    Image.image Subdirectory.of_string temp3;;

let  check_presences wmdata hm=
    let (Md_list_t.M mdata)=wmdata in
    match Small_array.seek (fun a->Modulesystem_data.name a=hm) mdata with
      None->Ocaml_ending.exhaustive_uple (fun _->false)
    |Some(dt)->Ocaml_ending.exhaustive_uple 
      (fun edg->Modulesystem_data.check_presence edg dt);;

module PrivateTwo=struct



let find_needed_names mdata mlx=
  let temp1=find_needed_data mdata mlx in
  Image.image Modulesystem_data.name temp1;;  

let find_needed_libraries mlx genealogy=
  let fn=Mlx_ended_absolute_path.to_path mlx in
  let temp1=Look_for_module_names.names_in_file fn in
  List.filter
  (
    fun lib->
      if List.exists 
         (fun mdl->List.mem(Naked_module.of_string mdl)(temp1))
           (Ocaml_library.modules_telling_a_library_away lib)
      then true
      else List.exists 
           (fun info->List.mem lib (Modulesystem_data.needed_libraries info) ) 
           genealogy
  )
  Ocaml_library.all_libraries;;


let find_needed_directories mlx genealogy=
  let temp1=Image.image 
    (fun t->Tidel.diforchan(Modulesystem_data.needed_directories t)) 
      genealogy in
  let s_mlx=Mlx_ended_absolute_path.to_string mlx in
  let temp2=(fun bowl->
      if bowl 
      then let new_subdir=Subdirectory.of_string(Father_and_son.father s_mlx '/') in
           Tidel.singleton(new_subdir)::temp1
      else temp1
  )(String.contains s_mlx '/') in    
  let temp3=Tidel.big_teuzin temp2 in
  Ordered.forget_order temp3;;
              
                    
end;;  


let complete_info wmdata  mlx=
  let (Md_list_t.M mdata)=wmdata in 
  let (hm,edg)=Mlx_ended_absolute_path.decompose mlx in
  let genealogy=find_needed_data wmdata mlx in
  let (mlp,mlip,mllp,mlyp)=check_presences wmdata hm
  and (mlmt,mlimt,mllmt,mlymt)=Modulesystem_data.compute_modification_times hm in
  let acrep=Acolyte_repartition.from_presences (mlp,mlip,mllp,mlyp) in
  let pr_end=Acolyte_repartition.principal_ending acrep in
  let prmt=Modulesystem_data.associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
  let dirfath=Image.image (Modulesystem_data.name) genealogy in
  let temp1=Image.image 
        (fun t->Tidel.diforchan(Modulesystem_data.all_ancestors t)) 
        genealogy in
  let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
  let tempf=(fun t->
            let nam_t=Modulesystem_data.name t in
            if Tidel.elfenn nam_t temp2
            then Some(nam_t)
            else None) in
  let allanc=Small_array.filter_and_unpack tempf mdata in
  let libned=PrivateTwo.find_needed_libraries mlx genealogy
  and dirned=PrivateTwo.find_needed_directories mlx genealogy in
  Modulesystem_data.make
  (hm,acrep,mlip,prmt,mlimt,libned,dirfath,allanc,dirned);;

  let check_unix_presence hm edg=
    let (_,dir)=Half_dressed_module.unveil hm in
    let s_hm=Half_dressed_module.uprooted_version hm 
    and s_dir=Root_directory.connectable_to_subpath dir in
    Sys.file_exists(s_dir^s_hm^(Ocaml_ending.to_string edg));;

let  check_unix_presences hm=
    Ocaml_ending.exhaustive_uple (fun edg->check_unix_presence hm edg);;  

let complete_info_during_registration wmdata  mlx=
    let (Md_list_t.M mdata)=wmdata in 
    let (hm,edg)=Mlx_ended_absolute_path.decompose mlx in
    let genealogy=find_needed_data wmdata mlx in
    let (mlp,mlip,mllp,mlyp)=check_presences wmdata hm
    and (mlmt,mlimt,mllmt,mlymt)=Modulesystem_data.compute_modification_times hm in
    let acrep=Acolyte_repartition.from_presences (mlp,mlip,mllp,mlyp) in
    let pr_end=Acolyte_repartition.principal_ending acrep in
    let prmt=Modulesystem_data.associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
    let dirfath=Image.image (Modulesystem_data.name) genealogy in
    let temp1=Image.image 
          (fun t->Tidel.diforchan(Modulesystem_data.all_ancestors t)) 
          genealogy in
    let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
    let tempf=(fun t->
              let nam_t=Modulesystem_data.name t in
              if Tidel.elfenn nam_t temp2
              then Some(nam_t)
              else None) in
    let allanc=Small_array.filter_and_unpack tempf mdata in
    let libned=PrivateTwo.find_needed_libraries mlx genealogy
    and dirned=PrivateTwo.find_needed_directories mlx genealogy in
    Modulesystem_data.make
    (hm,acrep,mlip,prmt,mlimt,libned,dirfath,allanc,dirned);;
  
  
  

exception Nonregistered_module of Half_dressed_module.t;;



let rename_module_on_monitored_modules root_dir wmdata old_name new_name=
  let (Md_list_t.M mdata)=wmdata in
  let interm_list=Small_array.image
  (Abstract_renamer.abstractify old_name) mdata in
  let opt=find_module_registration wmdata old_name in
  if opt=None
  then raise(Nonregistered_module(old_name))
  else 
  let old_dt=Option.unpack opt in
  let old_acolytes=Modulesystem_data.acolytes old_dt in
  let old_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) old_acolytes in 
  let new_acolytes=Image.image (fun mlx->Mlx_ended_absolute_path.do_file_renaming mlx new_name) old_acolytes in
  let new_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) new_acolytes in 
  let new_hm=Mlx_ended_absolute_path.half_dressed_core(List.hd new_acolytes) in
  let old_mname=Half_dressed_module.naked_module old_name
  and new_mname=Half_dressed_module.naked_module new_hm
  in
  let changer=Look_for_module_names.change_module_name_in_file
  old_mname new_mname in
  let temp1=Small_array.filter_and_unpack(
    fun dt->
     if List.mem old_name
    (Modulesystem_data.all_ancestors dt)
    then Some(Modulesystem_data.acolytes dt)
    else None
  ) mdata in
  let temp2=List.flatten temp1 in
  let temp3=Image.image Mlx_ended_absolute_path.to_path temp2 in
  let temp4=Option.filter_and_unpack (
    fun s->try Some(Absolute_path.of_string s) with _->None
  ) [
      Coma_constant.name_for_printersfile;
    ] in
  
  let _=Image.image changer (temp3@temp4) in
  let s_root=Root_directory.connectable_to_subpath root_dir in     
  let _=Unix_command.uc
      ("rm -f "^s_root^"_build/"^(Half_dressed_module.uprooted_version old_name)^".cm* ") in
  let new_list=Small_array.of_list(Image.image
  (Abstract_renamer.unabstractify new_hm) interm_list) in
  (Md_list_t.M(new_list),(old_files,new_files));;


let recompute_complete_info_for_module wmdata hm=
      let opt=find_module_registration wmdata hm in
      let dt=Option.unpack opt in
      let edg=List.hd(Modulesystem_data.registered_endings dt) in
      let mlx=Mlx_ended_absolute_path.join hm edg in
      complete_info wmdata mlx;;

let recompute_module_info wmdata hm=
  let (Md_list_t.M mdata)=wmdata in
  let idx=Small_array.leftmost_index_of_property_in
      (fun dt->
    Modulesystem_data.name dt=hm) mdata   in
    let new_dt=recompute_complete_info_for_module wmdata hm in 
    let _=Small_array.set mdata idx new_dt in
    Md_list_t.M mdata;;  




exception Nonregistered_module_during_relocation of Half_dressed_module.t;;  
          
let relocate_module_on_monitored_modules root_dir wmdata old_name new_subdir=
  let (Md_list_t.M mdata)=wmdata in
  let idx=
    (try Small_array.leftmost_index_of_property_in
      (fun dt->
    Modulesystem_data.name dt=old_name) mdata with 
    _->raise(Nonregistered_module_during_relocation(old_name)) ) in
    let old_dt=Small_array.get mdata idx in   
    let old_acolytes=Modulesystem_data.acolytes old_dt in
    let old_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) old_acolytes in 
    let new_acolytes=Image.image (fun mlx->Mlx_ended_absolute_path.do_file_displacing mlx new_subdir) old_acolytes in
    let new_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) new_acolytes in 
    let new_name=Mlx_ended_absolute_path.half_dressed_core(List.hd new_acolytes) in
    let data_renamer=Modulesystem_data.rename (old_name,new_name) in
    let s_root=Root_directory.connectable_to_subpath root_dir in     
    let _=Unix_command.uc
     ("rm -f "^s_root^"_build/"^(Half_dressed_module.uprooted_version old_name)^".cm* ") in
    let _=Small_array.apply_transformation_on_interval
       mdata data_renamer idx (Small_array.size mdata) in
    (Md_list_t.M mdata,(old_files,new_files));;



let above wmdata hm=
  let (Md_list_t.M mdata)=wmdata in
  match Small_array.seek(fun dt->Modulesystem_data.name dt=hm) mdata with
  None->raise(Non_registered_module(hm))
  |Some(dt)->Modulesystem_data.all_ancestors dt;;

let below wmdata hm=
  let (Md_list_t.M mdata)=wmdata in
  Small_array.filter_and_unpack(fun dt->
      if List.mem hm (Modulesystem_data.all_ancestors dt)
      then Some(Modulesystem_data.name dt)
      else None) mdata;;  

let directly_below wmdata hm=
  let (Md_list_t.M mdata)=wmdata in
  Small_array.filter_and_unpack(fun dt->
        if List.mem hm (Modulesystem_data.direct_fathers dt)
        then Some(Modulesystem_data.name dt)
        else None) mdata;;
               

let all_mlx_files wmdata=
  let (Md_list_t.M mdata)=wmdata in
        List.flatten
        (Small_array.image Modulesystem_data.acolytes mdata);; 
      
let all_mlx_paths wmdata=Image.image Mlx_ended_absolute_path.to_absolute_path 
        (all_mlx_files wmdata);;  

let all_short_paths wmdata=
  let (Md_list_t.M mdata)=wmdata in
  List.flatten(
    Small_array.image Modulesystem_data.short_paths mdata
  );;

let files_containing_string wmdata some_string=
let temp1=all_mlx_paths wmdata in
List.filter (fun ap->Substring.is_a_substring_of 
  some_string (Io.read_whole_file ap)) temp1;;


let system_size (Md_list_t.M mdata)=Small_array.size(mdata);;

exception Inconsistent_constraints of Half_dressed_module.t*Half_dressed_module.t;;
exception Bad_upper_constraint of Half_dressed_module.t;;  


exception Nonregistered_module_during_reposition of Half_dressed_module.t;;  

 
let reposition_module wmdata hm (l_before,l_after)=
    let (Md_list_t.M mdata)=wmdata in  
    let find_idx=(fun h->Small_array.leftmost_index_of_property_in (
       fun dt->Modulesystem_data.name dt=h
    ) mdata) in
    let main_idx=find_idx hm
    and indices_before=Image.image find_idx l_before
    and indices_after=Image.image find_idx l_after in
    let max_before=(if indices_before=[] then 1 else Max.list indices_before)
    and min_after=(if indices_after=[] then Small_array.size mdata else Min.list indices_after)
    in
    if max_before>min_after
    then let hm_before=Modulesystem_data.name(Small_array.get mdata max_before)
         and hm_after=Modulesystem_data.name(Small_array.get mdata min_after) in
         raise(Inconsistent_constraints(hm_before,hm_after))
    else 
    if max_before>main_idx
    then let hm_before=Modulesystem_data.name(Small_array.get mdata max_before) in
         raise(Bad_upper_constraint(hm_before))
    else 
    let _=Small_array.reposition_by_putting_snd_immediately_after_fst
       mdata max_before main_idx in 
    Md_list_t.M mdata;;  

let rename_directory_on_data (old_subdir,new_subdirname) wmdata=
  let (Md_list_t.M mdata)=wmdata in  
  let _=Small_array.apply_transformation_on_all mdata
  (Modulesystem_data.rename_endsubdirectory (old_subdir,new_subdirname)) 
  in 
  Md_list_t.M mdata;;

let find_value_definition wmdata s=
  let (Md_list_t.M mdata)=wmdata in 
  if not(String.contains s '.')
  then None
  else
  let j1=String.index(s)('.')+1 in
  let module_name=Cull_string.beginning (j1-1) s in
  let opt=Small_array.seek (fun md->
  Half_dressed_module.naked_module(Modulesystem_data.name md)=
  Naked_module.of_string(String.uncapitalize_ascii(module_name)) ) mdata in
  if opt=None
  then None 
  else
  let md1=Option.unpack opt in
  let hm1=Modulesystem_data.name md1 in
  let ap1=Mlx_ended_absolute_path.to_path(Mlx_ended_absolute_path.join hm1 
     Ocaml_ending.Ml) in
  let temp1=Read_ocaml_files.read_ocaml_files [ap1] in	 
  Option.seek (
     fun itm->Ocaml_gsyntax_item.name(itm)=s
  ) temp1;;
  
let find_naked_module_registration wmdata nm=
    let (Md_list_t.M mdata)=wmdata in 
    Small_array.seek (fun md->
      let hm=Modulesystem_data.name md in
      (Half_dressed_module.naked_module hm)=nm
    )
    mdata;;

let all_naked_modules wmdata=
  let (Md_list_t.M mdata)=wmdata in   
  Small_array.image (fun md->
    Naked_module.to_string(
    Half_dressed_module.naked_module(Modulesystem_data.name md))
  ) mdata;;     

let all_ml_absolute_paths wmdata=
  let (Md_list_t.M mdata)=wmdata in   
Small_array.filter_and_unpack (fun md->
  if not(Modulesystem_data.ml_present md)
  then None
  else 
  let hm=Modulesystem_data.name md in
  let mlx=Mlx_ended_absolute_path.join hm Ocaml_ending.ml in
  Some(Mlx_ended_absolute_path.to_absolute_path mlx)
) mdata;;

let modules_using_value wmdata value_name =
  let (Md_list_t.M mdata)=wmdata in  
  Small_array.filter_and_unpack (fun md->
   let ap=Modulesystem_data.principal_path md in
   if Substring.is_a_substring_of 
       value_name (Io.read_whole_file ap)
   then Some(Modulesystem_data.name md)
   else None ) mdata;;

module Private_for_ancs_libs_and_dirs=struct      

    let moduledata_hshtbl=Hashtbl.create 500;;

    exception Iterator_for_update_exn;; 
    
    let iterator_for_update (graet,da_ober)=match da_ober with
      []->raise(Iterator_for_update_exn)
      |(md,atoms_for_md)::peurrest->
         let hm=Modulesystem_data.name md 
         and mlx=Modulesystem_data.principal_mlx md in 
         let new_ancestor_names=Image.image Modulesystem_data.name atoms_for_md in
         let genealogy=Image.image (Hashtbl.find moduledata_hshtbl) new_ancestor_names in
         let new_libs=PrivateTwo.find_needed_libraries mlx genealogy
         and new_dirs=PrivateTwo.find_needed_directories mlx genealogy in
         let new_md=
         {
          Modulesystem_data.name=md.Modulesystem_data.name;
            acolyte_repartition=md.Modulesystem_data.acolyte_repartition;
            mli_present=md.Modulesystem_data.mli_present;
            principal_modification_time=md.Modulesystem_data.principal_modification_time;
            mli_modification_time=md.Modulesystem_data.mli_modification_time;
            needed_libraries=new_libs;
            direct_fathers=md.Modulesystem_data.direct_fathers;
            all_ancestors=new_ancestor_names;
            needed_directories=new_dirs;
         } in
         let _=Hashtbl.add moduledata_hshtbl hm new_md in
         (new_md::graet,peurrest);;
         
    let rec computer_for_update (graet,da_ober)=
      if da_ober=[]
      then List.rev(graet)
      else computer_for_update(iterator_for_update (graet,da_ober));;   
end;;      

let update_ancs_libs_and_dirs l=Private_for_ancs_libs_and_dirs.computer_for_update 
 ([],l);;
    
let quick_update wmdata x=
  let hm=Modulesystem_data.name (x) in
  if (Half_dressed_module.uprooted_version hm)=Coma_constant.name_for_debugged_module
  then None
  else
  let new_values=Modulesystem_data.compute_modification_times hm 
  and old_values=Modulesystem_data.modification_times x in
  if old_values=new_values
  then None
  else
  let (n_ml,n_mli,n_mll,n_mly)=new_values in
  let edg=List.hd(Modulesystem_data.registered_endings x) in
  let mlx=Mlx_ended_absolute_path.join hm edg in
  let fathers=PrivateTwo.find_needed_names wmdata mlx in
  let acrep=x.Modulesystem_data.acolyte_repartition in
  let pr_end=Acolyte_repartition.principal_ending acrep in
  let n_pr=Modulesystem_data.associated_modification_time (n_ml,n_mli,n_mll,n_mly) pr_end in

  Some(
  {
    Modulesystem_data.name=x.Modulesystem_data.name;
    acolyte_repartition =acrep;
    mli_present=x.Modulesystem_data.mli_present;
    principal_modification_time=n_pr;
    mli_modification_time=n_mli;
    needed_libraries=x.Modulesystem_data.needed_libraries;
    direct_fathers=fathers;
    all_ancestors=x.Modulesystem_data.all_ancestors;
    needed_directories=x.Modulesystem_data.needed_directories;
   }   
   )   
  ;;
    


module PrivateThree=struct

    let message_about_circular_dependencies printer cycles= 
      if cycles=[]
      then ""
      else
      let temp1=Image.image(fun cycle->
        let ttemp1=Image.image printer cycle in
         String.concat " -> " ttemp1 
      ) cycles in
      let temp2=String.concat "\n\n" temp1 in
      temp2;;
    
    exception Circular_dependencies of string;;
    
    let treat_circular_dependencies tolerate_cycles printer cycles=
      if cycles=[]
      then ()
      else let msg=message_about_circular_dependencies printer cycles in  
           if tolerate_cycles
           then (print_string msg;flush stdout)     
           else raise(Circular_dependencies(msg));; 
           
    let message_about_changed_modules changed_modules=
      let temp1=Image.image Half_dressed_module.uprooted_version changed_modules in
      "\n\n\n"^
      "The following modules have been directly changed :\n"^
      (String.concat "\n" temp1)^
      "\n\n\n"
    ;;       
           
    let announce_changed_modules changed_modules=
      if changed_modules=[]
      then ()
      else (print_string(message_about_changed_modules changed_modules);flush stdout);;
             
    
    let put_md_list_back_in_order tolerate_cycles 
      wmd_list initially_active_hms=
      let (Md_list_t.M smd_list)=wmd_list in  
      let md_list=Small_array.to_list smd_list in
      let coat=Memoized.make (fun md->
        let anc_md=Modulesystem_data.direct_fathers(md) in
        List.filter (
          fun md2->List.mem(Modulesystem_data.name md2) anc_md
        ) md_list
      ) in
      let (cycles,old_list)=Reconstruct_linear_poset.reconstruct_linear_poset 
         coat md_list in
      let _=treat_circular_dependencies tolerate_cycles
           (fun md->Half_dressed_module.uprooted_version(Modulesystem_data.name md)) 
           cycles in     
      let final_list=update_ancs_libs_and_dirs old_list in 
      let active_descendants=Option.filter_and_unpack (
          fun md->
            let hm=Modulesystem_data.name md in
            if List.mem hm initially_active_hms
            then Some(hm)
            else
            if List.exists (fun hm2->List.mem hm2 initially_active_hms) 
                 (Modulesystem_data.all_ancestors md)
            then Some(hm)
            else None
      ) final_list in  
      (Md_list_t.M (Small_array.of_list final_list),active_descendants);;
     
end;; 
     
let recompile_on_monitored_modules tolerate_cycles wmdata =
  let (Md_list_t.M mdata)=wmdata in    
  let ref_for_changed_modules=ref[] 
  and ref_for_changed_shortpaths=ref[] in
  let declare_changed=(fun md->
    let hm=Modulesystem_data.name md in
    ref_for_changed_modules:=hm::(!ref_for_changed_modules);
    ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                        (Modulesystem_data.short_paths md))
    ) in
  let _=Small_array.apply_transformation_on_all mdata (fun md->
    match quick_update wmdata md with
    None->md
    |Some(new_md)->let _=declare_changed(new_md) in
                  new_md
) in
let changed_modules=List.rev(!ref_for_changed_modules) in
if changed_modules=[] then ((wmdata,[]),[]) else
let _=PrivateThree.announce_changed_modules changed_modules in
(PrivateThree.put_md_list_back_in_order tolerate_cycles 
  wmdata changed_modules,
(!ref_for_changed_shortpaths));;  

let printer_equipped_types_from_data wmdata=
  let (Md_list_t.M mdata)=wmdata in 
  Small_array.filter_and_unpack (
    fun md->
    let hm=Modulesystem_data.name md
    and ap=Modulesystem_data.principal_path md in
    let text=Io.read_whole_file ap in
    if (Substring.is_a_substring_of ("let "^"print_out ") text)
    then Some(hm)
    else None
  ) mdata;;

let update_anclibdir changer mdata x=
    if not(List.mem (Modulesystem_data.name changer)
       (Modulesystem_data.all_ancestors x))
    then x
    else 
    let anc=Modulesystem_data.all_ancestors changer
    and llib=Modulesystem_data.needed_libraries changer in
    let anc_x=Modulesystem_data.all_ancestors x
    and llib_x=Modulesystem_data.needed_libraries x in
    let new_ancestors=Small_array.filter_and_unpack(
      fun fd->
        let hm=Modulesystem_data.name fd in
        if (List.mem hm anc_x)||(List.mem hm anc)
        then Some(hm)
        else None
    ) mdata in
    let new_lib=List.filter (
       fun lib->(List.mem lib llib)||(List.mem lib llib_x)
    ) Ocaml_library.all_libraries in
    let temp1=Option.filter_and_unpack(
      fun hm->
        let s_hm=Half_dressed_module.uprooted_version hm in
        let s_dir=Father_and_son.father s_hm '/' in
        if s_dir="" then None else
        Some(Subdirectory.of_string s_dir)
    )  new_ancestors in
   let new_dir=Ordered.forget_order(Tidel.diforchan temp1) in
   Modulesystem_data.tool_in_update_anclibdir 
     (new_ancestors,new_lib,new_dir) x;;      



exception Already_registered_file of Mlx_ended_absolute_path.t;;  
exception Overcrowding of Mlx_ended_absolute_path.t*(Ocaml_ending.t list);;
exception Bad_pair of Mlx_ended_absolute_path.t*Ocaml_ending.t;; 


let register_mlx_file_on_monitored_modules wmdata mlx_file =
          let (Md_list_t.M mdata)=wmdata in 
          let hm=Mlx_ended_absolute_path.half_dressed_core mlx_file
          and ending=Mlx_ended_absolute_path.ending mlx_file in 
          let opt_idx=(try 
           Some(Small_array.leftmost_index_of_property_in
              (fun dt->
             Modulesystem_data.name dt=hm) mdata) with 
            _->None ) in
          if opt_idx=None
          then  let old_info=complete_info_during_registration wmdata mlx_file in
                let info1=Modulesystem_data.make_presence ending old_info in
                (*
                if a mll or mly file is being registered, the ml will automatically be created,
                so let us anticipate by already adding a ml presence
                *)
                let info=(if List.mem ending [Ocaml_ending.mll;Ocaml_ending.mly]
                          then Modulesystem_data.make_ml_present info1 
                          else info1) in
                let _=Small_array.push_right mdata info in         
                Md_list_t.M(mdata)
          else
          let idx=Option.unpack(opt_idx) in
          let old_dt=Small_array.get mdata idx in
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
          let dt1=complete_info wmdata mlx_file in
          let new_dt=Modulesystem_data.make_presence ending dt1 in
          if ending<>Ocaml_ending.ml
          then let _=Small_array.set mdata idx new_dt in         
               Md_list_t.M(mdata)
          else 
          let temp3=List.rev(Modulesystem_data.direct_fathers new_dt) in
          if temp3=[]
          then let _=Small_array.set mdata idx new_dt in         
               Md_list_t.M(mdata)
          else  
          let last_father=List.hd(temp3) in
          let last_father_idx=Small_array.leftmost_index_of_property_in (
            fun dt->
            (Modulesystem_data.name dt)=last_father
          ) mdata in
          let _=
            (
              Small_array.apply_transformation_on_rightmost_interval
                   mdata (update_anclibdir new_dt mdata) (last_father_idx+1);
              Small_array.push_immediately_after_idx mdata new_dt last_father_idx;  
            )
          in
          Md_list_t.M mdata;;


