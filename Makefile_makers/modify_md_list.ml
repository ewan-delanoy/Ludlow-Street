
(* 

#use"Makefile_makers/modify_md_list.ml";;


*)

let find_module_registration mdata hm=
  Option.seek(fun a->Modulesystem_data.name a=hm) mdata;;   

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

let debuggable_targets_from_ancestors mdata ancestors=
    let temp1=Image.image (fun hm2->
           let opt2=find_module_registration mdata hm2 in
           let dt2=Option.unpack opt2 in
           Private.debuggable_targets_from_ancestor_data dt2
         ) ancestors in
    Preserve_initial_ordering.preserve_initial_ordering temp1;;

let find_needed_data_for_file mdata fn=
      let temp1=Look_for_module_names.names_in_file fn in
      let selecter=(fun info->
        let hm=Modulesystem_data.name info in
        let name=Half_dressed_module.naked_module hm in
        if List.mem name temp1
        then Some(info)
        else None
      ) in
      Option.filter_and_unpack selecter mdata;; 

let find_needed_data mdata mlx=
      let fn=Mlx_ended_absolute_path.to_path mlx in
      find_needed_data_for_file mdata fn;;         

let ingredients_for_debuggable mdata hm=
      let mlfile=Mlx_ended_absolute_path.join hm Ocaml_ending.Ml in
      let genealogy=find_needed_data mdata mlfile in
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
       let allanc=Option.filter_and_unpack tempf mdata in
      (debuggable_targets_from_ancestors mdata allanc)
      @(Private.immediate_ingredients_for_debuggable hm);; 

let all_modules mdata=Image.image Modulesystem_data.name mdata;; 

let usual_targets mdata=
  let temp1=Image.image Ocaml_target.from_modulesystem_data mdata in
  List.flatten temp1;;

let industrial_separator=Industrial_separator.alaskan_data;; 

let archive mdata=
    Nonblank.make(String.concat industrial_separator 
    (Image.image Modulesystem_data.archive mdata));;
     
let unarchive s=
    let v1=Str.split (Str.regexp_string industrial_separator) (Nonblank.decode(s)) in
    Image.image Modulesystem_data.unarchive v1;;

exception Non_existent_mtime of Mlx_ended_absolute_path.t;;

let force_modification_time root_dir mdata mlx=
      let hm=Mlx_ended_absolute_path.half_dressed_core mlx
      and edg=Mlx_ended_absolute_path.ending mlx in
      let (before,opt,after)=Three_parts.select_center_element  (fun dt->
         Modulesystem_data.name dt=hm) mdata in
      if opt=None
      then raise(Non_existent_mtime(mlx))
      else 
      let dt=Option.unpack opt in
      let file=(Root_directory.connectable_to_subpath root_dir)^(Mlx_ended_absolute_path.to_string mlx) in
      let old_val=Modulesystem_data.modification_time dt edg 
      and new_val=(Unix.stat file).Unix.st_mtime  in
      if old_val=new_val
      then mdata
      else let new_dt=Modulesystem_data.force_modification_time dt edg new_val in
      before@(new_dt::after);;

let everyone_except_the_debugger mdata=
        let temp3=Image.image Modulesystem_data.name mdata in
        let temp4=List.filter (fun hm->
           Half_dressed_module.uprooted_version(hm)<>Debugged_name.debugger_name
        ) temp3 in
        temp4;;      
        
exception Non_registered_module of Half_dressed_module.t;;  
exception Derelict_children of Half_dressed_module.t*(Half_dressed_module.t list);;  
           
            
let unregister_module_on_monitored_modules mdata hm=
              let desc=List.filter(
                  fun dt->List.mem hm (Modulesystem_data.all_ancestors dt)
              ) mdata in
               if desc<>[]
               then let temp1=Image.image Modulesystem_data.name desc in
                    raise(Derelict_children(hm,temp1))
               else
               let (before,opt,after)=Three_parts.select_center_element  (fun dt->
                 Modulesystem_data.name dt=hm) mdata in
               if opt=None 
               then raise(Non_registered_module(hm))  
               else 
               let acolytes=Modulesystem_data.acolytes(Option.unpack opt) in
               let short_paths=Image.image Mlx_ended_absolute_path.short_path acolytes in
               (before@after,short_paths);;           

exception Non_registered_file of Mlx_ended_absolute_path.t;;  
exception Abandoned_children of Mlx_ended_absolute_path.t*(Half_dressed_module.t list);;
                      
                     
let unregister_mlx_file_on_monitored_modules mdata mlxfile=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlxfile in
    let desc=List.filter(
          fun dt->List.mem hm (Modulesystem_data.all_ancestors dt)
    ) mdata in
    if desc<>[]
    then let temp1=Image.image Modulesystem_data.name desc in
          raise(Abandoned_children(mlxfile,temp1))
    else
                         let (before,opt,after)=Three_parts.select_center_element  (fun dt->
                           Modulesystem_data.name dt=hm) mdata in
                         match opt with
                          None->raise(Non_registered_file(mlxfile))
                         |Some(dt)->
                           let edg=Mlx_ended_absolute_path.ending mlxfile in
                           if (not(Modulesystem_data.check_presence edg dt))
                           then raise(Non_registered_file(mlxfile))
                           else 
                           let new_dt=Modulesystem_data.make_absence edg dt in
                           if (Modulesystem_data.registered_endings new_dt)=[]
                           then before@after
                           else before@(new_dt::after);;


let compute_subdirectories_list mdata=
                let temp1=Image.image (
                    fun md->
                     let hm=Modulesystem_data.name md in
                     Subdirectory.without_trailing_slash(Half_dressed_module.subdirectory hm)
                ) mdata in
                let temp2=Ordered_string.diforchan temp1 in
                let temp3=Ordered_string.forget_order temp2 in
                Image.image Subdirectory.of_string temp3;;

let check_presences mdata hm=
    match Option.seek (fun a->Modulesystem_data.name a=hm) mdata with
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

let complete_info mdata  mlx=
  let (hm,edg)=Mlx_ended_absolute_path.decompose mlx in
  let genealogy=find_needed_data mdata mlx in
  let (mlp,mlip,mllp,mlyp)=check_presences mdata hm
  and (mlmt,mlimt,mllmt,mlymt)=Modulesystem_data.compute_modification_times hm in
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
  let allanc=Option.filter_and_unpack tempf mdata in
  let libned=PrivateTwo.find_needed_libraries mlx genealogy
  and dirned=PrivateTwo.find_needed_directories mlx genealogy in
  Modulesystem_data.make
  (hm,mlp,mlip,mllp,mlyp,mlmt,mlimt,mllmt,mlymt,libned,dirfath,allanc,dirned);;




exception Nonregistered_module of Half_dressed_module.t;;

let descendants mdata names=
  let temp1=List.filter(
    fun dt->List.exists (fun t->List.mem t names) 
      (Modulesystem_data.all_ancestors dt)
  ) mdata in
  temp1;;

let rename_module_on_monitored_modules mdata old_name new_name=
  let interm_list=Image.image
  (Abstract_renamer.abstractify old_name) mdata in
  let opt=find_module_registration mdata old_name in
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
  let desc=descendants mdata [old_name] in
  let temp1=Image.image Modulesystem_data.acolytes desc in
  let temp2=List.flatten temp1 in
  let temp3=Image.image Mlx_ended_absolute_path.to_path temp2 in
  let temp4=Option.filter_and_unpack (
    fun s->try Some(Absolute_path.of_string s) with _->None
  ) [
      Coma_constant.name_for_printersfile;
    ] in
  
  let _=Image.image changer (temp3@temp4) in
  let s_root=Root_directory.connectable_to_subpath(German_constant.root) in     
  let _=Unix_command.uc
      ("rm -f "^s_root^"_build/"^(Half_dressed_module.uprooted_version old_name)^".cm* ") in
  let new_list=Image.image
  (Abstract_renamer.unabstractify new_hm) interm_list in
  (new_list,(old_files,new_files));;


let recompute_complete_info_for_module mdata hm=
      let opt=find_module_registration mdata hm in
      let dt=Option.unpack opt in
      let edg=List.hd(Modulesystem_data.registered_endings dt) in
      let mlx=Mlx_ended_absolute_path.join hm edg in
      complete_info mdata mlx;;

let recompute_module_info mdata hm=
    let (before,_,after)=Three_parts.select_center_element(
                         fun md->Modulesystem_data.name md=hm
    ) mdata in
    let new_md=recompute_complete_info_for_module mdata hm in 
    before@(new_md::after);;  




exception Nonregistered_module_during_relocation of Half_dressed_module.t;;  
          
let relocate_module_on_monitored_modules mdata old_name new_subdir=
            let (before,opt,after)=Three_parts.select_center_element  (fun dt->
               Modulesystem_data.name dt=old_name) mdata in
            if opt=None
            then raise(Nonregistered_module_during_relocation(old_name))
            else 
            let old_dt=Option.unpack opt in
            let old_acolytes=Modulesystem_data.acolytes old_dt in
            let old_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) old_acolytes in 
            let new_acolytes=Image.image (fun mlx->Mlx_ended_absolute_path.do_file_displacing mlx new_subdir) old_acolytes in
            let new_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) new_acolytes in 
            let new_name=Mlx_ended_absolute_path.half_dressed_core(List.hd new_acolytes) in
            let data_renamer=Modulesystem_data.rename (old_name,new_name) in
            let s_root=Root_directory.connectable_to_subpath(German_constant.root) in     
            let _=Unix_command.uc
                ("rm -f "^s_root^"_build/"^(Half_dressed_module.uprooted_version old_name)^".cm* ") in
            let part2=Image.image data_renamer (old_dt::after) in
                 (before@part2,(old_files,new_files));;



let above mdata hm=
  match Option.seek(fun dt->Modulesystem_data.name dt=hm) mdata with
  None->raise(Non_registered_module(hm))
  |Some(dt)->Modulesystem_data.all_ancestors dt;;

let below mdata hm=
  Option.filter_and_unpack(fun dt->
      if List.mem hm (Modulesystem_data.all_ancestors dt)
      then Some(Modulesystem_data.name dt)
      else None) mdata;;  

let directly_below mdata hm=
        Option.filter_and_unpack(fun dt->
        if List.mem hm (Modulesystem_data.direct_fathers dt)
        then Some(Modulesystem_data.name dt)
        else None) mdata;;         

let all_mlx_files mdata=
        List.flatten
        (Image.image Modulesystem_data.acolytes mdata);; 
      
let all_mlx_paths mdata=Image.image Mlx_ended_absolute_path.to_absolute_path 
        (all_mlx_files mdata);;  

let all_short_paths mdata=List.flatten(
    Image.image Modulesystem_data.short_paths mdata
);;

let files_containing_string mdata some_string=
let temp1=all_mlx_paths mdata in
List.filter (fun ap->Substring.is_a_substring_of 
  some_string (Io.read_whole_file ap)) temp1;;


let system_size mdata=List.length(mdata);;

exception Inconsistent_constraints of Half_dressed_module.t*Half_dressed_module.t;;
exception Bad_upper_constraint of Half_dressed_module.t;;  

let insertion_index mdata lower_bound upper_bound=
  if upper_bound=None
  then List.length(mdata)
  else let (j_up,data_up)=Option.unpack(upper_bound) in
       if lower_bound=None
       then (* this can be zero if j_up is 1 *)
            j_up-1
       else let (j_down,data_down)=Option.unpack(lower_bound) in
            if (j_down>j_up)
            then raise(Inconsistent_constraints(data_down,data_up))
            else j_up-1;;

let insert_data mdata x (l_hm_before,l_hm_after)=
let indexed_mdata=Ennig.index_everything mdata in
let idx=(fun hm->match Option.seek  
    (fun (j,dt)->Modulesystem_data.name dt=hm)
    indexed_mdata with
    None->None
    |Some(j0,dt0)->Some(j0,Modulesystem_data.name dt0)
)  in
let l_idx_before=Option.filter_and_unpack idx l_hm_before
and l_idx_after=Option.filter_and_unpack idx l_hm_after in
let lower_bound=(if l_idx_before=[] then None else 
   Some(fst(Max.maximize_it fst l_idx_before)))
and upper_bound= (if l_idx_after=[]  then None else 
   Some(fst(Min.minimize_it fst l_idx_after ))) in  
 let i=insertion_index mdata lower_bound upper_bound in
 let youngest_ancestor=List.hd(List.rev(Modulesystem_data.all_ancestors x)) in
 let youngest_ancestor_idx=fst(Option.unpack(idx(youngest_ancestor))) in
 if youngest_ancestor_idx>i
 then raise(Bad_upper_constraint(youngest_ancestor))
 else 
 let (temp1,temp2)=List.partition (fun (j,t)->j<=i) indexed_mdata in
 let temp3=Image.image snd temp1
 and temp4=Image.image snd temp2 in
 temp3@(x::temp4);;

exception Nonregistered_module_during_reposition of Half_dressed_module.t;;  

 
let reposition_module mdata hm (l_before,l_after)=
    let (before,opt,after)=Three_parts.select_center_element
    (fun dt->
       Modulesystem_data.name dt=hm) mdata in
    if opt=None
    then raise(Nonregistered_module_during_reposition(hm))
    else 
    let info=Option.unpack opt 
    and amputated_data=before@after in
    insert_data amputated_data info (l_before,l_after);;  

let rename_directory_on_data (old_subdir,new_subdirname) mdata=
Image.image (Modulesystem_data.rename_endsubdirectory (old_subdir,new_subdirname)) mdata;;

let find_value_definition mdata s=
  if not(String.contains s '.')
  then None
  else
  let j1=String.index(s)('.')+1 in
  let module_name=Cull_string.beginning (j1-1) s in
  let opt=Option.seek (fun md->
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
  
let find_naked_module_registration mdata nm=
    Option.seek (fun md->
      let hm=Modulesystem_data.name md in
      (Half_dressed_module.naked_module hm)=nm
    )
    mdata;;

let all_naked_modules mdata=
Image.image (fun md->
Naked_module.to_string(
  Half_dressed_module.naked_module(Modulesystem_data.name md))
) mdata;;     

let all_ml_absolute_paths mdata=
let temp1=List.filter Modulesystem_data.ml_present mdata in
Image.image (fun md->
  let hm=Modulesystem_data.name md in
  let mlx=Mlx_ended_absolute_path.join hm Ocaml_ending.ml in
  Mlx_ended_absolute_path.to_absolute_path mlx
) temp1;;

let modules_using_value mdata value_name =
  Option.filter_and_unpack (fun md->
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
            ml_present=md.Modulesystem_data.ml_present;
            mli_present=md.Modulesystem_data.mli_present;
            mll_present=md.Modulesystem_data.mll_present;
            mly_present=md.Modulesystem_data.mly_present;
            ml_modification_time=md.Modulesystem_data.ml_modification_time;
            mli_modification_time=md.Modulesystem_data.mli_modification_time;
            mll_modification_time=md.Modulesystem_data.mll_modification_time;
            mly_modification_time=md.Modulesystem_data.mly_modification_time;
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

let update_ancs_libs_and_dirs l=Private_for_ancs_libs_and_dirs.computer_for_update ([],l);;
    
let quick_update mdata x=
  let hm=Modulesystem_data.name (x) in
  if (Half_dressed_module.uprooted_version hm)=Debugged_name.debugger_name
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
  let fathers=PrivateTwo.find_needed_names mdata mlx in
  Some(
  {
    Modulesystem_data.name=x.Modulesystem_data.name;
    ml_present=x.Modulesystem_data.ml_present;
    mli_present=x.Modulesystem_data.mli_present;
    mll_present=x.Modulesystem_data.mll_present;
    mly_present=x.Modulesystem_data.mly_present;
    ml_modification_time=n_ml;
    mli_modification_time=n_mli;
    mll_modification_time=n_mll;
    mly_modification_time=n_mly;
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
      md_list initially_active_hms=
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
      (final_list,active_descendants);;
     
end;; 
     
let recompile_on_monitored_modules tolerate_cycles mdata =
let ref_for_changed_modules=ref[] 
and ref_for_changed_shortpaths=ref[] in
let declare_changed=(fun md->
let hm=Modulesystem_data.name md in
ref_for_changed_modules:=hm::(!ref_for_changed_modules);
ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                        (Modulesystem_data.short_paths md))
) in
let new_md_list=Image.image(
fun md->
match quick_update mdata md with
None->md
|Some(new_md)->
 let _=declare_changed(new_md) in
 new_md
) mdata in
let changed_modules=List.rev(!ref_for_changed_modules) in
if changed_modules=[] then ((mdata,[]),[]) else
let _=PrivateThree.announce_changed_modules changed_modules in
(PrivateThree.put_md_list_back_in_order tolerate_cycles new_md_list changed_modules,
(!ref_for_changed_shortpaths));;  

let printer_equipped_types_from_data mdata=
Option.filter_and_unpack (
fun md->
let hm=Modulesystem_data.name md
and ap=Modulesystem_data.principal_path md in
let text=Io.read_whole_file ap in
if (Substring.is_a_substring_of ("let "^"print_out ") text)
then Some(hm)
else None
) mdata;;


exception Already_registered_file of Mlx_ended_absolute_path.t;;  
exception Overcrowding of Mlx_ended_absolute_path.t*(Ocaml_ending.t list);;
exception Bad_pair of Mlx_ended_absolute_path.t*Ocaml_ending.t;; 
exception Name_conflict of Half_dressed_module.t * Half_dressed_module.t;; 

let register_mlx_file_on_monitored_modules mdata mlx_file =
          let hm=Mlx_ended_absolute_path.half_dressed_core mlx_file
          and ending=Mlx_ended_absolute_path.ending mlx_file in 
          let nm=Half_dressed_module.naked_module hm in
          let (before,opt,after)=Three_parts.select_center_element  (fun dt->
             Half_dressed_module.naked_module(Modulesystem_data.name dt)=nm) 
             mdata in
          if opt=None
          then  let old_info=complete_info mdata mlx_file in
                let info1=Modulesystem_data.make_presence ending old_info in
                (*
                if a mll or mly file is being registered, the ml will automatically be created,
                so let us anticipate by already adding a ml presence
                *)
                let info=(if List.mem ending [Ocaml_ending.mll;Ocaml_ending.mly]
                          then Modulesystem_data.make_ml_present info1 
                          else info1) in
                          before@[info]
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
          let dt1=complete_info mdata mlx_file in
          let new_dt=Modulesystem_data.make_presence ending dt1 in
          if ending<>Ocaml_ending.ml
          then (before@(new_dt::after)) 
          else 
          let temp3=List.rev(Modulesystem_data.direct_fathers new_dt) in
          if temp3=[]
          then (before@(new_dt::after))
          else  
          let last_father=List.hd(List.rev(Modulesystem_data.direct_fathers new_dt)) in
          let (before1,opt1,after1)=Three_parts.select_center_element  (fun dt->
                  (Modulesystem_data.name dt)=last_father) before in
          let lf1=Option.unpack opt1  in    
          let temp2=Image.image (Modulesystem_data.update_anclibdir new_dt mdata) (after1@after) in
          let final_list=before1@(lf1::new_dt::temp2) in
          final_list;;


