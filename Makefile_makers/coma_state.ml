
(* 

#use"Makefile_makers/coma_state.ml";;

*)


(* Inherited values *)

let root =Coma_state_field.root;;
let backup_dir =Coma_state_field.backup_dir;;
let module_at_idx = Coma_state_field.module_at_idx ;;
let subdir_at_idx = Coma_state_field.subdir_at_idx ;;
let principal_ending_at_idx = Coma_state_field.principal_ending_at_idx ;;
let mli_presence_at_idx = Coma_state_field.mli_presence_at_idx ;;
let principal_mt_at_idx = Coma_state_field.principal_mt_at_idx ;;
let mli_mt_at_idx = Coma_state_field.mli_mt_at_idx ;;
let needed_libs_at_idx  = Coma_state_field.needed_libs_at_idx ;;
let direct_fathers_at_idx = Coma_state_field.direct_fathers_at_idx ;;
let ancestors_at_idx = Coma_state_field.ancestors_at_idx ;; 
let needed_dirs_at_idx  = Coma_state_field.needed_dirs_at_idx ;;
let product_up_to_date_at_idx = Coma_state_field.product_up_to_date_at_idx ;;
let directories = Coma_state_field.directories;;
let targets = Coma_state_field.targets;;
let preq_types = Coma_state_field.preq_types;;

let set_module_at_idx = Coma_state_field.set_module_at_idx ;;
let set_subdir_at_idx = Coma_state_field.set_subdir_at_idx ;;
let set_principal_ending_at_idx = Coma_state_field.set_principal_ending_at_idx ;;
let set_mli_presence_at_idx = Coma_state_field.set_mli_presence_at_idx ;;
let set_principal_mt_at_idx = Coma_state_field.set_principal_mt_at_idx ;;
let set_mli_mt_at_idx = Coma_state_field.set_mli_mt_at_idx ;;
let set_needed_libs_at_idx  = Coma_state_field.set_needed_libs_at_idx ;;
let set_direct_fathers_at_idx = Coma_state_field.set_direct_fathers_at_idx ;;
let set_ancestors_at_idx = Coma_state_field.set_ancestors_at_idx ;; 
let set_needed_dirs_at_idx  = Coma_state_field.set_needed_dirs_at_idx ;;
let set_product_up_to_date_at_idx = Coma_state_field.set_product_up_to_date_at_idx ;;
let set_directories = Coma_state_field.set_directories;;
let set_targets = Coma_state_field.set_targets;;
let set_preq_types = Coma_state_field.set_preq_types;;


let modules = Coma_state_field.modules;;
let subdirs = Coma_state_field.subdirs;;
let needed_dirs = Coma_state_field.needed_dirs;;


(* End of inherited values *)

let find_module_index cs nm=
  Small_array.leftmost_index_of_in
   nm (modules cs);;   

let seek_module_index cs nm=
  try
  Some(find_module_index cs nm)
  with
  _->None;;   

let hm_at_idx cs k=
    let (Root_directory_t.R r)= root cs in
    {
      Half_dressed_module.bundle_main_dir = r;
      subdirectory = (Subdirectory.without_trailing_slash(subdir_at_idx cs k));
      naked_module = (Naked_module.to_string(module_at_idx cs k));
    } ;;
  
let hm_from_nm cs nm=
   let idx=find_module_index cs nm in
   hm_at_idx cs idx;;

let check_ending_in_at_idx edg cs idx=
   if edg=principal_ending_at_idx cs idx
   then true 
   else 
   if edg=Ocaml_ending.mli
   then mli_presence_at_idx cs idx
   else false;;

let acolytes_at_idx cs idx=
  let name=hm_at_idx cs idx in
  Option.filter_and_unpack (fun 
  edg->
     if check_ending_in_at_idx edg cs idx
     then Some(Mlx_ended_absolute_path.join name edg)
     else None
) Ocaml_ending.all_endings;;

let short_paths_at_idx cs idx=
   Image.image Mlx_ended_absolute_path.short_path (acolytes_at_idx cs idx);;
  

let registered_endings_at_idx cs idx=
  List.filter (fun edg->
  check_ending_in_at_idx edg cs idx 
  ) Ocaml_ending.all_endings;;


let check_for_single_ending_at_idx cs idx=
  if mli_presence_at_idx cs idx
  then (principal_ending_at_idx cs idx)=Ocaml_ending.mli
  else true ;;



    



let size cs = Small_array.size (modules cs);;      

module Private=struct

let debuggable_targets_from_ancestor_data pr_end hm=
    match pr_end with
     Ocaml_ending.Mll-> 
        let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
             [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
    |Ocaml_ending.Mly-> 
        let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
        [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
    |Ocaml_ending.Ml-> 
             let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
             [ml_target;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
    |Ocaml_ending.Mli-> 
             let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
             [mli_target;Ocaml_target.cmi hm];;    
    
let immediate_ingredients_for_debuggable hm=
        [Ocaml_target.dcmo hm;Ocaml_target.debuggable hm];;  
    
end;;  

let debuggable_targets_from_ancestors cs ancestors=
    let temp1=Image.image (fun hm2->
           let idx2=find_module_index cs hm2 in
           let pr_end2=principal_ending_at_idx cs idx2 
           and hm2=hm_at_idx cs idx2 in
           Private.debuggable_targets_from_ancestor_data pr_end2 hm2
         ) ancestors in
    Preserve_initial_ordering.preserve_initial_ordering temp1;;

let find_needed_data_for_file cs fn=
      let temp1=Look_for_module_names.names_in_file fn in
      Small_array.indices_of_property_of_in 
      (fun nm->List.mem nm temp1)
      (modules cs);; 

let find_needed_data cs mlx=
      let fn=Mlx_ended_absolute_path.to_path mlx in
      find_needed_data_for_file cs fn;;         

let needed_dirs_and_libs_in_command is_optimized cs idx=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let s_root=Root_directory.connectable_to_subpath(root cs) in
   let dirs=
   "-I "^s_root^(Subdirectory.connectable_to_subpath Coma_constant.build_subdir)
  and libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    (needed_libs_at_idx cs idx)) in
    String.concat " " ["";dirs;libs;""];;

let needed_dirs_and_libs_for_several is_optimized cs l_idx=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let pre_dirs1=Image.image 
     (fun idx->Tidel.diforchan(needed_dirs_at_idx cs idx)) l_idx in
   let pre_dirs2=Ordered.forget_order (Tidel.big_teuzin pre_dirs1) in
   let dirs=String.concat(" ")
    (Image.image(fun y->let z=Subdirectory.connectable_to_subpath(y) in 
    if z="" then "" else "-I "^z )
    pre_dirs2) in
   let pre_libs1=Image.image 
     (fun idx->Tidel.diforchan(needed_libs_at_idx cs idx)) l_idx in
   let pre_libs2=Ordered.forget_order (Tidel.big_teuzin pre_libs1) in 
   let libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    pre_libs2) in
    String.concat " " ["";dirs;libs;""];;

let ingredients_for_debuggable cs hm=
      let mlfile=Mlx_ended_absolute_path.join hm Ocaml_ending.Ml in
      let genealogy=find_needed_data cs mlfile in
      let dirfath=Image.image (module_at_idx cs) genealogy in
      let temp1=Image.image 
             (fun idx->
             Tidel.diforchan(ancestors_at_idx cs idx) 
             ) 
             genealogy in
       let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
       let temp3=Small_array.indices_of_property_of_in (
            fun nm->Tidel.elfenn nm temp2
       ) (modules cs) in
       let allanc=Image.image (module_at_idx cs) temp3 in
      (debuggable_targets_from_ancestors cs allanc)
      @(Private.immediate_ingredients_for_debuggable hm);; 

let all_modules cs=
  let n=Small_array.size((modules cs)) in
  Ennig.doyle (hm_at_idx cs) 1 n;; 

let target_at_idx cs idx=
    let hm=hm_at_idx cs idx 
    and mlp=check_ending_in_at_idx Ocaml_ending.ml cs idx
    and mlip=check_ending_in_at_idx Ocaml_ending.mli cs idx
    and mllp=check_ending_in_at_idx Ocaml_ending.mll cs idx
    and mlyp=check_ending_in_at_idx Ocaml_ending.mly cs idx in
    let temp1=[
                mllp,Ocaml_target.ml_from_mll hm;
                mlyp,Ocaml_target.ml_from_mly hm;
           mlp||mlip,Ocaml_target.cmi hm;
           mlp||mlip,Ocaml_target.cmo hm;
           mlp||mlip,Ocaml_target.cma hm;
           mlp||mlip,Ocaml_target.cmx hm;
                 mlp,Ocaml_target.executable hm;
    ] in
    Option.filter_and_unpack 
      (fun x->if fst x 
              then Some(snd x) 
              else None) temp1;;  


let usual_targets cs=
  let n=Small_array.size((modules cs)) in
  let temp1=Ennig.doyle (target_at_idx cs) 1 n in
  List.flatten temp1;;




exception Non_existent_mtime of Mlx_ended_absolute_path.t;;

let force_modification_time root_dir cs mlx=
      let hm=Mlx_ended_absolute_path.half_dressed_core mlx
      and edg=Mlx_ended_absolute_path.ending mlx in
      let nm=Half_dressed_module.naked_module hm in
      let idx=
        (try Small_array.leftmost_index_of_in
          nm (modules cs) with 
        _->raise(Non_existent_mtime(mlx)) ) in
      let file=(Root_directory.connectable_to_subpath root_dir)^
               (Mlx_ended_absolute_path.to_string mlx) in
      let new_val=string_of_float((Unix.stat file).Unix.st_mtime)  in
      let _=(
        if edg=principal_ending_at_idx cs idx 
        then set_principal_mt_at_idx cs idx new_val
      ) in
      let _=(
        if edg=Ocaml_ending.mli 
        then set_mli_mt_at_idx cs idx new_val
      ) in     
      cs;;

let everyone_except_the_debugger cs=
   (* used to be a more complicated function, when the debug module
     was registered.
    *)  
        let n=Small_array.size (modules cs) in
        Image.image (hm_at_idx cs) (Ennig.ennig 1 n);;      
        


exception Non_registered_module of Half_dressed_module.t;;  
exception Derelict_children of Naked_module_t.t*(Naked_module_t.t list);;  
           
            
let unregister_module_on_monitored_modules cs hm=
  let nm=Half_dressed_module.naked_module hm in
  let n=Small_array.size (modules cs) in
  let pre_desc=List.filter(
      fun idx->List.mem nm ( ancestors_at_idx cs idx )
  ) (Ennig.ennig 1 n) in
   if pre_desc<>[]
   then let temp1=Image.image ( module_at_idx cs ) pre_desc in
        raise(Derelict_children(nm,temp1))
   else
   let idx=
    (try Small_array.leftmost_index_of_in
      nm (modules cs) with 
    _->raise(Non_registered_module(hm)) ) in
    let acolytes=acolytes_at_idx cs idx  in
   let _=Coma_state_field.remove_in_each_at_index cs idx in
   let short_paths=Image.image Mlx_ended_absolute_path.short_path acolytes in
   (cs,short_paths);;     
                    

exception Non_registered_file of Mlx_ended_absolute_path.t;;  
exception Abandoned_children of Mlx_ended_absolute_path.t*(Naked_module_t.t list);;
                      
                     
let unregister_mlx_file_on_monitored_modules cs mlxfile=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlxfile in
    let nm=Half_dressed_module.naked_module hm in
    let n=Small_array.size (modules cs) in
    let pre_desc=List.filter(
      fun idx->List.mem nm ( ancestors_at_idx cs idx )
    ) (Ennig.ennig 1 n) in
    if pre_desc<>[]
    then let temp1=Image.image ( module_at_idx cs ) pre_desc in
        raise(Abandoned_children(mlxfile,temp1))
    else
    let idx=
      (try Small_array.leftmost_index_of_in
        nm (modules cs) with 
      _->raise(Non_registered_file(mlxfile)) ) in
    let edg=Mlx_ended_absolute_path.ending mlxfile in
    if (not(check_ending_in_at_idx edg cs idx))
    then raise(Non_registered_file(mlxfile))
    else let _=(if check_for_single_ending_at_idx cs idx
                then Coma_state_field.remove_in_each_at_index cs idx
                else (* if we get here, there are two registered endings, one of which
                       is the mli *) 
                     if edg=Ocaml_ending.mli
                     then (
                       set_mli_presence_at_idx cs idx false;
                       set_mli_mt_at_idx cs idx "0.";
                     )
                     else 
                     let old_mt=principal_mt_at_idx cs idx in
                     (
                      set_principal_ending_at_idx cs idx Ocaml_ending.mli;
                      set_principal_mt_at_idx cs idx old_mt;
                    )
                ) in
                cs;;
            


let compute_subdirectories_list cs=
  let temp1=Small_array.image 
        Subdirectory.without_trailing_slash (subdirs cs) in
    let temp2=Ordered_string.diforchan temp1 in
    let temp3=Ordered_string.forget_order temp2 in
    Image.image Subdirectory.of_string temp3;;

let  check_registrations cs hm=
   let nm=Half_dressed_module.naked_module hm in 
    match seek_module_index cs nm with
      None->Ocaml_ending.exhaustive_uple (fun _->false)
    |Some(idx)->Ocaml_ending.exhaustive_uple 
      (fun edg->check_ending_in_at_idx edg cs idx);;

module PrivateTwo=struct

let find_needed_names cs mlx=
  let temp1=find_needed_data cs mlx in
  Image.image (Small_array.get (modules cs) ) temp1;;  

let find_needed_libraries cs mlx genealogy=
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
           (fun k->List.mem lib (needed_libs_at_idx cs k) ) 
           genealogy
  )
  Ocaml_library.all_libraries;;


let find_needed_directories cs mlx genealogy=
  let temp1=Image.image 
    (fun t->Tidel.diforchan(needed_dirs_at_idx cs t)) 
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

let compute_principal_ending (mlr,mlir,mllr,mlyr)=
    let temp1=List.combine
      [mlr;mllr;mlyr]
      [Ocaml_ending.ml;Ocaml_ending.mll;Ocaml_ending.mly] in
    let temp2=Option.filter_and_unpack (
       fun (bowl,ending)->if bowl then Some(ending) else None 
    ) temp1 in
    if temp2=[] then Ocaml_ending.mli else List.hd temp2;;

let md_compute_modification_time hm edg=
  let dir=Half_dressed_module.bundle_main_dir hm in
  let mlx=Mlx_ended_absolute_path.join hm edg in
  let file=(Root_directory.connectable_to_subpath dir)^(Mlx_ended_absolute_path.to_string mlx) in
  if not(Sys.file_exists file) then "0." else
  let st=Unix.stat file in
  string_of_float(st.Unix.st_mtime);;

let md_compute_modification_times hm=
      Ocaml_ending.exhaustive_uple (md_compute_modification_time hm);;
    
let md_associated_modification_time  (ml_mt,mli_mt,mly_mt,mll_mt) edg=match edg with
     Ocaml_ending.Ml->ml_mt
    |Ocaml_ending.Mli->mli_mt
    |Ocaml_ending.Mll->mll_mt
    |Ocaml_ending.Mly->mly_mt;;  

let complete_info cs  mlx=
  let n=Small_array.size((modules cs)) in
  let (hm,edg)=Mlx_ended_absolute_path.decompose mlx in
  let genealogy=find_needed_data cs mlx in
  let (mlr,mlir,mllr,mlyr)=check_registrations cs hm
  and (mlmt,mlimt,mllmt,mlymt)=md_compute_modification_times hm in
  let pr_end=compute_principal_ending (mlr,mlir,mllr,mlyr) in
  let prmt=md_associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
  let dirfath=Image.image (Small_array.get (modules cs)) genealogy in
  let temp1=Image.image 
        (fun t->Tidel.diforchan(ancestors_at_idx cs t)) 
        genealogy in
  let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
  let tempf=(fun t->
            let nam_t=Small_array.get (modules cs) t in
            if Tidel.elfenn nam_t temp2
            then Some(nam_t)
            else None) in
  let allanc=Option.filter_and_unpack tempf (Ennig.ennig 1 n) in
  let libned=PrivateTwo.find_needed_libraries cs mlx genealogy
  and dirned=PrivateTwo.find_needed_directories cs mlx genealogy in
  (hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,false);;

  let check_unix_presence hm edg=
    let (_,dir)=Half_dressed_module.unveil hm in
    let s_hm=Half_dressed_module.uprooted_version hm 
    and s_dir=Root_directory.connectable_to_subpath dir in
    Sys.file_exists(s_dir^s_hm^(Ocaml_ending.to_string edg));;

let  check_unix_presences hm=
    Ocaml_ending.exhaustive_uple (fun edg->check_unix_presence hm edg);;  

let registrations_for_lonely_ending =function
   Ocaml_ending.Ml->(true,false,false,false)
  |Ocaml_ending.Mli->(false,true,false,false)
  |Ocaml_ending.Mll->(false,false,true,false)
  |Ocaml_ending.Mly->(false,false,false,true);;  



let complete_id_during_new_module_registration cs  mlx=
    let n=Small_array.size((modules cs)) in
    let (hm,edg)=Mlx_ended_absolute_path.decompose mlx in
    let genealogy=find_needed_data cs mlx in
    let (mlp,mlir,mllr,mlyr)=registrations_for_lonely_ending edg
    and (mlmt,mlimt,mllmt,mlymt)=md_compute_modification_times hm in
    let pr_end=edg in
    let prmt=md_associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
    let dirfath=Image.image (Small_array.get (modules cs)) genealogy in
    let temp1=Image.image 
          (fun t->Tidel.diforchan(ancestors_at_idx cs t)) 
          genealogy in
    let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
    let tempf=(fun t->
              let nam_t=Small_array.get (modules cs) t in
              if Tidel.elfenn nam_t temp2
              then Some(nam_t)
              else None) in
    let allanc=Option.filter_and_unpack tempf (Ennig.ennig 1 n) in
    let libned=PrivateTwo.find_needed_libraries cs mlx genealogy
    and dirned=PrivateTwo.find_needed_directories cs mlx genealogy in
    (hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,false);;
  
  
  

exception Nonregistered_module of Naked_module_t.t;;



let rename_module_on_monitored_modules root_dir cs old_name new_name=
  let n=Small_array.size (modules cs) in
  let old_nm=Half_dressed_module.naked_module old_name in
  let opt_idx=seek_module_index cs old_nm in
  if opt_idx=None
  then raise(Nonregistered_module(old_nm))
  else 
  let idx=Option.unpack opt_idx in
  let old_acolytes=acolytes_at_idx cs idx in
  let old_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) 
       old_acolytes in 
  let new_acolytes=Image.image 
     (fun mlx->Mlx_ended_absolute_path.do_file_renaming mlx new_name) 
     old_acolytes in
  let new_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) 
     new_acolytes in 
  let new_hm=Mlx_ended_absolute_path.half_dressed_core(List.hd new_acolytes) in
  let old_mname=Half_dressed_module.naked_module old_name
  and new_mname=Half_dressed_module.naked_module new_hm
  in
  let changer=Look_for_module_names.change_module_name_in_file
  old_mname new_mname in
  let separated_acolytes=Option.filter_and_unpack(
    fun k->
     if List.mem old_mname (ancestors_at_idx cs k)
    then Some(acolytes_at_idx cs k)
    else None
) (Ennig.ennig 1 n) in
  let all_acolytes=List.flatten separated_acolytes in
  let temp3=Image.image Mlx_ended_absolute_path.to_path all_acolytes in
  let temp4=Option.filter_and_unpack (
    fun s->try Some(Absolute_path.of_string s) with _->None
  ) [
      Coma_constant.name_for_printersfile;
    ] in
  let _=Image.image changer (temp3@temp4) in
  let s_root=Root_directory.connectable_to_subpath root_dir in     
  let _=Unix_command.uc
      ("rm -f "^s_root^"_build/"^
      (Half_dressed_module.uprooted_version old_name)^
      ".cm* ") in
  let principal_mt=md_compute_modification_time new_hm (principal_ending_at_idx cs idx)
  and mli_mt=md_compute_modification_time new_hm Ocaml_ending.mli in
  let _=(
    Small_array.set (modules cs) idx new_mname;
    set_principal_mt_at_idx cs idx principal_mt;
    set_mli_mt_at_idx cs idx mli_mt; 
    set_product_up_to_date_at_idx cs idx false; 
  ) in
  let replacer=Image.image(function x->if x=old_mname then new_mname else x) in
  let _=(
     for k=idx+1 to n do
      let old_dirfath=direct_fathers_at_idx cs k
      and old_ancestors=ancestors_at_idx cs k in
     set_direct_fathers_at_idx cs k (replacer old_dirfath) ;
     set_ancestors_at_idx cs k (replacer old_ancestors); 
     done;
  ) in
  (cs,(old_files,new_files));;


let recompute_complete_card_at_idx cs hm=
      let nm=Half_dressed_module.naked_module hm in
      let idx=find_module_index cs nm in
      let edg=List.hd(registered_endings_at_idx cs idx) in
      let mlx=Mlx_ended_absolute_path.join hm edg in
      complete_info cs mlx;;

let recompute_module_info cs hm=
  let nm=Half_dressed_module.naked_module hm in
  let idx=find_module_index cs nm in
  let new_dt=recompute_complete_card_at_idx cs hm in 
  let _=Coma_state_field.set_in_each cs idx new_dt in
  cs;;  

exception Nonregistered_module_during_relocation of Half_dressed_module.t;;  
          
let relocate_module_on_monitored_modules root_dir cs old_name new_subdir=
  let old_nm=Half_dressed_module.naked_module old_name in
  let opt_idx=seek_module_index cs old_nm in
  if opt_idx=None
  then raise(Nonregistered_module_during_relocation(old_name))
  else 
  let idx=Option.unpack opt_idx in 
  let old_acolytes=acolytes_at_idx cs idx in
  let old_files=Image.image Mlx_ended_absolute_path.short_path old_acolytes in 
  let new_acolytes=Image.image 
    (fun mlx->Mlx_ended_absolute_path.do_file_displacing mlx new_subdir) old_acolytes in
  let new_files=Image.image 
     (fun mlx->Mlx_ended_absolute_path.short_path mlx) new_acolytes in 
  let new_name=Mlx_ended_absolute_path.half_dressed_core
   (List.hd new_acolytes) in
  let s_root=Root_directory.connectable_to_subpath root_dir in     
    let _=Unix_command.uc
     ("rm -f "^s_root^"_build/"^(Half_dressed_module.uprooted_version old_name)^".cm* ") in
  let principal_mt=md_compute_modification_time new_name (principal_ending_at_idx cs idx)
  and mli_mt=md_compute_modification_time new_name Ocaml_ending.mli in
  let _=(
       set_subdir_at_idx cs idx new_subdir;
       set_principal_mt_at_idx cs idx principal_mt;
       set_mli_mt_at_idx cs idx mli_mt; 
       set_product_up_to_date_at_idx cs idx false; 
       
  ) in   
  (cs,(old_files,new_files));;



let above cs hm=
  let nm=Half_dressed_module.naked_module hm in
  match seek_module_index cs nm with
  None->raise(Non_registered_module(hm))
  |Some(idx)->ancestors_at_idx cs idx;;

let below cs hm=
  let nm=Half_dressed_module.naked_module hm 
  and n=Small_array.size (modules cs) in
  Option.filter_and_unpack(fun idx->
      if List.mem nm (ancestors_at_idx cs idx)
      then Some(Small_array.get (modules cs) idx)
      else None) (Ennig.ennig 1 n);;  

let directly_below cs hm=
        let nm=Half_dressed_module.naked_module hm 
        and n=Small_array.size (modules cs) in
        Option.filter_and_unpack(fun idx->
            if List.mem nm (direct_fathers_at_idx cs idx)
            then Some(Small_array.get (modules cs) idx)
            else None) (Ennig.ennig 1 n);;        

let ordered_as_in_coma_state cs l=
   let temp1=Small_array.to_list (modules cs) in
   List.filter (fun x->List.mem x l) temp1;;

let above_one_in_several_or_inside cs l=
  let temp1=Image.image (
      fun nm->let idx=find_module_index cs nm in
      ancestors_at_idx cs idx
  ) l in
  let temp2=List.flatten (l::temp1) in
  ordered_as_in_coma_state cs  temp2;;



let all_mlx_files cs=
  let n=Small_array.size (modules cs) in
  List.flatten(Ennig.doyle(acolytes_at_idx cs) 1 n);;                

      
let all_mlx_paths cs=Image.image Mlx_ended_absolute_path.to_absolute_path 
        (all_mlx_files cs);;  

let all_short_paths cs=
    let n=Small_array.size (modules cs) in
    List.flatten(Ennig.doyle(short_paths_at_idx cs) 1 n);;  

let files_containing_string cs some_string=
let temp1=all_mlx_paths cs in
List.filter (fun ap->Substring.is_a_substring_of 
  some_string (Io.read_whole_file ap)) temp1;;


let system_size cs=Small_array.size((modules cs));;

exception Inconsistent_constraints of Naked_module_t.t*Naked_module_t.t;;
exception Bad_upper_constraint of Naked_module_t.t;;  


exception Nonregistered_module_during_reposition of Half_dressed_module.t;;  

 
let reposition_module cs hm (l_before,l_after)=
    let n=Small_array.size((modules cs)) in 
    let find_idx=find_module_index cs in
    let main_idx=find_idx hm
    and indices_before=Image.image find_idx l_before
    and indices_after=Image.image find_idx l_after in
    let max_before=(if indices_before=[] then 1 else Max.list indices_before)
    and min_after=(if indices_after=[] then n else Min.list indices_after)
    in
    if max_before>min_after
    then let hm_before=Small_array.get (modules cs) max_before
         and hm_after=Small_array.get (modules cs) min_after in
         raise(Inconsistent_constraints(hm_before,hm_after))
    else 
    if max_before>main_idx
    then let hm_before=Small_array.get (modules cs) max_before in
         raise(Bad_upper_constraint(hm_before))
    else 
    let _=Coma_state_field.reposition_in_each cs max_before main_idx in 
    cs;;  

let rename_directory_on_data (old_subdir,new_subdirname) cs= 
  let ren_sub=Subdirectory.rename_endsubdirectory (old_subdir,new_subdirname) in 
  let toa=Small_array.apply_transformation_on_all in
  let _=(
   toa (subdirs cs) ren_sub;
   toa (needed_dirs cs) (Image.image ren_sub);
  ) in
  cs;;



let find_value_definition cs s= 
  if not(String.contains s '.')
  then None
  else
  let j1=String.index(s)('.')+1 in
  let module_name=Cull_string.beginning (j1-1) s in
  let nm=Naked_module.of_string(String.uncapitalize_ascii(module_name)) in
  let opt=seek_module_index cs nm in
  if opt=None
  then None 
  else
  let idx1=Option.unpack opt in
  let hm1=hm_at_idx cs idx1 in
  let ap1=Mlx_ended_absolute_path.to_path(Mlx_ended_absolute_path.join hm1 
     Ocaml_ending.Ml) in
  let temp1=Read_ocaml_files.read_ocaml_files [ap1] in	 
  Option.seek (
     fun itm->Ocaml_gsyntax_item.name(itm)=s
  ) temp1;;


let all_naked_modules cs=
  Small_array.image Naked_module.to_string (modules cs);;     

let all_ml_absolute_paths cs=
  let n=Small_array.size (modules cs) in   
Option.filter_and_unpack (fun idx->
  if not(check_ending_in_at_idx Ocaml_ending.ml cs idx)
  then None
  else 
  let hm=hm_at_idx cs idx in
  let mlx=Mlx_ended_absolute_path.join hm Ocaml_ending.ml in
  Some(Mlx_ended_absolute_path.to_absolute_path mlx)
) (Ennig.ennig 1 n);;

let modules_using_value cs value_name =
  let n=Small_array.size (modules cs) in 
  Option.filter_and_unpack (fun idx->
  let hm=hm_at_idx cs idx
  and pr_end=principal_ending_at_idx cs idx in
  let mlx=Mlx_ended_absolute_path.join hm pr_end in
   let ap=Mlx_ended_absolute_path.to_path mlx in
   if Substring.is_a_substring_of 
       value_name (Io.read_whole_file ap)
   then Some hm
   else None ) (Ennig.ennig 1 n);;




let update_ancs_libs_and_dirs_at_idx cs idx=
  let hm=hm_at_idx cs idx  
  and pr_end=principal_ending_at_idx cs idx in
  let mlx=Mlx_ended_absolute_path.join hm pr_end in 
  let fathers=direct_fathers_at_idx cs idx in
  let separated_ancestors=Image.image 
  (fun nm2->
    let idx2=Small_array.leftmost_index_of_in nm2 (modules cs) in
    Tidel.safe_set(ancestors_at_idx cs idx2)
  ) fathers in
  let all_ancestors=Tidel.big_teuzin((Tidel.safe_set fathers)::separated_ancestors) in
  let unordered_ancestor_indices=Tidel.image (
    fun nm3->Small_array.leftmost_index_of_in nm3 (modules cs)
  ) all_ancestors in
  let genealogy=Ordered.forget_order(Tidel.diforchan unordered_ancestor_indices) in
  let new_libs=PrivateTwo.find_needed_libraries cs mlx genealogy
  and new_dirs=PrivateTwo.find_needed_directories cs mlx genealogy 
  and ordered_ancestors=Image.image (
    Small_array.get (modules cs)
  ) genealogy in
  (
    set_ancestors_at_idx cs idx ordered_ancestors;
    set_needed_libs_at_idx cs idx new_libs;
    set_needed_dirs_at_idx cs idx new_dirs;
  );;

let update_ancs_libs_and_dirs cs=
  let n=Small_array.size (modules cs) in
  for idx=1 to n do
    update_ancs_libs_and_dirs_at_idx cs idx
  done;;  
  
  


    



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
      let temp1=Image.image Naked_module.to_string changed_modules in
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
      cs initially_active_nms=
      let md_list=Small_array.to_list (modules cs) in
      let coat=Memoized.make (fun nm->
        let idx=Small_array.leftmost_index_of_in nm (modules cs) in
        direct_fathers_at_idx cs idx
      ) in
      let (cycles,reordered_list)=Reconstruct_linear_poset.reconstruct_linear_poset 
         coat md_list in
      let _=treat_circular_dependencies tolerate_cycles
           (fun nm->
           let idx=Small_array.leftmost_index_of_in nm (modules cs) in 
           Half_dressed_module.uprooted_version( hm_at_idx cs idx) )
           cycles in     
      let _=Coma_state_field.reorder cs (Image.image fst reordered_list) in    
      let _=update_ancs_libs_and_dirs cs in 
      let n=Small_array.size (modules cs) in
      let active_descendants=Option.filter_and_unpack (
          fun idx->
            let nm=Small_array.get (modules cs) idx in
            if List.mem nm initially_active_nms
            then Some(nm)
            else
            if List.exists (fun nm2->List.mem nm2 initially_active_nms) 
                 (ancestors_at_idx cs idx)
            then Some(nm)
            else None
      ) (Ennig.ennig 1 n) in  
      (cs,active_descendants);;
     
end;; 
     
let md_recompute_modification_time hm edg=
 let dir=Half_dressed_module.bundle_main_dir hm in
  let mlx=Mlx_ended_absolute_path.join hm edg in
  let file=(Root_directory.connectable_to_subpath dir)^(Mlx_ended_absolute_path.to_string mlx) in
  if not(Sys.file_exists file) then "0." else
  let st=Unix.stat file in
  string_of_float(st.Unix.st_mtime);;
  

let quick_update cs idx=
  let hm=hm_at_idx cs idx 
  and pr_ending=principal_ending_at_idx cs idx in
  if (Half_dressed_module.uprooted_version hm)=Coma_constant.name_for_debugged_module
  then None
  else
  let mli_modif_time=md_recompute_modification_time hm Ocaml_ending.mli 
  and pr_modif_time=md_recompute_modification_time hm pr_ending 
  and old_mli_modif_time=mli_mt_at_idx cs idx
  and old_pr_modif_time=principal_mt_at_idx cs idx 
  in
  let new_values=(mli_modif_time,pr_modif_time)
  and old_values=(old_mli_modif_time,old_pr_modif_time) in
  if old_values=new_values
  then None
  else
  let mlx=Mlx_ended_absolute_path.join hm pr_ending in
  let direct_fathers=PrivateTwo.find_needed_names cs mlx in
  Some(
    pr_modif_time,
    mli_modif_time,
    direct_fathers
   )   
  ;;
    

let recompile_on_monitored_modules tolerate_cycles cs = 
  let n=Small_array.size (modules cs) in
  let ref_for_changed_modules=ref[] 
  and ref_for_changed_shortpaths=ref[] in
  let declare_changed=(fun idx->
    let nm=Small_array.get (modules cs) idx in
    ref_for_changed_modules:=nm::(!ref_for_changed_modules);
    ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                        (short_paths_at_idx cs idx))
    ) in
  let _=List.iter (fun idx->
    match quick_update cs idx with
    None->()
    |Some(pr_modif_time,mli_modif_time,direct_fathers)->
    (
    declare_changed(idx);
    set_principal_mt_at_idx cs idx pr_modif_time;
    set_mli_mt_at_idx cs idx mli_modif_time;
    set_direct_fathers_at_idx cs idx direct_fathers
    )
)(Ennig.ennig 1 n) in
let changed_modules=List.rev(!ref_for_changed_modules) in
if changed_modules=[] then ((cs,[]),[]) else
let _=PrivateThree.announce_changed_modules changed_modules in
(PrivateThree.put_md_list_back_in_order tolerate_cycles 
  cs changed_modules,
(!ref_for_changed_shortpaths));;  

let printer_equipped_types_from_data cs=
  let n=Small_array.size (modules cs) in
  Option.filter_and_unpack (
    fun idx->
    let hm=hm_at_idx cs idx
    and pr_end=principal_ending_at_idx cs idx in
    let mlx=Mlx_ended_absolute_path.join hm pr_end in
    let ap=Mlx_ended_absolute_path.to_absolute_path mlx in
    let text=Io.read_whole_file ap in
    if (Substring.is_a_substring_of ("let "^"print_out ") text)
    then Some(hm)
    else None
  ) (Ennig.ennig 1 n);;
 



exception Already_registered_file of Mlx_ended_absolute_path.t;;  
exception Overcrowding of Mlx_ended_absolute_path.t*(Ocaml_ending.t list);;
exception Bad_pair of Mlx_ended_absolute_path.t*Ocaml_ending.t;; 


let register_mlx_file_on_monitored_modules cs mlx_file =
          let n=Small_array.size (modules cs) in
          let hm=Mlx_ended_absolute_path.half_dressed_core mlx_file
          and ending=Mlx_ended_absolute_path.ending mlx_file in 
          let nm=Half_dressed_module.naked_module hm in
          let opt_idx=seek_module_index cs nm in
          if opt_idx=None
          then  let info=complete_id_during_new_module_registration cs mlx_file in
                let _=Coma_state_field.push_right_in_each cs info in         
                cs
          else
          let idx=Option.unpack(opt_idx) in
          let edgs=registered_endings_at_idx cs idx in
          if List.length(edgs)>1
          then  raise(Overcrowding(mlx_file,edgs))
          else  
          if List.mem ending edgs
          then raise(Already_registered_file(mlx_file))
          else
          if (not(List.mem Ocaml_ending.mli (ending::edgs)))
          then raise(Bad_pair(mlx_file,List.hd edgs))
          else 
          let (hm,old_pr_end,old_mlir,prmt,mlimt,libned,dirfath,allanc,dirned,is_updated)=complete_info cs mlx_file in
          let nm=Half_dressed_module.naked_module hm in
          let (pr_end,mlir)=(
            if ending=Ocaml_ending.mli
            then (old_pr_end,true)
            else (ending,old_mlir) 
          ) in
          let new_dt=(hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,false) in
          if ending<>Ocaml_ending.ml
          then let _=Coma_state_field.set_in_each cs idx new_dt in         
               cs
          else 
          let temp3=List.rev(dirfath) in
          if temp3=[]
          then let _=Coma_state_field.set_in_each cs idx new_dt in         
               cs
          else  
          let last_father=List.hd(temp3) in
          let last_father_idx=Small_array.leftmost_index_of_in last_father (modules cs) in
          let _=
            (
              for k=last_father_idx+1 to n 
              do
              let current_anc= ancestors_at_idx cs k in  
              if not(List.mem nm current_anc)
              then ()
              else  
                   let current_libs= needed_libs_at_idx cs k in
                   let new_ancestors=Small_array.filter_and_unpack(
                      fun nm2->
                      if (List.mem nm2 allanc)||(List.mem nm2 current_anc)
                      then Some(nm2)
                      else None
                    ) (modules cs) 
                    and new_libs=List.filter (
                      fun lib->(List.mem lib libned)||(List.mem lib current_libs)
                    ) Ocaml_library.all_libraries in  
                    let ordered_dirs=Tidel.teuzin
                       (Tidel.safe_set(needed_dirs_at_idx cs k))
                       (Tidel.safe_set (dirned)) in
                    let new_dirs=Ordered.forget_order(ordered_dirs) in
                    set_ancestors_at_idx cs k new_ancestors;
                    set_needed_libs_at_idx cs k new_libs;
                    set_needed_dirs_at_idx cs k new_dirs;
              done;
              Coma_state_field.push_after_in_each cs last_father_idx new_dt;  
            )
          in
          cs;;

module Ingredients_for_ocaml_target=struct

  exception Unregistered_cmo  of Half_dressed_module.t;;
  exception Unregistered_dcmo of Half_dressed_module.t;;
  exception Unregistered_cmi  of Half_dressed_module.t;;
  exception Unregistered_cma  of Half_dressed_module.t;;
  exception Unregistered_cmx  of Half_dressed_module.t;;
  exception Unregistered_ml_from_mll of Half_dressed_module.t;;
  exception Unregistered_ml_from_mly of Half_dressed_module.t;;
  exception Unregistered_executable of Half_dressed_module.t;;
  exception Unregistered_debuggable of Half_dressed_module.t;;
  exception Unregistered_module of (Half_dressed_module.t);;
  exception NonMarkedIngredientsForToplevel of string;;
  
  
  let targets_from_ancestor_data cs idx=
    let hm=hm_at_idx cs idx in
    if check_ending_in_at_idx Ocaml_ending.mll cs idx
    then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
    else 
    if check_ending_in_at_idx Ocaml_ending.mly cs idx
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
    else
    if check_ending_in_at_idx Ocaml_ending.ml cs idx
    then 
         let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
         [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
    else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
         [mli_target;Ocaml_target.cmi hm];;  
  
  let targets_from_ancestors cs idx=
       let ancestors=ancestors_at_idx cs idx in
       let temp1=Image.image (fun nm2->
              let idx2=find_module_index cs nm2 in
              targets_from_ancestor_data cs idx2
            ) ancestors in
       Preserve_initial_ordering.preserve_initial_ordering temp1;;
  
  let optimized_targets_from_ancestor_data cs idx=
    let hm=hm_at_idx cs idx in
    if check_ending_in_at_idx Ocaml_ending.mll cs idx
    then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
    else 
    if check_ending_in_at_idx Ocaml_ending.mly cs idx
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
    else
    if check_ending_in_at_idx Ocaml_ending.ml cs idx
    then 
         let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
         [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
    else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
         [mli_target;Ocaml_target.cmi hm];;  
  
  let optimized_targets_from_ancestors cs idx=
       let ancestors=ancestors_at_idx cs idx in
       let temp1=Image.image (fun nm2->
            let idx2=find_module_index cs nm2 in
            optimized_targets_from_ancestor_data cs idx2
            ) ancestors in
       Preserve_initial_ordering.preserve_initial_ordering temp1;;
  
  let immediate_ingredients_for_ml_from_mll hm=
    let mll_target=Ocaml_target.no_dependencies
       (Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
    [mll_target];;
  
  let immediate_ingredients_for_ml_from_mly hm=
    let mly_target=Ocaml_target.no_dependencies
      (Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
    [mly_target];;
  
  let immediate_ingredients_for_cmi cs idx hm=
      if check_ending_in_at_idx Ocaml_ending.mll cs idx
      then let mll_target=Ocaml_target.no_dependencies
             (Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
           [mll_target;Ocaml_target.ml_from_mll hm]
      else 
      if check_ending_in_at_idx Ocaml_ending.mly cs idx
      then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
           [mly_target;Ocaml_target.ml_from_mly hm]
      else
    if check_ending_in_at_idx Ocaml_ending.mli cs idx
    then let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
         [mli_target]
    else let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
         [ml_target];; 
  
  let immediate_ingredients_for_cmo cs idx hm=
      if check_ending_in_at_idx Ocaml_ending.mll cs idx
      then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
           [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
      else 
      if check_ending_in_at_idx Ocaml_ending.mly cs idx
      then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
           [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
      else
    if check_ending_in_at_idx Ocaml_ending.ml cs idx
    then let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
         [ml_target;Ocaml_target.cmi hm]
    else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
         [mli_target;Ocaml_target.cmi hm];;  
  
  
  let immediate_ingredients_for_dcmo=immediate_ingredients_for_cmo;;
  
  let immediate_ingredients_for_cma=immediate_ingredients_for_cmo;;
  
  let immediate_ingredients_for_cmx cs idx hm=
      if check_ending_in_at_idx Ocaml_ending.mll cs idx
      then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
           [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
      else 
      if check_ending_in_at_idx Ocaml_ending.mly cs idx
      then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
           [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
      else
    if check_ending_in_at_idx Ocaml_ending.ml cs idx
    then let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
         [ml_target;Ocaml_target.cmi hm]
    else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
         [mli_target;Ocaml_target.cmi hm];;  
  
  
  let immediate_ingredients_for_executable hm=
   [Ocaml_target.cmx hm;Ocaml_target.executable hm];;  
  
  
  let ingredients_for_nodep mlx=[];;
  
  let ingredients_for_ml_from_mll cs hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_ml_from_mll(hm)) else 
    let idx=Option.unpack opt_idx in
    (targets_from_ancestors cs idx)@(immediate_ingredients_for_ml_from_mll hm);;
  
  let ingredients_for_ml_from_mly cs hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_ml_from_mly(hm)) else 
      let idx=Option.unpack opt_idx in
      (targets_from_ancestors cs idx)@(immediate_ingredients_for_ml_from_mly hm);;
  
  
  let ingredients_for_cmi cs hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_cmi(hm)) else 
    let idx=Option.unpack opt_idx in
    (targets_from_ancestors cs idx)@(immediate_ingredients_for_cmi cs idx hm);;
  
  let ingredients_for_cmo cs hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_cmo(hm)) else 
      let idx=Option.unpack opt_idx in
      (targets_from_ancestors cs idx)@
      (immediate_ingredients_for_cmo cs idx hm);;
  
  
  let ingredients_for_dcmo cs hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_dcmo(hm)) else 
    let idx=Option.unpack opt_idx in
    let ancestors=ancestors_at_idx cs idx  in
    (debuggable_targets_from_ancestors cs ancestors)@
    (immediate_ingredients_for_dcmo cs idx hm);;
  
  let ingredients_for_cma cs hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_cma(hm)) else 
      let idx=Option.unpack opt_idx in
      (targets_from_ancestors cs idx)@
      (immediate_ingredients_for_cma cs idx hm);;
  
  let ingredients_for_cmx cs hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_cma(hm)) else 
      let idx=Option.unpack opt_idx in
      (optimized_targets_from_ancestors cs idx)@
      (immediate_ingredients_for_cmx cs idx hm);;    
  
  let ingredients_for_executable cs hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_executable(hm)) else 
      let idx=Option.unpack opt_idx in
      (optimized_targets_from_ancestors cs idx)@
      (immediate_ingredients_for_executable  hm);;   
  
  
  let ingredients_for_usual_element cs hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_executable(hm)) else 
    let idx=Option.unpack opt_idx in
    let mli_reg=check_ending_in_at_idx Ocaml_ending.mli cs idx
    and ml_reg=check_ending_in_at_idx Ocaml_ending.mli cs idx in
    if mli_reg&&(not ml_reg)
    then (ingredients_for_cmi cs hm)@[Ocaml_target.cmi hm]
    else (ingredients_for_cmo cs hm)@[Ocaml_target.cmo hm];;  
    
    
  let ingredients_for_ocaml_target cs=function
    Ocaml_target.NO_DEPENDENCIES(mlx)->[]
   |Ocaml_target.ML_FROM_MLL(hm)->ingredients_for_ml_from_mll cs hm
   |Ocaml_target.ML_FROM_MLY(hm)->ingredients_for_ml_from_mly cs hm
   |Ocaml_target.CMI(hm)->ingredients_for_cmi cs hm
   |Ocaml_target.CMO(hm)->ingredients_for_cmo cs hm
   |Ocaml_target.DCMO(hm)->ingredients_for_dcmo cs hm
   |Ocaml_target.CMA(hm)->ingredients_for_cma cs hm
   |Ocaml_target.CMX(hm)->ingredients_for_cmx cs hm
   |Ocaml_target.EXECUTABLE(hm)->ingredients_for_executable cs hm
   |Ocaml_target.DEBUGGABLE(hm)->ingredients_for_debuggable cs hm;;      
   
  
  
  let marked_ingredients_for_full_compilation cs name l=
    let temp1=Image.image (ingredients_for_usual_element cs) l in
    Preserve_initial_ordering.and_mark_endings temp1;;
  
  let module_dependency_for_nodep mlx=false;;
  let module_dependency_for_ml_from_mll cs l_hm hm1=
         if List.mem hm1 l_hm
         then true
         else  
         let nm1=Half_dressed_module.naked_module hm1 in
         let idx1=find_module_index cs nm1 in
         let anc1=ancestors_at_idx cs idx1 in
         List.exists 
          (fun z->List.mem (Half_dressed_module.naked_module z) anc1 ) 
          l_hm;;
  
  
  let module_dependency_for_ml_from_mly=module_dependency_for_ml_from_mll;; 
  let module_dependency_for_cmi=module_dependency_for_ml_from_mll;;
  let module_dependency_for_cmo=module_dependency_for_ml_from_mll;;
  let module_dependency_for_dcmo=module_dependency_for_ml_from_mll;;
  let module_dependency_for_cma=module_dependency_for_ml_from_mll;;                 
  let module_dependency_for_cmx=module_dependency_for_ml_from_mll;;  
  let module_dependency_for_executable=module_dependency_for_ml_from_mll;;  
  let module_dependency_for_debuggable=module_dependency_for_ml_from_mll;;  
  let module_dependency_for_toplevel cs l_hm name l_hm2=
    List.exists(fun hm2->
    (module_dependency_for_cmo cs l_hm hm2)||(List.mem hm2 l_hm)
    ) l_hm2;;
  
  
  let module_dependency_for_ocaml_target cs l_hm =function
    Ocaml_target.NO_DEPENDENCIES(mlx)->false
   |Ocaml_target.ML_FROM_MLL(hm)->module_dependency_for_ml_from_mll cs l_hm hm
   |Ocaml_target.ML_FROM_MLY(hm)->module_dependency_for_ml_from_mly cs l_hm hm
   |Ocaml_target.CMI(hm)->module_dependency_for_cmi cs l_hm hm
   |Ocaml_target.CMO(hm)->module_dependency_for_cmo cs l_hm hm
   |Ocaml_target.DCMO(hm)->module_dependency_for_dcmo cs l_hm hm
   |Ocaml_target.CMA(hm)->module_dependency_for_cma cs l_hm hm
   |Ocaml_target.CMX(hm)->module_dependency_for_cmx cs l_hm hm
   |Ocaml_target.EXECUTABLE(hm)->module_dependency_for_executable cs l_hm hm
   |Ocaml_target.DEBUGGABLE(hm)->module_dependency_for_debuggable cs l_hm hm;;
  
         
  
  
  
  let mlx_dependency_for_ocaml_target cs mlx tgt=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
    module_dependency_for_ocaml_target cs [hm] tgt;;
  
  let mlx_list_dependency_for_ocaml_target cs l_mlx tgt=
   List.exists (fun mlx->mlx_dependency_for_ocaml_target cs mlx tgt) l_mlx;;
  
  

end;;  

module Command_for_ocaml_target=struct

  let ocamlc="ocamlc  -bin-annot ";;
  let ocamlopt="ocamlopt  -bin-annot ";;
  let cee=" -c ";;
  
  exception Command_called_on_nodep of Mlx_ended_absolute_path.t;;
  exception Unregistered_cmo  of Half_dressed_module.t;;
  exception Unregistered_dcmo of Half_dressed_module.t;;
  exception Unregistered_cmi  of Half_dressed_module.t;;
  exception Unregistered_cma  of Half_dressed_module.t;;
  exception Unregistered_cmx  of Half_dressed_module.t;;
  exception Unregistered_ml_from_mll of Half_dressed_module.t;;
  exception Unregistered_ml_from_mly of Half_dressed_module.t;;
  exception Unregistered_executable of Half_dressed_module.t;;
  exception Unregistered_debuggable of Half_dressed_module.t;;
  exception Unregistered_modules_in_toplevel of string*(Half_dressed_module.t list);;  
  
  let ingr=Ingredients_for_ocaml_target.ingredients_for_ocaml_target;;
  
  let cmx_manager=function
   Ocaml_target.CMX(hm2)->
      let s_hm2=Half_dressed_module.to_shortened_string hm2 in
      Some((Subdirectory.connectable_to_subpath Coma_constant.exec_build_subdir)^s_hm2^".cmx")
   |_->None;;
  
  let dcmo_manager=function
   Ocaml_target.DCMO(hm2)->
      let s_hm2=Half_dressed_module.to_shortened_string hm2 in
      Some((Subdirectory.connectable_to_subpath Coma_constant.debug_build_subdir)^s_hm2^".d.cmo")
   |_->None;;
  
  let command_for_nodep mlx=[];;
  
  let command_for_ml_from_mll dir hm=
            let s_hm=Half_dressed_module.uprooted_version hm in
            let s_root=Root_directory.connectable_to_subpath dir in
            let s_fhm=s_root^s_hm in
            [
              "ocamllex  -o "^s_fhm^".ml "^s_fhm^".mll";
            ];;
   
  let command_for_ml_from_mly dir hm=
            let s_hm=Half_dressed_module.uprooted_version hm in
            let s_root=Root_directory.connectable_to_subpath dir in
            let s_fhm=s_root^s_hm in
            [
              "ocamlyacc "^s_fhm^".mly"
            ];;  
  
  let command_for_cmi dir cs hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_cmi(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_root=Root_directory.connectable_to_subpath(dir) in
    let s_hm=Half_dressed_module.uprooted_version hm in
    let s_fhm=s_root^s_hm in
    let mli_reg=check_ending_in_at_idx Ocaml_ending.mli cs idx in
    let ending=(if mli_reg then ".mli" else ".ml") in
    let central_cmd=
        "ocamlc  -bin-annot "^
        (needed_dirs_and_libs_in_command false cs idx)^
            " -c "^s_fhm^ending in
            let full_mli=s_root^s_hm^".mli" in
            if (not mli_reg)
               &&(Sys.file_exists(full_mli))
            then (* 
                   in this situation the mli file exists but is not registered.
                   So the modulesystem manager must treat it as though it didn't
                   exist. We temporarily rename it so that ocamlc will ignore it.
                  *)
                  let dummy_mli=s_root^"uvueaoqhkt.mli" in
                  [
                   "mv "^full_mli^" "^dummy_mli;
                   central_cmd;
                   "mv "^s_fhm^".cm* "^s_root^(Subdirectory.connectable_to_subpath Coma_constant.build_subdir);
                   "mv "^dummy_mli^" "^full_mli
                  ] 
            else  [
                     central_cmd;
                     "mv "^s_fhm^".cm* "^s_root^(Subdirectory.connectable_to_subpath Coma_constant.build_subdir)
                   ];;
  
  let command_for_cmo dir cs hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_cmo(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Half_dressed_module.uprooted_version hm in
    let s_root=Root_directory.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let dirs_and_libs=needed_dirs_and_libs_in_command false cs idx in
    [ 
      "ocamlc -bin-annot "^dirs_and_libs^" -o "^s_fhm^".cmo -c "^s_fhm^".ml";
      "mv "^s_fhm^".cm* "^s_root^(Subdirectory.connectable_to_subpath Coma_constant.build_subdir)
    ];;
  
  let command_for_dcmo dir cs hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_cmo(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Half_dressed_module.uprooted_version hm in
    let s_root=Root_directory.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let dirs_and_libs=needed_dirs_and_libs_in_command false cs idx in
    [ 
      "ocamlc -g "^dirs_and_libs^" -o "^s_fhm^".d.cmo -c "^s_fhm^".ml";
      "mv "^s_fhm^".d.cm* "^s_root^(Subdirectory.connectable_to_subpath Coma_constant.debug_build_subdir)
    ];;
  
  let command_for_cma dir cs hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_cma(hm)) else 
      let idx=Option.unpack opt_idx in
      let s_hm=Half_dressed_module.uprooted_version hm in
      let s_root=Root_directory.connectable_to_subpath(dir) in
      let s_fhm=s_root^s_hm in
      let dirs_and_libs=needed_dirs_and_libs_in_command true cs idx in
      [ 
        "ocamlopt -bin-annot -a "^dirs_and_libs^" -o "^s_fhm^".cma -c "^s_fhm^".ml";
        "mv "^s_fhm^".cm* "^s_root^(Subdirectory.connectable_to_subpath Coma_constant.build_subdir)
      ];;  
  
  let command_for_cmx dir cs hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index cs nm in
      if opt_idx=None then raise(Unregistered_cmx(hm)) else 
      let idx=Option.unpack opt_idx in
      let s_hm=Half_dressed_module.uprooted_version hm in
      let s_root=Root_directory.connectable_to_subpath(dir) in
      let s_fhm=s_root^s_hm in
      let dirs_and_libs=needed_dirs_and_libs_in_command true cs idx in
      let dir_for_building=
        (Subdirectory.connectable_to_subpath Coma_constant.exec_build_subdir) in
      [ 
        "ocamlopt "^dirs_and_libs^" -o "^s_fhm^".cmx -c "^s_fhm^".ml";
        "mv "^s_fhm^".cm* "^s_root^dir_for_building;
        "mv "^s_fhm^".o "^s_root^dir_for_building
      ];;      
            
  let command_for_executable dir cs hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_executable(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Half_dressed_module.uprooted_version hm in
    let s_root=Root_directory.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let temp1=ingr cs (Ocaml_target.EXECUTABLE(hm)) in
    let temp2=Option.filter_and_unpack cmx_manager temp1 in
    let long_temp2=Image.image (fun t->s_root^t) temp2 in
    let dirs_and_libs=needed_dirs_and_libs_in_command true cs idx  in
    let dir_for_building=
        (Subdirectory.connectable_to_subpath Coma_constant.exec_build_subdir) in
    [ 
      "ocamlopt "^dirs_and_libs^" -o "^s_fhm^".ocaml_executable "^
        (String.concat " " long_temp2);
      "mv "^s_fhm^".ocaml_executable "^s_root^dir_for_building
    ];;
            
  let command_for_debuggable dir cs hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index cs nm in
    if opt_idx=None then raise(Unregistered_debuggable(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Half_dressed_module.uprooted_version hm in
    let s_root=Root_directory.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let temp1=ingr cs (Ocaml_target.DEBUGGABLE(hm)) in
    let temp2=Option.filter_and_unpack dcmo_manager temp1 in
    let long_temp2=Image.image (fun t->s_root^t) temp2 in
    let dirs_and_libs=needed_dirs_and_libs_in_command false cs idx in
    let dir_for_building=
      (Subdirectory.connectable_to_subpath Coma_constant.debug_build_subdir) in
    [ 
      "ocamlc -g "^dirs_and_libs^" -o "^s_fhm^".ocaml_debuggable "^
        (String.concat " " long_temp2);
      "mv "^s_fhm^".ocaml_debuggable "^s_root^dir_for_building
    ];;          
    
   
  let command_for_ocaml_target dir cs tgt=
     match tgt with
    Ocaml_target.NO_DEPENDENCIES(mlx)->command_for_nodep mlx 
   |Ocaml_target.ML_FROM_MLL(hm)->command_for_ml_from_mll dir hm
   |Ocaml_target.ML_FROM_MLY(hm)->command_for_ml_from_mly dir hm
   |Ocaml_target.CMI(hm)->command_for_cmi dir cs hm
   |Ocaml_target.CMO(hm)->command_for_cmo dir cs hm
   |Ocaml_target.DCMO(hm)->command_for_dcmo dir cs hm
   |Ocaml_target.CMA(hm)->command_for_cma dir cs hm
   |Ocaml_target.CMX(hm)->command_for_cmx dir cs hm
   |Ocaml_target.EXECUTABLE(hm)->command_for_executable dir cs hm
   |Ocaml_target.DEBUGGABLE(hm)->command_for_debuggable dir cs hm;;
     
  
   
  

end;;

module Ocaml_target_making=struct

  let cmd_for_tgt=Command_for_ocaml_target.command_for_ocaml_target;;

  let ingr_for_tgt =Ingredients_for_ocaml_target.ingredients_for_ocaml_target;;
  let ingr_for_top =Ingredients_for_ocaml_target.marked_ingredients_for_full_compilation;;
  
  
  let is_up_to_date dir tgts tgt=
    if Ocaml_target.is_a_debuggable tgt
    then false
    else 
    if Ocaml_target.test_target_existence dir tgt
    then List.mem tgt tgts
    else false;;
  
  let unit_make dir (bowl,(mdata,tgts,rejected_ones)) tgt=
    if (not bowl)
    then (bowl,(mdata,tgts,rejected_ones))
    else
    if is_up_to_date dir tgts tgt
    then (true,(mdata,tgts,rejected_ones))
    else 
    let temp1=Image.image Unix_command.uc (cmd_for_tgt dir mdata tgt) in 
    if List.for_all (fun i->i=0) temp1
    then let tgts2=(
          if Ocaml_target.is_a_debuggable tgt 
          then tgts
          else tgt::tgts
         )  in
          match Ocaml_target.ml_from_lex_or_yacc_data tgt with
         None->(true,(mdata,tgts2,rejected_ones))
         |Some(mlx)->
                     let mdata2=force_modification_time dir mdata mlx in
                     (true,(mdata2,tgts2,rejected_ones))        
    else let rejected_ones2=(
          match Ocaml_target.main_module tgt with
          None->rejected_ones
          |Some(hm)->hm::rejected_ones
          )  in
         (false,(mdata,tgts,rejected_ones2));;
  
  let make dir (mdata,tgts,rejected_ones) tgt=
    let l=ingr_for_tgt mdata tgt in
    List.fold_left (unit_make dir)  (true,(mdata,tgts,rejected_ones)) l;;
    
  exception Ending_for_full_compilation_pusher;;  
  
  let pusher_for_full_compilation dir (successful_ones,to_be_treated,ts)=
    match to_be_treated with
    []->raise(Ending_for_full_compilation_pusher)
    |(tgt,is_an_ending_or_not)::others->
    let (bowl2,ts2)=unit_make dir (true,ts) tgt in
    if bowl2
    then let new_successful_ones=(
           if is_an_ending_or_not=Is_an_ending_or_not.Yes
           then let hm=Option.unpack(Ocaml_target.main_module tgt) in
                (*
                  Note that the cmi and cmo give the same hm
                *)
                if List.mem hm successful_ones
                then successful_ones
                else hm::successful_ones
           else successful_ones
         ) in
         (new_successful_ones,others,ts2)
    else let hm=Option.unpack(Ocaml_target.main_module tgt) in
         let root=Half_dressed_module.bundle_main_dir hm in
         let s_root=Root_directory.connectable_to_subpath root in
         let (mdata,_,_)=ts 
         and (mdata2,tgts2,rejected_ones2)=ts2 in
         let (rejects,remains)=List.partition
         (fun (tgtt,_)->
           Ingredients_for_ocaml_target.module_dependency_for_ocaml_target 
           mdata [hm] tgtt
         ) others in
         let _=Image.image (
           fun (tgtt,_)->
           if Ocaml_target.has_dependencies tgtt
           then let s_ap=s_root^"_build/"^
                    (Ocaml_target.to_shortened_string tgtt) in
                let _=Unix_command.uc("rm -f "^s_ap) in
                ()
         ) ((tgt,is_an_ending_or_not)::rejects) in
         let newly_rejected_ones=Option.filter_and_unpack 
         (fun (tgt,_)->Ocaml_target.main_module tgt) rejects in
         let rejected_ones2=Listennou.nonredundant_version(
           List.rev_append newly_rejected_ones rejected_ones2
         ) in
         (successful_ones,remains,(mdata2,tgts2,rejected_ones2));; 
  
  let rec  iterator_for_full_compilation dir (successful_ones,to_be_treated,ts)=
    match to_be_treated with
    []->(List.rev successful_ones,ts)
    |_->iterator_for_full_compilation dir 
     (pusher_for_full_compilation dir (successful_ones,to_be_treated,ts));;
  
    
  
   
let feydeau old_mdata=
    let dir=root old_mdata 
    and old_tgts=targets old_mdata in
    let l=everyone_except_the_debugger old_mdata in
    let ts=(old_mdata,old_tgts,[]) in
    let temp1=ingr_for_top old_mdata "ecaml" l in
    let (successful_ones,ts2)=iterator_for_full_compilation dir ([],temp1,ts) in
    ts2;;
  
let make_final_target mode opt_argument cs=
    match   mode with
     Compilation_mode_t.Usual->(false,feydeau cs)
    |Compilation_mode_t.Debug->
        let dbg=Coma_constant.name_for_debugged_module in
        let rdir=compute_subdirectories_list cs in
        let ap=Find_suitable_ending.find_file_location (root cs) rdir 
         (dbg^".ml") in
        let hm=Half_dressed_module.of_path_and_root ap (root cs) in
        let cs2=recompute_module_info cs hm in
        let old_tgts=targets cs in
        let tgt=Ocaml_target.debuggable hm in
        make (root cs) (cs2,old_tgts,[]) tgt
    |Compilation_mode_t.Executable->
        let hm=Option.unpack opt_argument in
        let tgt=Ocaml_target.EXECUTABLE hm in 
        let _=make (root cs) (cs,targets cs,[]) tgt in
        let l_cmd=Command_for_ocaml_target.command_for_ocaml_target
         (root cs) cs tgt in
        let _=Image.image Unix_command.hardcore_uc l_cmd in
        (false,(cs,[],[]));; 
  
end;;  


let recompile cs=
     let old_tgts=targets cs in
     let ((_,nms_to_be_updated),short_paths)=
        recompile_on_monitored_modules false cs in
     if nms_to_be_updated=[] then (false,[]) else
     let new_dirs=compute_subdirectories_list cs 
     and new_tgts1=Ocaml_target.still_up_to_date_targets nms_to_be_updated old_tgts in
     let checker=Ocaml_target.test_target_existence (root cs) in
     let new_tgts=List.filter checker new_tgts1 in
     let _=(set_targets cs new_tgts) in
     let (_,(_,new_tgts2,rejected_ones2))=
       Ocaml_target_making.make_final_target
       Compilation_mode_t.Usual None cs  in
      let new_preqt=Image.image(
        fun (hm,_)->(hm,not(List.mem hm rejected_ones2))
      )  (preq_types cs) in   
     let _=(
        set_directories cs new_dirs;
        set_targets cs new_tgts2;
        set_preq_types cs new_preqt;
     )  in
    (true,short_paths);;       

let add_printer_equipped_type cs hm=
  set_preq_types cs ((preq_types cs)@[hm]);;

let remove_printer_equipped_type cs hm=
  set_preq_types cs (List.filter (fun hm2->hm2<>hm) (preq_types cs));;

let uple_form cs=
  (cs,
   directories cs,
   targets cs,
   preq_types cs
   );;

let remove_debuggables x=
  let new_tgts=List.filter Ocaml_target.is_not_a_debuggable
         (targets x)  in 
         set_targets x new_tgts;;
    
let backup x diff opt=
  Alaskan_backup_target_system.backup 
  (root x,backup_dir x) diff opt;;

  let unregister_mlx_file_on_targets root_dir (old_cs,old_tgts) mlx=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
    let new_cs=unregister_mlx_file_on_monitored_modules old_cs mlx in
    let new_dirs=compute_subdirectories_list new_cs
    and new_tgts=List.filter (fun tgt->
       match Ocaml_target.main_module tgt with
       None->false |Some(hm2)->hm2<>hm
    ) old_tgts in
    let _=(set_targets new_cs new_tgts) in
    let (_,(new_cs2,new_tgts2,_))=
       Ocaml_target_making.make_final_target
       Compilation_mode_t.Usual None new_cs  in
    (new_cs2,new_dirs,new_tgts2);;   

exception FileWithDependencies of 
Mlx_ended_absolute_path.t*(Naked_module_t.t list);;


let forget_file_on_targets root_dir triple ap=
  let (cs,dirs,tgts)=triple in
  let hm=Half_dressed_module.of_path_and_root ap root_dir 
  and mlx=Mlx_ended_absolute_path.of_path_and_root ap root_dir  in
  let nm=Half_dressed_module.naked_module hm in
  match seek_module_index  cs nm with
   None->triple
  |Some(_)->
   let bel=below cs (Mlx_ended_absolute_path.half_dressed_core mlx) in
    if bel=[]
    then let s_hm=Half_dressed_module.uprooted_version hm in
         let fn=(Root_directory.connectable_to_subpath(root_dir))^s_hm in
         let _=Image.image
         (fun edg->Unix_command.uc("rm -f "^fn^edg^"*"))
         [".cm";".d.cm";".caml_debuggable"] in
         unregister_mlx_file_on_targets root_dir 
            (cs,tgts) mlx
    else raise(FileWithDependencies(mlx,bel));;



let forget_file x ap=
    let (_,new_dirs,new_tgts)= 
     forget_file_on_targets (root x)
       (x,directories x,targets x) ap in  
        (
          set_directories x new_dirs;
          set_targets x new_tgts;
        );;         

module Unregister_module=struct

let test_for_non_obsolescence (hm,short_paths) tgt=
  match tgt with
  Ocaml_target.NO_DEPENDENCIES(mlx)->
      not(List.mem (Mlx_ended_absolute_path.short_path mlx) short_paths)
  |_->(
    match Ocaml_target.main_module tgt with
    None->false |Some(hm2)->hm2<>hm
      );;    


let on_targets root_dir (old_cs,old_tgts) hm=
    let (new_cs,short_paths)=unregister_module_on_monitored_modules  old_cs hm in
    let new_dirs=compute_subdirectories_list new_cs 
    and new_tgts=List.filter 
     (test_for_non_obsolescence (hm,short_paths) ) old_tgts in
    let _=(set_targets new_cs new_tgts) in
    let (_,(new_cs2,new_tgts2,_))=
        Ocaml_target_making.make_final_target
        Compilation_mode_t.Usual None new_cs  in 
     ((new_cs2,new_dirs,new_tgts2),short_paths);;   
     
   

end;;          
   
exception ModuleWithDependenciesDuringForgetting of 
        Half_dressed_module.t*(Naked_module_t.t list);;
exception Non_registered_module_during_forgetting of Naked_module_t.t;;
      
let forget_module_on_targets root_dir (cs,dirs,tgts) hm=
        let nm=Half_dressed_module.naked_module hm in
        match seek_module_index  cs nm with
         None->raise(Non_registered_module_during_forgetting(nm))
        |Some(dt)->
         let bel=below cs hm in
          if bel=[]
          then let (answer,short_paths)=Unregister_module.on_targets root_dir 
                          (cs,tgts) hm in
               let sfn=Half_dressed_module.to_shortened_string hm in
               let _=Image.image
               (fun edg->
                let cmd="rm -f _build/"^sfn^edg in
                Unix_command.uc(cmd))
               [".cm*";".d.cm*";".caml_debuggable"] in
               let temp1=Image.image (fun t->
                  Absolute_path.of_string(Root_directory.join root_dir t)
               ) short_paths in
               let _=Image.image 
               (German_forget_unregistered_file.forget root_dir) temp1 in
               (answer,short_paths)
          else raise(ModuleWithDependenciesDuringForgetting(hm,bel));;
      

let forget_module x hm=
    let ((_,new_dirs,new_tgts),short_paths)= 
      forget_module_on_targets (root x)
      (x,directories x,targets x) hm in
      let _=(
          set_directories x new_dirs;
          set_targets x new_tgts;
      ) in
      short_paths;;          

let initialize x=
        let s_ap=Root_directory.join (root x)  Coma_constant.name_for_targetfile in
        let ap=Absolute_path.of_string s_ap in
        let the_archive=Io.read_whole_file ap in
        let the_bulky_one=Coma_state_field.unarchive the_archive in
        Coma_state_field.copy_mutables_from x the_bulky_one;;      

module Try_to_register=struct

  let mlx_file mdata mlx_file=
    try(Some(register_mlx_file_on_monitored_modules 
        mdata mlx_file)) with _->None;;  

module Private=struct

exception Pusher_exn;;

let pusher  (vdata,failures,yet_untreated)=
     match yet_untreated with
      []->raise(Pusher_exn)
      |mlx::others->
      (
        match mlx_file vdata mlx with
        None->(vdata,mlx::failures,others)
        |Some(nfs)->(nfs,failures,others)
      );; 

let rec iterator x=
   let (vdata,failures,yet_untreated)=x in
   match yet_untreated with
      []->(failures,vdata)
      |mlx::others->
      (
        match mlx_file vdata mlx with
        None->iterator(pusher x)
        |Some(nfs)->iterator(pusher x)
      );;   

end;;

let mlx_files mdata mlx_files=
   Private.iterator(mdata,[],mlx_files);;
 
  
   


end;;  


module Target_system_creation=struct

  module Private=struct

    let display_circular_dependencies printer l cycles= 
      if cycles=[]
      then ()
      else
      let temp1=Image.image(fun cycle->
        let ttemp1=Image.image (fun j->printer (List.nth l (j-1))) cycle in
         String.concat " -> " ttemp1 
      ) cycles in
      let temp2="\n\n The following cycles have been detected : "^
        (String.concat "\n\n" temp1) in
      (print_string temp2;flush stdout);;
     
    let init_dir=
      Subdirectory.connectable_to_subpath 
      (Coma_constant.kept_up_to_date_but_not_registered);;
    
    let copy_special_files s_main_dir=
      let dname=Coma_constant.name_for_debugged_module in
      let _=Image.image(
       fun s->
        Unix_command.uc 
          ("mkdir -p "^s_main_dir^"/"^(Subdirectory.without_trailing_slash s))
      ) [
           Coma_constant.kept_up_to_date_but_not_registered;
           Coma_constant.temporary;
        ]
      in
      let _=Unix_command.uc ("mkdir -p "^s_main_dir^(Subdirectory.connectable_to_subpath Coma_constant.build_subdir)) in
      let _=Unix_command.uc ("mkdir -p "^s_main_dir^(Subdirectory.connectable_to_subpath Coma_constant.debug_build_subdir)) in
      let _=Unix_command.uc ("mkdir -p "^s_main_dir^(Subdirectory.connectable_to_subpath Coma_constant.exec_build_subdir)) in
      let _=Image.image (fun s->
        Unix_command.uc ("touch "^s_main_dir^"/"^s)
         ) ([dname^".ml";
           ".ocamlinit"]
           @
           Coma_constant.up_to_date_but_not_registered_files
        ) in ();;
    
    let put_default_content_in_special_files s_main_dir=
      (Io.overwrite_with 
      (Absolute_path.of_string (s_main_dir^"/.ocamlinit"))
      (
      "\n#use\""^Coma_constant.path_for_loadingsfile^"\""^Double_semicolon.ds^
      "\n#use\""^Coma_constant.path_for_printersfile^"\""^Double_semicolon.ds^
      "\nopen Needed_values;;"^
      "\ninitialize_toplevel();;"
       );
      Io.overwrite_with 
      (Absolute_path.of_string (s_main_dir^"/"^init_dir^"/my_printers.ml"))
      "\n\n (*Registered printers start here *) \n\n (*Registered printers end here *) \n\n");; 
      
    
let select_good_files s_main_dir=
       let ap1=Absolute_path.of_string s_main_dir in        
       let temp1=More_unix.complete_ls (Directory_name.of_string s_main_dir) in
       let s_ap1=Absolute_path.to_string ap1 in
       let n1=String.length(s_ap1) in
       let dname=Coma_constant.name_for_debugged_module in
       let selector=(
       fun ap->
         let s=Absolute_path.to_string ap in
         let t=Cull_string.cobeginning n1 s in
         (List.exists (fun edg->Substring.ends_with s edg) [".ml";".mli";".mll";".mly"])
         &&
         (List.for_all (fun beg->not(Substring.begins_with t beg)) 
         (Image.image Subdirectory.connectable_to_subpath 
          [
            Coma_constant.kept_up_to_date_but_not_registered;
            Coma_constant.left_out_of_updating;
            Coma_constant.old_and_hardly_reusable;
            Coma_constant.temporary;
            Coma_constant.build_subdir;
            Coma_constant.debug_build_subdir;
            Coma_constant.exec_build_subdir; 
          ]
         ))
         &&
         (* When a mll or mly is present, do not register the ml *)
         (not(
               (Substring.ends_with s ".ml")
               &&
               (List.exists (fun edg->Sys.file_exists(s_ap1^s^edg)) ["l";"y"])
         ))
         &&
         (List.for_all (fun edg->not(Substring.ends_with s edg) ) 
         [".ocamlinit";"executable.ml"]
         )
         &&
         (t<>(dname^".ml"))
       ) in
       List.filter selector temp1;;
       
     let rec detect_identical_names (identical_names,l)=
       match l with 
       []->identical_names
      |(a,b)::others->
         let (temp1,temp2)=List.partition (fun t->snd(t)=b) others in
         if temp1<>[]
         then detect_identical_names(((a,b)::temp1)::identical_names,temp2)
         else detect_identical_names(identical_names,temp2);;  
         
     exception Identical_names of (((string*string) list) list);;    
         
     let clean_list_of_files main_dir l=
      (*
         raises an exception if there are different modules with
         identical names.
         Removes the files outside main_dir.
      *)
      let s_dir=Root_directory.connectable_to_subpath main_dir in
      let temp1=List.filter (fun ap->
        Substring.begins_with (Absolute_path.to_string ap) s_dir
      ) l in
      let temp2=Image.image (fun ap->
        let s=Absolute_path.to_string ap in
        (ap,Father_and_son.son s '/')
      ) temp1 in
      let temp3=detect_identical_names ([],temp2) in
      if temp3<>[]
      then let n1=String.length s_dir in
           let tempf1=(fun (x,y)->
               (Cull_string.cobeginning n1 (Absolute_path.to_string x),y)
            ) in
           let tempf2=Image.image (Image.image tempf1) in
           let temp4=tempf2 temp3 in
           raise(Identical_names(temp4))
      else temp2;;
      
    let compute_dependencies l=
      let temp1=Ennig.index_everything l 
      and n=List.length l in
      let rec tempf=(fun (j1,(ap1,s1))->
        let ttemp1=Look_for_module_names.names_in_file ap1 in
        let ttemp2=Image.image Naked_module.to_string ttemp1 in
        let ttempf=(fun s_nm->
          Option.filter_and_unpack (fun 
          (k,(_,s))->
          if (Father_and_son.father s '.')=s_nm
          then Some(k)
          else None ) temp1
        ) in
        let ttemp3=Image.image ttempf ttemp2 in
        List.flatten  ttemp3
      )  in
      let tempg=(fun x-> let (_,(_,s))=x in
         if Substring.ends_with s ".mli"
         then let t=Cull_string.coending 1 s in
              match Option.seek (fun (_,(_,s1))->s1=t) temp1 with
               None->tempf x
              |Some(y)->tempf y 
         else tempf x
      ) in
      let table_for_coatoms=Image.image tempg temp1 in
      let coat=Memoized.make(fun j->List.nth table_for_coatoms (j-1)) in
      let (cycles,good_list)=
        Reconstruct_linear_poset.reconstruct_linear_poset coat 
        (Ennig.ennig 1 n) in
      let _=display_circular_dependencies
      (fun (j1,(ap1,s1))->s1) temp1 cycles in
      Image.image (fun (j,_)->snd(List.nth temp1 (j-1)) ) good_list;;
      
    let from_prepared_list dir backup_dir l=
       let temp1=Option.filter_and_unpack (fun (ap,s)->
          Mlx_ended_absolute_path.try_from_path_and_root ap dir
       ) l in
       Try_to_register.mlx_files (Coma_state_field.empty_one dir backup_dir) temp1;;
    
    end;;   
    
let from_main_directory dir backup_dir =
      let old_s=Root_directory.connectable_to_subpath(dir) in
      let s_main_dir=Cull_string.coending 1 old_s in (* mind the trailing slash *)
      let _=
        (Private.copy_special_files s_main_dir;
         Private.put_default_content_in_special_files s_main_dir 
        ) in
      let temp1=Private.select_good_files s_main_dir in
        let temp2=Private.clean_list_of_files dir temp1 in
        let temp3=Private.compute_dependencies temp2 in
        let (failures,mdata1)=Private.from_prepared_list dir backup_dir temp3 in
        let pre_preqt=printer_equipped_types_from_data mdata1 in
        let (_,(new_mdata2,new_tgts2,rejected_ones2))=
           Ocaml_target_making.make_final_target
           Compilation_mode_t.Usual None mdata1  in
       let preqt=Image.image (fun hm->(hm,not(List.mem hm rejected_ones2))) pre_preqt in 
       (new_mdata2,new_tgts2,preqt);;
    
    

end;;  

let delchacre_from_scratch (source_dir,dir_for_backup) mdata=
  let temp1=all_mlx_paths mdata in
  let temp3=temp1 in
  let temp4=Image.image (fun ap->Root_directory.cut_beginning 
   source_dir (Absolute_path.to_string ap)) temp3 in
 Prepare_dircopy_update.compute_diff
    (source_dir,temp4) dir_for_backup;;





let refresh x=
      let (new_mdata,new_tgts,new_ptypes)=
        Target_system_creation.from_main_directory 
             (root x)
             (backup_dir x)
         in 
        let new_dirs=compute_subdirectories_list new_mdata in
        let new_diff=delchacre_from_scratch (root x,backup_dir x) new_mdata in
        let _=
        (
          Coma_state_field.copy_mutables_from x new_mdata;
          set_directories x new_dirs;
          set_targets x new_tgts;
          set_preq_types x new_ptypes;
         ) in
         new_diff;; 

module Register_mlx_file=struct

let on_targets (old_mdata,old_dirs,old_tgts) mlx=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
    let new_dir=Half_dressed_module.subdirectory hm in
   let new_mdata=register_mlx_file_on_monitored_modules old_mdata mlx in
   let new_dirs=
   (if List.mem new_dir old_dirs then old_dirs else old_dirs@[new_dir] )
   and new_tgts=
   (*
         The only outdated targets are the targets 
         corresponding to an identical module
         (for example when a mll or mly is added to
         an already registered ml) 
          *)
         List.filter (
          fun tgt->match Ocaml_target.main_module tgt with
                   None->true
                   |Some(hm2)->hm2<>hm
         ) old_tgts
    in
    (new_mdata,new_dirs,new_tgts);; 
   



end;;  


let register_mlx_file cs mlx=
          let (new_cs,new_dirs,new_tgts)= 
          Register_mlx_file.on_targets 
           (cs,directories cs,targets cs) mlx in
         let _=(set_targets new_cs new_tgts) in
         let (_,(_,new_tgts2,_))=
             Ocaml_target_making.make_final_target
             Compilation_mode_t.Usual None new_cs  in     
                
             (
              set_directories cs new_dirs;
              set_targets cs new_tgts2;   
              ) ;;             

let relocate_module_on_targets root_dir (old_cs,old_tgts) old_name new_subdir= 
  let untouched_tgts=List.filter
   (fun tgt->not(Ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_cs [old_name] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_name)) ) old_tgts in
  let (new_cs,(old_files,new_files))=
    relocate_module_on_monitored_modules root_dir old_cs old_name new_subdir in
  let _=(set_targets new_cs untouched_tgts) in
  let (_,(new_cs2,new_tgts2,_))=
        Ocaml_target_making.make_final_target
        Compilation_mode_t.Usual None new_cs  in     
  ((new_cs2,new_tgts2),(old_files,new_files));;   
 

  let register_mlx_file x mlx=
    let (new_cs,new_dirs,new_tgts)= 
    Register_mlx_file.on_targets 
     (x,directories x,targets x) mlx in
   let _=(set_targets new_cs new_tgts) in
   let (_,(_,new_tgts2,_))=
       Ocaml_target_making.make_final_target
       Compilation_mode_t.Usual None new_cs  in     
          
       (
        set_directories x new_dirs;
        set_targets x new_tgts2;   
        ) ;;             

let relocate_module_on_targets root_dir (old_cs,old_tgts) old_name new_subdir= 
  let untouched_tgts=List.filter
  (fun tgt->not(Ingredients_for_ocaml_target.module_dependency_for_ocaml_target
  old_cs [old_name] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_name)) ) old_tgts in
  let (new_cs,(old_files,new_files))=
  relocate_module_on_monitored_modules root_dir old_cs old_name new_subdir in
  let _=(set_targets new_cs untouched_tgts) in
  let (_,(new_cs2,new_tgts2,_))=
    Ocaml_target_making.make_final_target
    Compilation_mode_t.Usual None new_cs  in     
  ((new_cs2,new_tgts2),(old_files,new_files));;   



let relocate_module x old_name new_subdir=
  let ((_,new_tgts),(old_files,new_files))=
    relocate_module_on_targets (root x)
       (x,targets x) 
       old_name new_subdir in
     (
      set_targets x new_tgts; 
     );;    

let rename_directory x (old_subdir,new_subdirname)=
      let _=Rename_endsubdirectory.in_unix_world 
       (root x) (old_subdir,new_subdirname) in
      let pair=(old_subdir,new_subdirname) in
      let _=rename_directory_on_data pair x
         
      and new_dirs=German_rename_directory.on_subdirectories pair 
        (directories x)
      and new_tgts=German_rename_directory.on_up_to_date_targets pair 
        (targets x)
      and new_peqt=German_rename_directory.on_printer_equipped_types pair 
        (preq_types x)
      in
         (
          
          set_directories x new_dirs;
          set_targets x new_tgts;
          set_preq_types x new_peqt;  
         );;   
      
let rename_module_on_targets root_dir (old_cs,old_tgts) old_name new_name= 
  let untouched_tgts=List.filter
   (fun tgt->not(Ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_cs [old_name] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_name)) ) old_tgts in
  let (new_cs,(old_files,new_files))=
     rename_module_on_monitored_modules root_dir old_cs old_name new_name in
  let _=(set_targets new_cs untouched_tgts) in
  let (_,(new_cs2,new_tgts2,_))=
       Ocaml_target_making.make_final_target
       Compilation_mode_t.Usual None new_cs  in
  ((new_cs2,new_tgts2),(old_files,new_files));;   

let rename_module x old_name new_name=
      let ((_,new_tgts),(old_files,new_files))=
        rename_module_on_targets (root x)
          (x,targets x) old_name new_name in  
         (
          set_targets x new_tgts;
         );;    

let pre_start_debugging cs=
  let root_dir=root cs in
  let _=Alaskan_remove_debuggables.rd root_dir cs in
  let dbg=Coma_constant.name_for_debugged_module in
	let rdir=compute_subdirectories_list cs in
	let ap=Find_suitable_ending.find_file_location root_dir rdir 
	     (dbg^".ml") in
	let hm=Half_dressed_module.of_path_and_root ap root_dir in
  let answer=Ocaml_target_making.make_final_target
     Compilation_mode_t.Debug (Some hm) cs in
	let msg=(
	  if (fst answer)
	  then "\n\n Now, start \n\nocamldebug _build/"^dbg^".ocaml_debuggable\n\nin another terminal\n\n"
	  else "\n\n Something went wrong, see above. \n\n"
	) in
	let _=(
	  print_string msg;
	  flush stdout
	) in
	answer;;   
   

let start_debugging cs=
          let (bowl,(_,new_tgts,_))=
            pre_start_debugging cs  in
          if bowl
          then (
            set_targets cs new_tgts 
               )
          else ();;

          
          


let unregister_mlx_file x mlx=
        let (_,new_dirs,new_tgts)= 
          unregister_mlx_file_on_targets (root x)
          (x,targets x) mlx in
          (
              set_directories x new_dirs;
              set_targets x new_tgts;
          ) ;;  




let unregister_module x hm=
        let ((_,new_dirs,new_tgts),short_paths)= 
         Unregister_module.on_targets (root x)
           (x,targets x) hm in
            (
              set_directories x new_dirs;
              set_targets x new_tgts;
            );;        


module Write_makefile=struct
  
  let slice_targets tgts=
    let temp1=Sliced_string.make_aggregates_if_possible 
                     (Separator.of_string " ") 
                     (Image.image Ocaml_target.to_string tgts) in
    Sliced_string.to_string_list temp1;;
  
  let slice_shortened_targets tgts=
    let temp1=Sliced_string.make_aggregates_if_possible 
                     (Separator.of_string " ") 
                     (Image.image Ocaml_target.to_shortened_string tgts) in
    Sliced_string.to_string_list temp1;;
  
  let write_usual_makefile_element main_root mdata tgt=
   let ingrs=Ingredients_for_ocaml_target.ingredients_for_ocaml_target 
      mdata tgt in
   let sliced_ingrs=slice_shortened_targets ingrs in
   let cmds=Command_for_ocaml_target.command_for_ocaml_target 
                         main_root mdata tgt in
   let s1=(Ocaml_target.to_shortened_string tgt)^" : " 
   and s2=String.concat " \\\n" sliced_ingrs
   and s3="\n\t"
   and s4=String.concat "\n\t" cmds in
   String.concat "" [s1;s2;s3;s4];;
   
  let write_full_compilation_makefile_element  cs=
    let main_root=root cs in
    let l=all_modules cs in
    let temp1=Image.image 
    (Ingredients_for_ocaml_target.ingredients_for_usual_element cs) l in
    let ingrs=Preserve_initial_ordering.preserve_initial_ordering temp1 in
    let sliced_ingrs=slice_shortened_targets ingrs in
    let l_idx=Image.image (fun hm->
      let nm=Half_dressed_module.naked_module hm in
      find_module_index cs nm) l  in
    let s_root=Root_directory.connectable_to_subpath(main_root) in
    let long_temp4=Image.image (fun idx->
               let hm=hm_at_idx cs idx in
               let s_hm=(Half_dressed_module.uprooted_version hm) in
               let short_s_hm=Father_and_son.son s_hm '/' in
               let ml_reg=check_ending_in_at_idx Ocaml_ending.ml cs idx in
               if ml_reg
               then s_root^"_build/"^short_s_hm^".cmo"
               else " "
    ) l_idx in   
    let long_s_lhm=String.concat " " long_temp4 in
    let dirs_and_libs=needed_dirs_and_libs_for_several false cs l_idx in
    let cmds=[ "ocamlmktop "^dirs_and_libs^" -o "^s_root^"ecaml "^long_s_lhm^" ";
            "mv "^s_root^"ecaml "^s_root^"_build/"] in
    let s1="ecaml : " 
    and s2=String.concat " \\\n" sliced_ingrs
    and s3="\n\t"
    and s4=String.concat "\n\t" cmds in
    String.concat "" [s1;s2;s3;s4];; 
   
  let write_makefile cs=
   let main_root=root cs in 
   let temp1=usual_targets cs in
   let temp2=Image.image (write_usual_makefile_element main_root cs) temp1 in
   let temp3=temp2@[write_full_compilation_makefile_element cs] in
   let temp5=slice_targets  temp1 in
   let temp6=String.concat " \\\n" temp5 in
   let temp7="clean :\n\trm -r -f "^temp6^"\n\n" in
   String.concat "\n\n" (temp3@[temp7]);;
  
     

end;;


module Save_all=struct

  module Private=struct

    let save_makefile (root,location_for_makefile) mdata=
      let s1="# This makefile was automatocally written by\n"^
      "# the write_makefile function in the ml_manager module. \n\n"^
      (Write_makefile.write_makefile mdata) in
      let lm=Root_directory.force_join root location_for_makefile in
      Io.overwrite_with (Absolute_path.of_string lm) s1;;
    
  
    let save_loadingsfile (root,location_for_loadingsfile) (dirs,tgts)=
       let path_for_loadingsfile=
           (Subdirectory.connectable_to_subpath Coma_constant.kept_up_to_date_but_not_registered)^
           location_for_loadingsfile in
       let s=Alaskan_up_to_date_targets.loadings (root,location_for_loadingsfile)
        (dirs,tgts)
       and lm=Root_directory.force_join root  path_for_loadingsfile in
       Io.overwrite_with (Absolute_path.of_string lm) s;;
    
    let save_merlinfile (root,location_for_merlinfile) dirs=
        let s=Alaskan_write_merlinfile.instructions root dirs 
        and lm=Root_directory.force_join root  location_for_merlinfile in
        Io.overwrite_with (Absolute_path.of_string lm) s;;
  
    let save_printersfile (root,location_for_printersfile) printer_equipped_types=
       let init_dir=
        Subdirectory.connectable_to_subpath 
        (Coma_constant.kept_up_to_date_but_not_registered) in
       let s=Alaskan_printer_equipped_types.instructions printer_equipped_types
       and lm=Root_directory.force_join root  (init_dir^location_for_printersfile) in
       let beg_mark="(*Registered printers start here *)"
       and end_mark="(*Registered printers end here *)" in
       Replace_inside.overwrite_between_markers_inside_file
       (Overwriter.of_string s)
       (beg_mark,end_mark)
       (Absolute_path.of_string lm);;
    
    
  
    let save_targetfile (root,location_for_targetfile) mdata=
      let s1=Coma_state_field.archive mdata in
      let lt=Root_directory.force_join root location_for_targetfile in
      Io.overwrite_with (Absolute_path.of_string lt) s1;;
    
    end;;
    

    
    
    let write_all 
    (root,
      location_for_makefile,
      location_for_targetfile,
      location_for_loadingsfile,
      location_for_printersfile
      )
      uple= 
      let (mdata,directories,up_to_date_targets,printer_equipped_types)=uple in
       (
        Private.save_makefile (root,location_for_makefile) mdata;
        Private.save_merlinfile (root,Coma_constant.name_for_merlinfile) directories;
        Private.save_loadingsfile (root,location_for_loadingsfile) (directories,up_to_date_targets);
        Private.save_targetfile (root,location_for_targetfile) mdata;
        Private.save_printersfile (root,location_for_printersfile) printer_equipped_types;
       );;
    
    
  

end;;  


module Create_or_update_copied_compiler=struct

  let prepare mdata (sourcedir,destdir)=
    let l1=all_short_paths mdata in
    let main_diff=Prepare_dircopy_update.compute_diff 
          (sourcedir,l1) destdir in
    Prepare_dircopy_update.commands_for_update (sourcedir,destdir) main_diff;;
  
  let file_for_backup="Country/Alaska/alaskan_backup_target_system.ml";;
  
  let replacement_for_special_file (sourcedir,destdir) filename=
    if filename=file_for_backup
    then ("let github_after_backup=ref(true)"^Double_semicolon.ds,
          "let github_after_backup=ref(false)"^Double_semicolon.ds)
    else (Root_directory.without_trailing_slash sourcedir,
          Root_directory.without_trailing_slash destdir);;
  
  let prepare_special_file (sourcedir,destdir) filename=
    let the_file=Absolute_path.create_file(Root_directory.join destdir filename) in
    Replace_inside.replace_inside_file
    (replacement_for_special_file (sourcedir,destdir) filename)
    the_file;;
  
  let init_dir=
      Subdirectory.connectable_to_subpath 
      (Coma_constant.kept_up_to_date_but_not_registered);;
  
  let up_to_date_but_not_registered_files=
     [
        Coma_constant.path_for_loadingsfile;
        Coma_constant.path_for_printersfile;
     ];;
  
  let ucc mdata (sourcedir,destdir,backup_dir)=
    let knr=Subdirectory.without_trailing_slash(Coma_constant.kept_up_to_date_but_not_registered) in
    let s_dir=Root_directory.connectable_to_subpath destdir in 
    let _=Unix_command.uc ("mkdir -p "^s_dir^(Subdirectory.connectable_to_subpath Coma_constant.build_subdir)) in
    let _=Unix_command.uc ("mkdir -p "^s_dir^(Subdirectory.connectable_to_subpath Coma_constant.exec_build_subdir)) in
    let _=Unix_command.uc ("mkdir -p "^s_dir^(Subdirectory.connectable_to_subpath Coma_constant.debug_build_subdir)) in
    let _=Unix_command.uc ("mkdir -p "^s_dir^knr) in
    let _=Image.image (
        fun s->Unix_command.uc("touch "^s_dir^s)
    ) up_to_date_but_not_registered_files in
    let _=Image.image Unix_command.uc (prepare mdata (sourcedir,destdir)) in
    let _=Image.image (prepare_special_file (sourcedir,destdir))
      (
        up_to_date_but_not_registered_files@
      ["Country/Germany/german_wrapper.ml";file_for_backup]
      ) 
     in 
    Target_system_creation.from_main_directory destdir backup_dir;;
         
         
  
  
  
  
  
  
     
     
      

end;;  
