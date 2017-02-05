(*

#use"Country/Germany/german_mutable_target_system.ml";;

*)



let discreet_self_update tolerate_cycles 
   (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=
   let (old_mdata,old_dirs,old_tgts)=(!mdata_ref,!dirs_ref,!tgts_ref) in 
   let opt=German_up_to_date_targets.self_update 
   		tolerate_cycles (old_mdata,old_dirs,old_tgts) in
   if opt=None
   then None
   else let (new_mdata,new_dirs,new_tgts)=Option.unpack opt in
        let _=(
        mdata_ref:=new_mdata;
        dirs_ref:=new_dirs;
        tgts_ref:=new_tgts;
        German_save_all.write_all 
        (!mdata_ref,!dirs_ref,!tgts_ref,
         !ofiles_ref,!odirs_ref,
         !rdel_ref,!ptypes_ref)
        ) 
        and d12=German_compare_two_target_systems.compare 
            (old_mdata,old_dirs,old_tgts) (new_mdata,new_dirs,new_tgts) in
        Some(d12);;


let unit_discreet_self_update tolerate_cycles mts= 
  let _=  discreet_self_update tolerate_cycles mts in ();;     

let semi_discreet_self_update tolerate_cycles mts=
   match discreet_self_update tolerate_cycles mts with
   (Some d12)-> print_string(German_compare_two_target_systems.display d12)
   |None->();;

let self_update tolerate_cycles mts=
   match discreet_self_update tolerate_cycles mts with
   (Some d12)-> print_string(German_compare_two_target_systems.display d12)
   |None->(print_string("Nothing new in the source files. \n");flush stdout);;


let machen mts tgt=
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let (old_mdata,old_tgts)=(!mdata_ref,!tgts_ref) in  
   let (new_mdata,new_tgts)=snd(German_make_ocaml_target.make (old_mdata,old_tgts) tgt) in
   (
        mdata_ref:=new_mdata;
        tgts_ref:=new_tgts;
        German_save_all.write_all 
        (!mdata_ref,!dirs_ref,!tgts_ref,
         !ofiles_ref,!odirs_ref,
         !rdel_ref,!ptypes_ref)
    ) ;;


let force_remake mts=
  let (mdata_ref,_,_,_,_,_,_)=mts in 
  (
   machen mts (German_data.default_toplevel (!mdata_ref));
   );;

let recompile mts=
  let opt=discreet_self_update false mts in
  if opt=None
  then ()
  else force_remake mts;;
  

  
let register mts mlx=
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let (old_mdata,old_dirs,old_tgts)=(!mdata_ref,!dirs_ref,!tgts_ref) in 
   let (new_mdata,new_dirs,new_tgts)=German_up_to_date_targets.register_mlx_file 
   		(old_mdata,old_dirs,old_tgts) mlx in 
   (
        mdata_ref:=new_mdata;
        dirs_ref:=new_dirs;
        tgts_ref:=new_tgts;
        force_remake mts
    ) ;;
   
let unregister mts mlx=
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let (old_mdata,old_dirs,old_tgts)=(!mdata_ref,!dirs_ref,!tgts_ref) in 
   let (new_mdata,new_dirs,new_tgts)=German_up_to_date_targets.unregister_mlx_file 
   		(old_mdata,old_tgts) mlx in 
   (
        mdata_ref:=new_mdata;
        dirs_ref:=new_dirs;
        tgts_ref:=new_tgts;
        force_remake mts
    ) ;;
      
let unregister_module mts hm=
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let (old_mdata,old_dirs,old_tgts)=(!mdata_ref,!dirs_ref,!tgts_ref) in 
   let (new_mdata,new_dirs,new_tgts)=German_up_to_date_targets.unregister_module
   		(old_mdata,old_tgts) hm in 
   (
        mdata_ref:=new_mdata;
        dirs_ref:=new_dirs;
        tgts_ref:=new_tgts;
        force_remake mts
    ) ;;      

let register_outside_file mts ap=
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let (new_ofiles,new_odirs)=German_outside_directories.register_outside_file
   		(!ofiles_ref,!odirs_ref) ap in 
   (
        ofiles_ref:=new_ofiles;
        odirs_ref:=new_odirs;
    ) ;;        
  
  
let unregister_outside_file mts ap=
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let (new_ofiles,new_odirs)=German_outside_directories.unregister_outside_file
   		(!ofiles_ref,!odirs_ref) ap in 
   (
        ofiles_ref:=new_ofiles;
        odirs_ref:=new_odirs;
    ) ;;        
  
let declare_printer mts hm=
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let new_ptypes=German_printer_equipped_types.declare_printer
   		hm (!ptypes_ref)  in 
    (
        ptypes_ref:=new_ptypes;
    ) ;;     

let undeclare_printer mts hm=
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let new_ptypes=German_printer_equipped_types.undeclare_printer
   		hm (!ptypes_ref)  in 
    (
        ptypes_ref:=new_ptypes;
    ) ;;   
  



let reposition mts mlx (l_before,l_after)=
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let new_mdata=German_modify_modulesystem.reposition (!mdata_ref) mlx (l_before,l_after) in
   (mdata_ref:=new_mdata;
    recompile mts);;


let rename_module mts name_before name_after=
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let _=unit_discreet_self_update false mts in
   let (old_mdata,old_dirs,old_tgts)=(!mdata_ref,!dirs_ref,!tgts_ref) in 
   let (new_mdata,new_dirs,new_tgts)=German_up_to_date_targets.rename_module
   		(old_mdata,old_dirs,old_tgts) name_before name_after in 
   (
        mdata_ref:=new_mdata;
        dirs_ref:=new_dirs;
        tgts_ref:=new_tgts;
        force_remake mts
    ) ;;   
   
let relocate_module mts hm new_dir=
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let _=unit_discreet_self_update false mts in
   let (old_mdata,old_tgts)=(!mdata_ref,!tgts_ref) in 
   let (new_mdata,new_dirs,new_tgts)=German_up_to_date_targets.relocate_module
   		(old_mdata,old_tgts) hm new_dir in 
   (
        mdata_ref:=new_mdata;
        dirs_ref:=new_dirs;
        tgts_ref:=new_tgts;
        force_remake mts
    ) ;;      

let make_module_optional mts hm =
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let _=unit_discreet_self_update false mts in
   let (old_mdata,old_tgts)=(!mdata_ref,!tgts_ref) in 
   let (new_mdata,new_dirs,new_tgts)=German_up_to_date_targets.make_module_optional
   		(old_mdata,old_tgts) hm in 
   (
        mdata_ref:=new_mdata;
        dirs_ref:=new_dirs;
        tgts_ref:=new_tgts;
        force_remake mts
    ) ;;      
      




    
let forget_unregistered_file mts ap=
   let s_dir=Directory_name.to_string(German_constant.root) in
   let n_dir=String.length s_dir in
   let s_ap=Absolute_path.to_string ap in
   let subpath=Cull_string.cobeginning n_dir s_ap in
   let new_subpath=(Current_date.current_date())^"_"^
         (Replace_inside.replace_inside_string ("/","_dir_") subpath) in
   let _=Sys.command ("mkdir -p "^s_dir^"Forgotten") in
   let _=Sys.command ("touch "^s_dir^"Forgotten/"^new_subpath) in
   let cmd="mv "^s_ap^" "^s_dir^"Forgotten/"^new_subpath in
   let _=Shell_command.do_and_notice_failure cmd in 
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   (rdel_ref:=subpath::(!rdel_ref);
     German_save_all.write_all 
        (!mdata_ref,!dirs_ref,!tgts_ref,
         !ofiles_ref,!odirs_ref,
         !rdel_ref,!ptypes_ref));;


exception FileWithDependencies of 
	Mlx_filename.t*(Half_dressed_module.t list);;


let forget_file mts ap=
  let root=German_constant.root in
  let hm=Half_dressed_module.of_path_and_root ap root 
  and mlx=Mlx_filename.of_path_and_root ap root  in
  let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
  match German_data.find_module_registration (!mdata_ref) hm with
   None->forget_unregistered_file mts ap
  |Some(_)->
   let bel=German_data.below (!mdata_ref) (Mlx_filename.half_dressed_core mlx) in
    if bel=[]
    then let s_hm=Half_dressed_module.to_string hm in
         let fn=(Directory_name.to_string(root))^s_hm in
         let _=Image.image
         (fun edg->Shell_command.do_and_notice_failure("rm -f "^fn^edg^"*"))
         [".cm";".d.cm";".caml_debuggable"] in
         (unregister mts mlx;forget_unregistered_file mts ap)
    else raise(FileWithDependencies(mlx,bel));;


exception ModuleWithDependencies of 
	Half_dressed_module.t*(Half_dressed_module.t list);;
exception Non_registered_module of Half_dressed_module.t;;

let forget_module mts hm=
  let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
  match German_data.find_module_registration (!mdata_ref) hm with
   None->raise(Non_registered_module(hm))
  |Some(dt)->
   let bel=German_data.below (!mdata_ref) hm in
    if bel=[]
    then let s_hm=Half_dressed_module.to_string hm in
         let fn=(Directory_name.to_string(German_constant.root))^s_hm in
         let _=Image.image
         (fun edg->Shell_command.do_and_notice_failure("rm -f "^fn^edg^"*"))
         [".cm";".d.cm";".caml_debuggable"] in
         let edgs=Modulesystem_data.registered_endings dt in
         let paths=Image.image (fun edg->Mlx_filename.to_path(Mlx_filename.join hm edg)) edgs in
         (unregister_module mts hm;
         List.iter (forget_unregistered_file mts) paths)
    else raise(ModuleWithDependencies(hm,bel));;



   
let recompute_module_info mts hm=
  let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
  let new_mdata=German_modify_modulesystem.recompute_module_info (!mdata_ref) hm in
  mdata_ref:=new_mdata;;
      


let system_depth mts=
   let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
   let temp1=Image.image Subdirectory.depth (!dirs_ref) in
   Max.list temp1;;
   
 
let remove_debuggables mts=
   let d=system_depth mts 
   and sroot=Directory_name.to_string(German_constant.root) in
   let tempf=(fun j->
   let stars=String.concat "" (Ennig.doyle (fun t->"*/") 1 j) in
   sroot^stars^"*.d.cm*"
   ) in
   let temp1=String.concat " " (Ennig.doyle tempf 0 d) in
   Shell_command.do_and_notice_failure("rm -f "^temp1);;
   
  


let refresh mts=
  let (mdata_ref,dirs_ref,tgts_ref,
    ofiles_ref,odirs_ref,
    rdel_ref,ptypes_ref)=mts in
  let (new_mdata,new_tgts,new_ptypes)=snd(German_create_target_system.from_main_directory ()) in 
  (
    mdata_ref:=new_mdata;
    tgts_ref:=new_tgts;
    ptypes_ref:=new_ptypes;
    German_save_all.write_all 
        (!mdata_ref,!dirs_ref,!tgts_ref,
         !ofiles_ref,!odirs_ref,
         !rdel_ref,!ptypes_ref));;



