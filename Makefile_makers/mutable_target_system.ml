(*

#use"Makefile_makers/mutable_target_system.ml";;

*)



type t={
   content : Target_system.t ref;
   location_for_makefile : string;
   location_for_targetfile : string;
   location_for_loadingsfile : string;
   location_for_pervasivesfile : string;
   location_for_printersfile : string;
   
};;

let makefile_counter=ref 0;;
let targetfile_counter=ref 0;;
let loadingsfile_counter=ref 0;;
let pervasivesfile_counter=ref 0;;
let printersfile_counter=ref 0;;

let make ts opt_makefile opt_targetfile opt_loadingsfile opt_pervasivesfile opt_printersfile=
   let makefile_location=(
     match opt_makefile with
     None->let m=(!makefile_counter)+1 in
           (
            makefile_counter:=m;
            "makefile"^(string_of_int(m))
            )
     |Some(s)->s       
   ) and targetfile_location=(
     match opt_targetfile with
     None->let m=(!targetfile_counter)+1 in
           (
            targetfile_counter:=m;
            "targetfile"^(string_of_int(m))
            )
     |Some(s)->s       
   ) and loadingsfile_location=(
     match opt_loadingsfile with
     None->let m=(!loadingsfile_counter)+1 in
           (
            loadingsfile_counter:=m;
            "loadingsfile"^(string_of_int(m))
            )
     |Some(s)->s       
   ) and pervasivesfile_location=(
     match opt_pervasivesfile with
     None->let m=(!pervasivesfile_counter)+1 in
           (
            pervasivesfile_counter:=m;
            "my_pervasives"^(string_of_int(m))^".ml"
            )
     |Some(s)->s       
   ) and printersfile_location=(
     match opt_printersfile with
     None->let m=(!printersfile_counter)+1 in
           (
            printersfile_counter:=m;
            "printersfile"^(string_of_int(m))
            )
     |Some(s)->s       
   )
   in

{
   content=ref(ts);
   location_for_makefile=makefile_location;
   location_for_targetfile=targetfile_location;
   location_for_loadingsfile=loadingsfile_location;
   location_for_pervasivesfile=pervasivesfile_location;
   location_for_printersfile=printersfile_location;
};;


let target_system mts=(!(mts.content));;
let modulesystem mts=Target_system.modulesystem (!(mts.content));;
let directories mts=Target_system.directories (!(mts.content));;
let up_to_date_targets mts=Target_system.up_to_date_targets (!(mts.content));;
let root mts=Target_system.root (!(mts.content));;
let printer_equipped_types mts=Target_system.printer_equipped_types (!(mts.content));;
let find_module_registration mts mlx=Target_system.find_module_registration (!(mts.content)) mlx;;
let all_modules mts=Target_system.all_modules (!(mts.content));;
let all_filedata mts=Modulesystem.all_filedata (modulesystem mts);;
let all_mlx_files mts=Target_system.all_mlx_files (!(mts.content));;
let location_for_makefile mts=mts.location_for_makefile;;
let location_for_targetfile mts=mts.location_for_targetfile;;
let location_for_loadingsfile mts=mts.location_for_loadingsfile;;
let location_for_pervasivesfile mts=mts.location_for_pervasivesfile;;
let location_for_printersfile mts=mts.location_for_printersfile;;

let absolute_location_for_makefile mts=
  Directory_name.force_join (root mts) 
  (mts.location_for_makefile);;

let absolute_location_for_targetfile mts=
  Directory_name.force_join (root mts) 
  (mts.location_for_targetfile);;

let absolute_location_for_loadingsfile mts=
  Directory_name.force_join (root mts) 
  (mts.location_for_loadingsfile);;

let absolute_location_for_pervasivesfile mts=
  Directory_name.force_join (root mts) 
  (mts.location_for_pervasivesfile);;

let absolute_location_for_printersfile mts=
  Directory_name.force_join (root mts) 
  (mts.location_for_printersfile);;

let from_modulesystem fs opt1 opt2 opt3 opt4=
   make (Target_system.from_modulesystem fs) opt1 opt2 opt3 opt4;;


let unregistered_mlx_files mts=Target_system.unregistered_mlx_files (!(mts.content));; 


 
let change_content mts ts=(mts.content:=ts);;



let loadings mts=
  let s_root=Directory_name.to_string(root mts) in
  let part1="\n(*\n #use\""^s_root^(mts.location_for_loadingsfile)^"\";;\n*)\n\n" in
  let temp5=Image.image(
     fun sd->
     "#directory\""^s_root^(Subdirectory.to_string sd)^"\";;"
  ) (directories mts) in
  let part2=String.concat "\n" temp5 
  and part3="\n\n#load\"nums.cma\";;\n#load\"str.cma\";;\n#load\"unix.cma\";;\n\n\n" in
  let temp1=up_to_date_targets mts in
  let temp2=Option.filter_and_unpack (
    function (Ocaml_target.CMO(x))->
      Some("#load\""^(Half_dressed_module.to_string x)^".cmo\";;") 
    |_->None
  ) temp1 in
  let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
  let part4=String.concat "\n" temp3 in
  part1^part2^part3^part4;; 
 
let printers mts=
  let temp1=printer_equipped_types mts in
  let temp2=List.rev_map (
    function x->
      "#install_printer "^(Half_dressed_module.module_name x)^".print_out;;"
  ) temp1 in
  let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
  let part2=String.concat "\n" temp3 in
  part2;;  
 
 
let save_makefile mts=
  let cs=modulesystem mts in
  let s1="# This makefile was automatocally written by\n"^
  "# the write_makefile function in the ml_manager module. \n\n"^
  (Write_makefile.write_makefile cs) in
  let lm=absolute_location_for_makefile mts in
  Io.erase_file_and_fill_it_with_string (Absolute_path.of_string lm) s1;;

let save_loadingsfile mts=
   let s=loadings mts
   and lm=absolute_location_for_loadingsfile mts in
   Io.erase_file_and_fill_it_with_string (Absolute_path.of_string lm) s;;

let save_printersfile mts=
   let s=printers mts
   and lm=absolute_location_for_printersfile mts in
   let beg_mark="(*Registered printers start here *)"
   and end_mark="(*Registered printers end here *)" in
   Replace_inside.overwrite_between_markers_inside_file
   (Overwriter.of_string s)
   (beg_mark,end_mark)
   (Absolute_path.of_string lm);;

let save_targetfile mts=
  let ts=target_system mts in
  let s1=Target_system.archive ts in
  let lt=absolute_location_for_targetfile mts in
  Io.erase_file_and_fill_it_with_string (Absolute_path.of_string lt) s1;;

let save_all mts=
   (save_makefile mts;
   save_loadingsfile mts;
   save_targetfile mts;
   save_printersfile mts;);;

let discreet_self_update tolerate_cycles mts=
   let ts=target_system mts in
   let (change_exists,ts2)=Modify_target_system.self_update tolerate_cycles ts in
   if change_exists
   then let _=(mts.content:=ts2;save_all mts) 
        and d12=Compare_two_target_systems.compare ts ts2 in
        Some(d12)
   else None;;


let unit_discreet_self_update tolerate_cycles mts= 
  let _=  discreet_self_update tolerate_cycles mts in ();;     

let semi_discreet_self_update tolerate_cycles mts=
   match discreet_self_update tolerate_cycles mts with
   (Some d12)-> print_string(Compare_two_target_systems.display d12)
   |None->();;

let self_update tolerate_cycles mts=
   match discreet_self_update tolerate_cycles mts with
   (Some d12)-> print_string(Compare_two_target_systems.display d12)
   |None->(print_string("Nothing new in the source files. \n");flush stdout);;

let machen mts tgt=
   let ts=target_system mts in
   let ts2=snd(Make_ocaml_target.make ts tgt) in
   (mts.content:=ts2;save_all mts);;

let usual_toplevel mts=
  Toplevel_from_modulesystem.toplevel_from_modulesystem (modulesystem mts);;


let force_remake mts=
  (
   machen mts (usual_toplevel mts);
   save_all mts
   );;

let recompile mts=
  let opt=discreet_self_update false mts in
  if opt=None
  then ()
  else force_remake mts;;

let compile_acyclic_part mts=
  let opt=discreet_self_update true mts in
  if opt=None
  then ()
  else force_remake mts;;
  
let reset_inactivity_counts mts=
   let ts=(!(mts.content)) in
   (
   mts.content:=(Modify_target_system.reset_inactivity_counts ts);
   save_all mts
   );;
  
let register mts mlx=
   let ts=target_system mts in
   let ts2=Modify_target_system.register_mlx_file ts mlx in
   (mts.content:=ts2;force_remake mts);;


let unregister mts mlx=
   let ts=target_system mts in
   let ts2=Modify_target_system.unregister_mlx_file ts mlx in
   (mts.content:=ts2;force_remake mts);;

let unregister_module mts hm=
   let ts=target_system mts in
   let ts2=Modify_target_system.unregister_module ts hm in
   (mts.content:=ts2;force_remake mts);;

let register_outside_file mts ap=
   let ts=target_system mts in
   let ts2=Modify_target_system.register_outside_file ts ap in
   (mts.content:=ts2);;

let unregister_outside_file mts ap=
   let ts=target_system mts in
   let ts2=Modify_target_system.unregister_outside_file ts ap in
   (mts.content:=ts2);;

let declare_printer mts hm=
   let ts=target_system mts in
   let ts2=Modify_target_system.declare_printer ts hm in
   (mts.content:=ts2);;

let undeclare_printer mts hm=
   let ts=target_system mts in
   let ts2=Modify_target_system.undeclare_printer ts hm in
   (mts.content:=ts2);;

let unregistered_mlx_files mts=
  let ts=target_system mts in
  Target_system.unregistered_mlx_files ts;;  

let reposition mts mlx (l_before,l_after)=
   let ts=target_system mts in
   let ts2=Modify_target_system.reposition ts mlx (l_before,l_after) in
   (mts.content:=ts2;recompile mts);;

let rename_module mts name_before name_after=
   let _=unit_discreet_self_update false mts in
   let ts=target_system mts in
   let ts2=Modify_target_system.rename_module ts name_before name_after in
   (mts.content:=ts2;force_remake mts);;

let relocate_module mts hm new_dir=
   let _=unit_discreet_self_update false mts in
   let ts=target_system mts in
   let ts2=Modify_target_system.relocate_module ts hm new_dir in
   (mts.content:=ts2;force_remake mts);;

let make_module_optional mts hm=
   let _=unit_discreet_self_update false mts in
   let ts=target_system mts in
   let ts2=Modify_target_system.make_module_optional ts hm in
   (mts.content:=ts2;force_remake mts);;

exception Non_registered_module of Half_dressed_module.t;;

let above mts hm=Modulesystem.above (modulesystem mts) hm;;
let below mts hm=Modulesystem.below (modulesystem mts) hm;;
let directly_below mts hm=Modulesystem.directly_below (modulesystem mts) hm;;   

    
let forget_unregistered_file mts ap=
   let s_dir=Directory_name.to_string(root mts) in
   let n_dir=String.length s_dir in
   let s_ap=Absolute_path.to_string ap in
   let subpath=Cull_string.cobeginning n_dir s_ap in
   let new_subpath=(Current_date.current_date())^"_"^
         (Replace_inside.replace_inside_string ("/","_dir_") subpath) in
   let _=Sys.command ("mkdir -p "^s_dir^"Forgotten") in
   let _=Sys.command ("touch "^s_dir^"Forgotten/"^new_subpath) in
   let cmd="mv "^s_ap^" "^s_dir^"Forgotten/"^new_subpath in
   let _=Shell_command.do_and_notice_failure cmd in 
   let ts=target_system mts in
   let ts2=Target_system.make
    (Target_system.modulesystem ts)
    (Target_system.directories ts)
    (Target_system.up_to_date_targets ts)
    (Target_system.outside_directories ts)
    (Target_system.outside_files ts)
    (subpath::(Target_system.recently_deleted ts))
    (Target_system.printer_equipped_types ts)
     in
   (mts.content:=ts2;save_all mts);;


exception FileWithDependencies of 
	Mlx_filename.t*(Half_dressed_module.t list);;


let forget_file mts ap=
  let hm=Half_dressed_module.of_path_and_root ap (root mts) 
  and mlx=Mlx_filename.of_path_and_root ap (root mts)  in
  match Modulesystem.find_module_registration (modulesystem mts) hm with
   None->forget_unregistered_file mts ap
  |Some(_)->
   let bel=below mts (Mlx_filename.half_dressed_core mlx) in
    if bel=[]
    then let s_hm=Half_dressed_module.to_string hm in
         let fn=(Directory_name.to_string(root mts))^s_hm in
         let _=Image.image
         (fun edg->Shell_command.do_and_notice_failure("rm -f "^fn^edg^"*"))
         [".cm";".d.cm";".caml_debuggable"] in
         (unregister mts mlx;forget_unregistered_file mts ap)
    else raise(FileWithDependencies(mlx,bel));;

exception ModuleWithDependencies of 
	Half_dressed_module.t*(Half_dressed_module.t list);;

let forget_module mts hm=
  match Modulesystem.find_module_registration (modulesystem mts) hm with
   None->raise(Non_registered_module(hm))
  |Some(dt)->
   let bel=below mts hm in
    if bel=[]
    then let s_hm=Half_dressed_module.to_string hm in
         let fn=(Directory_name.to_string(root mts))^s_hm in
         let _=Image.image
         (fun edg->Shell_command.do_and_notice_failure("rm -f "^fn^edg^"*"))
         [".cm";".d.cm";".caml_debuggable"] in
         let edgs=Modulesystem_data.registered_endings dt in
         let paths=Image.image (fun edg->Mlx_filename.to_path(Mlx_filename.join hm edg)) edgs in
         (unregister_module mts hm;
         List.iter (forget_unregistered_file mts) paths)
    else raise(ModuleWithDependencies(hm,bel));;

let unregistered_ml_files mts=
   let temp1=Mlx_filename.complete_ls (root mts) in
   let temp2=List.filter (fun mlx->
   if (Mlx_filename.ending mlx)=Ocaml_ending.ml
   then not(Modulesystem.see_if_file_is_registered (modulesystem mts) mlx)
   else false) temp1 in
   temp2;;

let register_outside_file mts ap=
  let ts=(!(mts.content)) in
  let ts2=Modify_target_system.register_outside_file ts ap in
  change_content mts ts2;;

let unregister_outside_file mts ap=
  let ts=(!(mts.content)) in
  let ts2=Modify_target_system.unregister_outside_file ts ap in
  change_content mts ts2;;
   
let recompute_module_info mts hm=
  let ts=(!(mts.content)) in
  let ts2=Modify_target_system.recompute_module_info ts hm in
  change_content mts ts2;;
      
   
let system_size mts=Target_system.system_size(!(mts.content));; 

let system_depth mts=
   let temp1=Image.image Subdirectory.depth (directories mts) in
   Max.list temp1;;
   
let remove_debuggables mts=
   let d=system_depth mts 
   and sroot=Directory_name.to_string(root mts) in
   let tempf=(fun j->
   let stars=String.concat "" (Ennig.doyle (fun t->"*/") 1 j) in
   sroot^stars^"*.d.cm*"
   ) in
   let temp1=String.concat " " (Ennig.doyle tempf 0 d) in
   Shell_command.do_and_notice_failure("rm -f "^temp1);;
   
   
let deletable_files mts=
  let temp1=all_filedata mts  in
  let temp2=Image.image Modulesystem_data.name temp1 in
  let temp3=List.filter (
    fun hm->below mts hm=[]
  ) temp2 in
  List.partition (fun hm->
    let s_hm=Half_dressed_module.to_string hm in
    not(Substring.begins_with s_hm "Optional/")
   ) temp3;;   
   

let inactivity_report mts=Modulesystem.inactivity_report (modulesystem mts);;

let refresh mts=
  let temp1=Create_target_system.from_main_directory (root mts) in 
  (mts.content:=(snd temp1); 
   save_all mts);;



