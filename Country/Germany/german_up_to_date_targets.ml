
(* 

#use"Country/Germany/german_up_to_date_targets.ml";;


*)


let loadings (dirs,tgts)=
  let s_root=Directory_name.to_string(German_constant.root) in
  let part1="\n(*\n #use\""^s_root^(German_constant.location_for_loadingsfile)^"\";;\n*)\n\n" in
  let temp5=Image.image(
     fun sd->
     "#directory\""^s_root^(Subdirectory.to_string sd)^"\";;"
  ) dirs in
  let part2=String.concat "\n" temp5 
  and part3="\n\n#load\"nums.cma\";;\n#load\"str.cma\";;\n#load\"unix.cma\";;\n\n\n" in
  let temp2=Option.filter_and_unpack (
    function (Ocaml_target.CMO(x))->
      Some("#load\""^(Half_dressed_module.to_string x)^".cmo\";;") 
    |_->None
  ) tgts in
  let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
  let part4=String.concat "\n" temp3 in
  part1^part2^part3^part4;; 
  
let unregister_mlx_file (old_mdata,old_tgts) mlx=
  let hm=Mlx_filename.half_dressed_core mlx in
  let new_mdata=German_modify_modulesystem.unregister_mlx_file old_mdata mlx in
  let new_dirs=German_directories.from_data new_mdata
  and new_tgts=List.filter (fun tgt->
   	match Ocaml_target.main_module tgt with
   	None->false |Some(hm2)->hm2<>hm
  ) old_tgts in
  (new_mdata,new_dirs,new_tgts);;
  
let unregister_module (old_mdata,old_tgts) hm=
 let new_mdata=German_modify_modulesystem.unregister_module old_mdata hm in
 let new_dirs=German_directories.from_data new_mdata 
 and new_tgts=List.filter (fun tgt->
   match Ocaml_target.main_module tgt with
   None->false |Some(hm2)->hm2<>hm
 ) old_tgts in
  (new_mdata,new_dirs,new_tgts);;
  
  
let register_mlx_file (old_mdata,old_dirs,old_tgts) mlx=
  let hm=Mlx_filename.half_dressed_core mlx in
  let new_dir=Half_dressed_module.subdirectory hm in
 let new_mdata=German_modify_modulesystem.register_mlx_file old_mdata mlx in
 let new_dirs=
 (if List.mem new_dir old_dirs then old_dirs else old_dirs@[new_dir] )
 and new_tgts=
 (if Half_dressed_module.is_optional hm
  then old_tgts
  else (*
       The only outdated targets are the main toplevel, 
       and targets corresponding to an identical module
       (for example when a mll or mly is added to
       an already registered ml) 
        *)
       let mn=German_constant.main_toplevel_name in
       List.filter (
        fun tgt->match Ocaml_target.toplevel_name tgt with
          None->(match Ocaml_target.main_module tgt with
                 None->true
                 |Some(hm2)->hm2<>hm
                )
          |Some(name)->name<>mn
       ) old_tgts
  ) in
  (new_mdata,new_dirs,new_tgts);; 
 

let self_update tolerate_cycles (old_mdata,old_dirs,old_tgts)=
    let (new_mdata,hms_to_be_updated)=
      German_self_update_modulesystem.self_update_modulesystem tolerate_cycles old_mdata in
	if hms_to_be_updated=[] then None else
	let new_dirs=German_directories.from_data new_mdata 
 	and new_tgts1=Ocaml_target.still_up_to_date_targets hms_to_be_updated old_tgts in
 	let dir=German_constant.root in
 	let checker=(fun tgt->
 	  let s=(Directory_name.to_string dir)^(Ocaml_target.to_string tgt) in 
 	  Sys.file_exists s ) in
 	let new_tgts=List.filter checker new_tgts1 in
    Some(new_mdata,new_dirs,new_tgts);;   
     
let heavy_self_update tolerate_cycles x=
  match  self_update tolerate_cycles x with
  None->(false,x)
  |Some(new_x)->(true,new_x);;
 
 
let rename_module (old_mdata,old_dirs,old_tgts) old_hm new_hm=
  let untouched_targets=List.filter
   (fun tgt->not(German_ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_mdata [old_hm] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_hm)) ) old_tgts in
  let new_mdata=German_rename_file_in_system.rename old_mdata old_hm new_hm in
  snd(heavy_self_update false (new_mdata,old_dirs,untouched_targets));;   
  

let make_module_optional (old_mdata,old_tgts) old_hm =
 let new_mdata=German_relocate_file_in_system.make_module_optional old_mdata old_hm in
 let new_dirs=German_directories.from_data new_mdata in
 snd(heavy_self_update false (new_mdata,new_dirs,old_tgts));;   
  
let relocate_module (old_mdata,old_tgts) old_hm new_dir=
    let new_mdata=German_relocate_file_in_system.relocate_module old_mdata old_hm new_dir in
    let new_dirs=German_directories.from_data new_mdata in
    snd(heavy_self_update false (new_mdata,new_dirs,old_tgts));;   
   

let add_target_perhaps opt_tgt l=Option.add_perhaps opt_tgt l;;

  
