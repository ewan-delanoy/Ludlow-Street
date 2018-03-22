(*

#use"Country/Germany/german_make_self_contained_copy.ml";;

*)

let prepare destdir l=
  let all_data=German_wrapper.data() in
  let temp1=Image.image German_vague_string.to_module l in
  let temp2=Image.image(fun hm->
     Option.find (fun md->Modulesystem_data.name md=hm) all_data
  ) temp1 in
  let temp3=Image.image (fun x->Tidel.diforchan(Modulesystem_data.all_ancestors x)) 
      temp2 in
  let involved_modules=Tidel.big_teuzin ((Tidel.diforchan temp1)::temp3) in
  let involved_data=List.filter (fun md->
         Tidel.elfenn (Modulesystem_data.name md) involved_modules) all_data in
  let involved_files=(List.flatten 
    (Image.image Modulesystem_data.acolytes involved_data)) in
  let involved_paths=
    (German_wrapper.outside_files())@
    (Image.image Mlx_ended_absolute_path.to_path involved_files) in
  let short_paths=Image.image (
    fun ap->
      let s_ap=Absolute_path.to_string ap in
      Directory_name.cut_beginning German_constant.root s_ap
  ) involved_paths in
  let main_diff=Prepare_dircopy_update.compute_diff 
        (German_constant.root,short_paths) destdir in
  Prepare_dircopy_update.commands_for_update destdir main_diff;;

let prepare_special_file destdir filename=
  let the_file=Absolute_path.create_file(Directory_name.join destdir filename) in
  Replace_inside.replace_inside_file
  (Directory_name.connectable_to_subpath German_constant.root,
   Directory_name.connectable_to_subpath destdir)
  the_file;;


    
let mscc destdir l=
  let s_dir=Directory_name.connectable_to_subpath destdir in 
  let _=Unix_command.uc ("mkdir -p "^s_dir^"_build") in
  let _=Image.image Unix_command.uc (prepare destdir l) in
  let _=Image.image (prepare_special_file destdir)
    [German_constant.name_for_printersfile;German_constant.name_for_loadingsfile]
  in 
  let command_for_coming_out=
  "mv "^s_dir^"Optional/* "^s_dir in
  let _=Unix_command.uc command_for_coming_out in  
  (*
  let bundle1=Alaskan_create_target_system.from_main_directory destdir None [] in
   let (new_mdata2,new_targets,new_ofiles,new_pe_types)=bundle1 in
   let _=Alaskan_save_all.write_all 
   (destdir,German_constant.main_toplevel_name, 
   German_constant.name_for_makefile,
   German_constant.name_for_targetfile,
   German_constant.name_for_loadingsfile,
   German_constant.name_for_printersfile)
   (
      new_mdata,
      new_directories,
      new_targets,
      [],
      [],
      [],
      [],
      [],
      new_pe_types
  )
    ts in
   *) 


  ();; 

       






   
   
  