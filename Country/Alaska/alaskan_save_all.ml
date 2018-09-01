
(* 

#use"Country/Alaska/alaskan_save_all.ml";;


*)

module Private=struct

  let save_makefile (root,location_for_makefile) mdata=
    let s1="# This makefile was automatocally written by\n"^
    "# the write_makefile function in the ml_manager module. \n\n"^
    (Alaskan_write_makefile.write_makefile mdata) in
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
    let s1=Modify_md_list.archive mdata in
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
  
  
