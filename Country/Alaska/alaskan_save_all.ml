
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
  
  let industrial_separator1=Industrial_separator.alaskan_save_all1;;  
  let industrial_separator2=Industrial_separator.alaskan_save_all2;;    

  let archive 
    (mdata,directories,up_to_date_targets,printer_equipped_types)=
      
     let temp2=Image.image (fun w->Nonblank.make(Subdirectory.without_trailing_slash w)) directories 
     and temp3=Image.image Ocaml_target.archive up_to_date_targets 
     and temp9=Image.image Half_dressed_module.archive_pair printer_equipped_types in
     String.concat industrial_separator1
     [
       Modify_md_list.archive mdata;
       Nonblank.make(String.concat industrial_separator2 temp2);
       Nonblank.make(String.concat industrial_separator2 temp3);
       Nonblank.make(String.concat industrial_separator2 temp9);
     ];;
  
  let save_targetfile (root,location_for_targetfile) uple=
    let s1=archive uple in
    let lt=Root_directory.force_join root location_for_targetfile in
    Io.overwrite_with (Absolute_path.of_string lt) s1;;
  
  end;;
  
  let read_all s=
     let l1=Str.split (Str.regexp_string Private.industrial_separator1) s in
     let v1=List.nth l1 0 in
     let v2=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  1)) 
     and v3=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  2)) 
     and v9=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  3))
     in
     let new_mdata=Modify_md_list.unarchive v1 in
     let new_directories=Image.image (fun v->Subdirectory.of_string(Nonblank.decode v)) v2 in
     let new_targets=Image.image Ocaml_target.unarchive v3 in
     let new_pe_types=Image.image Half_dressed_module.unarchive_pair v9 in
     
  (
      new_mdata,
      new_directories,
      new_targets,
      new_pe_types
  );;
  
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
      Private.save_targetfile (root,location_for_targetfile) uple;
      Private.save_printersfile (root,location_for_printersfile) printer_equipped_types;
     );;
  
  
