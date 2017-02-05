(*

#use"Country/Germany/german_save_all.ml";;

*)

module Private=struct

let location_for_makefile=German_constant.location_for_makefile;;
let location_for_targetfile=German_constant.location_for_targetfile;;
let location_for_loadingsfile=German_constant.location_for_loadingsfile;;
let location_for_pervasivesfile=German_constant.location_for_pervasivesfile;;
let location_for_printersfile=German_constant.location_for_printersfile;;

let joiner=Directory_name.force_join German_constant.root ;;


let absolute_location_for_makefile =joiner location_for_makefile;;
let absolute_location_for_targetfile =joiner location_for_targetfile;;
let absolute_location_for_loadingsfile =joiner location_for_loadingsfile;;
let absolute_location_for_pervasivesfile =joiner location_for_pervasivesfile;;
let absolute_location_for_printersfile =joiner location_for_printersfile;;
 

let save_makefile mdata=
  let s1="# This makefile was automatocally written by\n"^
  "# the write_makefile function in the ml_manager module. \n\n"^
  (German_write_makefile.write_makefile mdata) in
  let lm=absolute_location_for_makefile in
  Io.erase_file_and_fill_it_with_string (Absolute_path.of_string lm) s1;;

let save_loadingsfile (dirs,tgts)=
   let s=German_up_to_date_targets.loadings 
    (dirs,tgts)
   and lm=absolute_location_for_loadingsfile in
   Io.erase_file_and_fill_it_with_string (Absolute_path.of_string lm) s;;

let save_printersfile printer_equipped_types=
   let s=Alaskan_printer_equipped_types.instructions printer_equipped_types
   and lm=absolute_location_for_printersfile in
   let beg_mark="(*Registered printers start here *)"
   and end_mark="(*Registered printers end here *)" in
   Replace_inside.overwrite_between_markers_inside_file
   (Overwriter.of_string s)
   (beg_mark,end_mark)
   (Absolute_path.of_string lm);;

let industrial_separator1=Industrial_separator.new_separator ();;  
let industrial_separator2=Industrial_separator.new_separator ();;    


let archive 
  (mdata,directories,up_to_date_targets,
    outside_files,outside_directories,
    recently_deleted,printer_equipped_types)=
    
   let temp1=Image.image (fun w->Nonblank.make(Subdirectory.unveil w)) directories 
   and temp2=Image.image (fun w->Nonblank.make(Subdirectory.unveil w)) outside_directories 
   and temp3=Image.image (fun w->Absolute_path.to_string w) outside_files 
   and temp4=Image.image (fun w->Nonblank.make w) recently_deleted 
   and temp5=Image.image Half_dressed_module.archive printer_equipped_types in
   String.concat industrial_separator1
   [
     German_data.archive mdata;
     Nonblank.make(String.concat industrial_separator2 temp1);
     Nonblank.make(String.concat industrial_separator2 (Image.image Ocaml_target.archive up_to_date_targets));
     Nonblank.make(String.concat industrial_separator2 temp2);
     Nonblank.make(String.concat industrial_separator2 temp3);
     Nonblank.make(String.concat industrial_separator2 temp4);
     Nonblank.make(String.concat industrial_separator2 temp5);
   ];;

let save_targetfile uple=
  let s1=archive uple in
  let lt=absolute_location_for_targetfile in
  Io.erase_file_and_fill_it_with_string (Absolute_path.of_string lt) s1;;

end;;

let read_all s=
   let l1=Str.split (Str.regexp_string Private.industrial_separator1) s in
   let v1=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  1))
   and v2=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  2)) 
   and v3=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  3)) 
   and v4=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  4)) 
   and v5=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  5))   
   and v6=Str.split (Str.regexp_string Private.industrial_separator2) (Nonblank.decode(List.nth l1  6))
   in
   let new_mdata=German_data.unarchive(List.nth l1 0) in
   let new_directories=Image.image (fun v->Subdirectory.of_string(Nonblank.decode v)) v1 in
   let new_targets=Image.image Ocaml_target.unarchive v2 in
   let new_odirectories=Image.image (fun v->Subdirectory.of_string(Nonblank.decode v)) v3 in
   let new_ofiles=Image.image Absolute_path.of_string v4 in
   let new_dfiles=Image.image Nonblank.decode v5 in
   let new_pe_types=Image.image Half_dressed_module.unarchive v6 in
   
(
    new_mdata,
    new_directories,
    new_targets,
    new_ofiles,
    new_odirectories,
    new_dfiles,
    new_pe_types
);;


let write_all uple= 
  let (mdata,directories,up_to_date_targets,
    _,_,_,printer_equipped_types)=uple in
   (
    Private.save_makefile mdata;
    Private.save_loadingsfile (directories,up_to_date_targets);
    Private.save_targetfile uple;
    Private.save_printersfile printer_equipped_types;
   );;

