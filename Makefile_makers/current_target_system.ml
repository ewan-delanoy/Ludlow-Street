(*

#use"Makefile_makers/current_target_system.ml";;

*)




let location_for_current_makefile="makefile";;
let location_for_current_targetfile="targetfile.ocaml_made";;
let location_for_current_loadingsfile="my_loadings.ml";;
let location_for_current_pervasivesfile="my_pervasives.ml";;
let location_for_current_printersfile="my_printers.ml";;
let location_for_backup="/Users/ewandelanoy/Documents/OCaml/Githubbed_ocaml";;


let current_root=Current_root_directory.current_root_directory;;
let root_string=Cull_string.coending 1 (Directory_name.to_string current_root);;


let read_archive ()=
  let lm=Directory_name.join current_root location_for_current_targetfile in
   let archive=Io.read_whole_file (Absolute_path.of_string lm) in
   Target_system.unarchive archive ;;

let current=
 let ts=(
   try read_archive() with 
   _-> Target_system.from_root_and_toplevel current_root (Some"ecaml")
 ) in
 {
   Mutable_target_system.content=ref(ts);
   Mutable_target_system.location_for_makefile=location_for_current_makefile;
   Mutable_target_system.location_for_targetfile=location_for_current_targetfile;  
   Mutable_target_system.location_for_loadingsfile=location_for_current_loadingsfile;  
   Mutable_target_system.location_for_pervasivesfile=location_for_current_pervasivesfile;
   Mutable_target_system.location_for_printersfile=location_for_current_printersfile;
 };;

let current_target_system ()=(!(current.Mutable_target_system.content));; 
 
let current_modulesystem ()=
  Target_system.modulesystem(current_target_system ());; 
 
let refresh ()=Mutable_target_system.refresh current;;
  
let backup msg=
   let ts=(!(current.Mutable_target_system.content))
   and destdir=Directory_name.of_string location_for_backup in
   let ts2=Backup_target_system.backup ts destdir msg in
   (current.Mutable_target_system.content:=ts2; 
    Mutable_target_system.save_all current;);;


let deletable_files ()=
  let (temp1,temp2)=Mutable_target_system.deletable_files current in
  let filterer=List.filter (
     fun hm->
       let s=Half_dressed_module.to_string hm in
       not(List.mem s Interesting_modules.list)
  ) in
  (filterer temp1,filterer temp2);;
  
let outdated_interesting_modules ()=
	let all_data=Mutable_target_system.all_filedata current in
	List.filter (
       fun s->match Option.find_it(
         fun md->Half_dressed_module.to_string(Modulesystem_data.name md)=s
       ) all_data with
       None->true
       |Some(md0)->
         let hm0=Modulesystem_data.name md0 in
        (Mutable_target_system.below current hm0)<>[]
    ) Interesting_modules.list;;
  

let rename_value old_name new_name=   
Values_in_modules.rename_value (current_modulesystem()) old_name new_name;;  
   
let show_value_occurrences s=   
Values_in_modules.show_value_occurrences_in_modulesystem s (current_modulesystem());;   

let values_from_module s=
Values_in_modules.list_values_from_module_in_modulesystem 
   s (current_modulesystem());;

let modules_using_value s=
Values_in_modules.modules_using_value (current_modulesystem()) s;;

let start_debugging()=
    let _=Mutable_target_system.remove_debuggables current in
    let dbg=Debugger_name.debugger_name in
	let ts=(!(current.Mutable_target_system.content)) in
	let fs=Target_system.modulesystem ts in
	let dir=Target_system.root ts in
	let rdir=Compute_modulesystem_directories.compute_modulesystem_directories fs in
	let ap=Find_suitable_ending.find_file_location dir rdir 
	     (dbg^".ml") in
	let hm=Half_dressed_module.of_path_and_root ap dir in
	let _=Mutable_target_system.recompute_module_info current hm in
	let new_ts=(!(current.Mutable_target_system.content)) in
	let tgt=Ocaml_target.debuggable hm in
	let (bowl,_)=Make_ocaml_target.make_nontoplevel new_ts tgt in
	let msg=(
	  if bowl 
	  then "\n\n Now, start \n\nocamldebug "^dbg^".ocaml_debuggable\n\nin another terminal\n\n"
	  else "\n\n Something went wrong, see above. \n\n"
	) in
	(
	  print_string msg;
	  flush stdout
	);;   
   
 
