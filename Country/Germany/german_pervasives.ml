(*

#use"Country/Germany/german_pervasives.ml";;

*)

let cmts=German_wrapper.whole;;

let whole_ref=(
  German_wrapper.Private.data_ref,
  German_wrapper.Private.directories_ref,
  German_wrapper.Private.up_to_date_targets_ref,
  German_wrapper.Private.outside_files_ref,
  German_wrapper.Private.outside_directories_ref,
  German_wrapper.Private.recently_deleted_ref,
  German_wrapper.Private.printer_equipped_types_ref
);;


let cdir=German_constant.root;;
let s_cdir=Directory_name.to_string cdir;;
let current_registered_directories ()=German_wrapper.directories();;

let current_directories()=
  let temp1=List.filter (
    fun x->x<>Subdirectory.SD ""
  ) (current_registered_directories()) in
  (Subdirectory.SD "")::(temp1@
  [Subdirectory.SD "Remembered";Subdirectory.SD "Forgotten"]);;

let fl=Find_suitable_ending.find_file_location cdir (current_registered_directories());;

exception Absent_module of string;;

let hmx x=
  let s=Father_and_son.invasive_father x '.' in
  match (Option.find_and_stop(
      fun edg->try(Some(fl(s^edg))) with _->None
  ) Ocaml_ending.all_string_endings) with
  None->raise(Absent_module(x))
  |Some(ap)->Half_dressed_module.of_path_and_root ap cdir;; 



let fmr x=German_data.find_module_registration (German_wrapper.data()) (hmx x);;
let abo x=German_data.above (German_wrapper.data()) (hmx x);;
let bel x=German_data.below (German_wrapper.data()) (hmx x);;
let dbel x=German_data.directly_below (German_wrapper.data()) (hmx x);;


let ren x y=German_wrapper.rename_module (hmx x) (No_slashes.of_string y);;
let relo x y=German_wrapper.relocate_module (hmx x) y;;
let mmo x=German_wrapper.make_module_optional (hmx x) ;;



let fg x=
   if String.contains x '.'
   then German_wrapper.forget_file (fl x)
   else German_wrapper.forget_module (hmx x);;

let regi x= 
  let mlx=Mlx_filename.of_path_and_root (fl x) cdir in
  German_wrapper.register_mlx_file mlx;;

let rego x=German_wrapper.register_outside_file (Absolute_path.of_string x);;

let ureg x=
  if List.exists (fun edg->Substring.ends_with x edg) [".ml";".mll";".mly"] 
  then let mlx=Mlx_filename.of_path_and_root (fl x) cdir in
       German_wrapper.unregister_mlx_file mlx 
  else German_wrapper.unregister_module (hmx x);;

let cf t1 t2=
   let ap1=fl t1 in
   let pre_s2=(Father_and_son.son (Father_and_son.invasive_father t2 '.') '/')^".ml" in
   let s1=Cull_string.cobeginning (String.length s_cdir) (Absolute_path.to_string ap1) in
   let s2=(fun w->if w="" then pre_s2 else w^"/"^pre_s2) (Father_and_son.father s1 '/')  in
   let os1=s_cdir^s1 and os2=s_cdir^s2 in
   let _=Sys.command ("cp "^os1^" "^os2) in
   let txt1="#use\""^s1^"\";;"
   and txt2="#use\""^s2^"\";;" in
   let _=Replace_inside.replace_inside_file 
    (txt1,txt2) (Absolute_path.of_string os2)  in 
   Sys.command ("open -a /Applications/TextWrangler"^".app "^os2);;   

let tw s=
  let temp1=Find_suitable_ending.find_file_location cdir (current_directories()) s in
  let s1=Absolute_path.to_string temp1 in
  Sys.command ("open -a /Applications/TextWrangler"^".app "^s1);;


let syz()=German_data.system_size (German_wrapper.data());;

let reco =German_wrapper.recompile;;


let pbk=German_backup_target_system.prepare_backup;;
let bk=German_backup_target_system.backup;;
let sd=German_wrapper.start_debugging;;

let rv=German_values_in_modules.rename_value (German_wrapper.data()) ;;
let sv wal=German_values_in_modules.show_value_occurrences_in_modulesystem wal (German_wrapper.data()) ;;
let vfm modname =German_values_in_modules.list_values_from_module_in_modulesystem modname (German_wrapper.data()) ;;
let muv=German_values_in_modules.modules_using_value (German_wrapper.data()) ;;

let ed =German_wrapper.end_debugging;;


let rsh ()=German_wrapper.refresh;;



let df=German_data.deletable_files;;
let oim=German_data.outdated_interesting_modules;;


