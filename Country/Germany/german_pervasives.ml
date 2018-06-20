(*

#use"Country/Germany/german_pervasives.ml";;

*)


let cdir=German_constant.root;;

let s_cdir=Directory_name.connectable_to_subpath cdir;;
let current_registered_directories ()=German_wrapper.directories();;

let current_directories()=
  let temp1=List.filter (
    fun x->x<>Subdirectory.SD ""
  ) (current_registered_directories()) in
  (Subdirectory.SD "")::(temp1@
  [
    German_constant.kept_up_to_date_but_not_registered;
    German_constant.not_registered_any_more;
    German_constant.old_and_hardly_reusable
  ]);;

let fl=German_vague_string.to_path;; 

let fmr x=
  let uncapitalized_x=
    Naked_module.of_string(String.uncapitalize_ascii x) in
  Md_list.find_naked_module_registration
  (German_wrapper.data()) uncapitalized_x;;

exception No_module_with_name of string;;

let hmx x=
   match fmr x
   with 
   Some(md)->Modulesystem_data.name md
   |None->raise(No_module_with_name(x));;  

let abo x=
  Image.image Half_dressed_module.uprooted_version
  (Md_list.above (German_wrapper.data()) (hmx x));;
let bel x=
  Image.image Half_dressed_module.uprooted_version
  (Md_list.below (German_wrapper.data()) (hmx x));;
let dbel x=
  Image.image Half_dressed_module.uprooted_version
  (Md_list.directly_below (German_wrapper.data()) (hmx x));;


let ren_without_backup x y=German_wrapper.rename_module (hmx x) (No_slashes.of_string y);;
let relo_without_backup x y=German_wrapper.relocate_module (hmx x) y;;

let fg_without_backup x=
   if String.contains x '.'
   then German_wrapper.forget_file (fl x)
   else German_wrapper.forget_module (hmx x);;

let regi_without_backup x= 
  let path=Absolute_path.of_string(Directory_name.join German_constant.root x) in
  let mlx=Mlx_ended_absolute_path.of_path_and_root path German_constant.root in
  German_wrapper.register_mlx_file mlx;;



let ureg_without_backup x=
  if List.exists (fun edg->Substring.ends_with x edg) [".ml";".mli";".mll";".mly"] 
  then let path=Absolute_path.of_string(Directory_name.join German_constant.root x) in
       let mlx=Mlx_ended_absolute_path.of_path_and_root path cdir in
       German_wrapper.unregister_mlx_file mlx 
  else German_wrapper.unregister_module (hmx x);;

let double_semicolon=";"^";";;

let cf t1 t2=
   let ap1=fl t1 in
   let s_ap1=Absolute_path.to_string ap1 in
   let s_ap2=(Father_and_son.invasive_father s_ap1 '/')^"/"^t2^".ml" in
   let _=Unix_command.uc ("cp "^s_ap1^" "^s_ap2) in
   let ap2=Absolute_path.of_string s_ap2 in
   let s1=Cull_string.cobeginning (String.length s_cdir) s_ap1
   and s2=Cull_string.cobeginning (String.length s_cdir) s_ap2 in
   let txt1="#use\""^s1^"\""^double_semicolon
   and txt2="#use\""^s2^"\""^double_semicolon in
   let _=Replace_inside.replace_inside_file 
    (txt1,txt2) ap2  in 
   Unix_command.uc ("open -a \"/Applications/Visual Studio Code.app\" "^s_ap2);;   

let vo s=
  let temp1=Find_suitable_ending.find_file_location cdir (current_directories()) s in
  let s1=Absolute_path.to_string temp1 in
  Unix_command.uc ("open -a \"/Applications/Visual Studio Code.app\" "^s1);;


let syz()=Md_list.system_size (German_wrapper.data());;

let init=German_wrapper.initialize;;

let reco_without_backup ()=
  German_wrapper.recompile();;


let pbk ()=Dircopy_diff.display(German_wrapper.diff());;
let bk=German_backup_target_system.backup_with_message (German_wrapper.diff());;

let rd ()=Alaskan_remove_debuggables.rd German_constant.root (German_wrapper.data());;
let sd=German_wrapper.start_debugging;;


let rv_without_backup x y=German_values_in_modules.rename_string_or_value (German_wrapper.data()) x y;;
let srv_without_backup x y=German_values_in_modules.replace_string (German_wrapper.data()) x y;;


let sv wal=German_values_in_modules.show_value_occurrences_in_modulesystem 
  wal (German_wrapper.data()) ;;
let vfm modname =German_values_in_modules.list_values_from_module_in_modulesystem 
    modname (German_wrapper.data()) ;;
let muv x=Md_list.modules_using_value (German_wrapper.data()) x;;

let ed =German_wrapper.end_debugging;;


let vd=German_wrapper.view_definition;;
let fvd=Find_value_descendants.fvd 
  (Compute_all_ocaml_items.caoi(German_wrapper.data())) ;;

let rsh_without_backup=German_wrapper.refresh;;


let am ()=Md_list.all_naked_modules (German_wrapper.data());;
  
    
let tw x=
  let hm=hmx x in
  let s_hm=Half_dressed_module.uprooted_version hm in
  let fn=(Directory_name.connectable_to_subpath(cdir))^s_hm in    
  Sys.command ("open -a /Applications/TextWrangler.app "^fn^".ml");;



let ucc ()=German_create_or_update_copied_compiler.ucc 
 (Directory_name.of_string "/Users/ewandelanoy/Documents/OCaml/Cherokee");;

let reco ()=let bowl=reco_without_backup () in (if bowl then German_wrapper.backup None);;
let reco_with_comment s=let bowl=reco_without_backup () in (if bowl then German_wrapper.backup (Some s));;


let fg x=(fg_without_backup x;German_wrapper.backup None);;


let regi x=(regi_without_backup x;German_wrapper.backup None);;
let rndir p=(German_wrapper.rename_directory p;reco());;

let relo x y=(relo_without_backup x y;reco());;
let ren  x y=(ren_without_backup  x y;reco());;
let rsh ()=(rsh_without_backup ();German_wrapper.backup None);;
let rwc =reco_with_comment;;
let rv x y=(rv_without_backup x y;reco());;
let srv x y=(srv_without_backup x y;reco());;
let ureg x=(ureg_without_backup x;reco());;



