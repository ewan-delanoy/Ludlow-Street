(*

#use"Country/Germany/german_pervasives.ml";;

*)


let cdir=German_constant.root;;

let s_cdir=Root_directory.connectable_to_subpath cdir;;
let current_registered_directories ()=German_wrapper.directories();;

let current_directories()=
  let temp1=List.filter (
    fun x->x<>Subdirectory_t.SD ""
  ) (current_registered_directories()) in
  (Subdirectory_t.SD "")::(temp1@
  [
    Coma_constant.kept_up_to_date_but_not_registered;
    Coma_constant.left_out_of_updating;
    Coma_constant.old_and_hardly_reusable
  ]);;

let fl=German_vague_string.to_path;; 

let fmr x=
  let uncapitalized_x=
    Naked_module.of_string(String.uncapitalize_ascii x) in
  Modify_md_list.find_naked_module_registration
  (German_wrapper.data()) uncapitalized_x;;

exception No_module_with_name of string;;

let hmx x=
   match fmr x
   with 
   Some(md)->Modulesystem_data.name md
   |None->raise(No_module_with_name(x));;  

let abo x=
  Image.image Half_dressed_module.uprooted_version
  (Modify_md_list.above (German_wrapper.data()) (hmx x));;
let bel x=
  Image.image Half_dressed_module.uprooted_version
  (Modify_md_list.below (German_wrapper.data()) (hmx x));;
let dbel x=
  Image.image Half_dressed_module.uprooted_version
  (Modify_md_list.directly_below (German_wrapper.data()) (hmx x));;


let ren_without_backup x y=German_wrapper.rename_module (hmx x) (No_slashes.of_string y);;
let relo_without_backup x y=German_wrapper.relocate_module (hmx x) y;;

let fg_without_backup x=
   if String.contains x '.'
   then German_wrapper.forget_file (fl x)
   else let _=German_wrapper.forget_module (hmx x) in ();;

let regi_without_backup x= 
  let path=Absolute_path.of_string(Root_directory.join German_constant.root x) in
  let mlx=Mlx_ended_absolute_path.of_path_and_root path German_constant.root in
  German_wrapper.register_mlx_file mlx;;



let ureg_without_backup x=
  if List.exists (fun edg->Substring.ends_with x edg) [".ml";".mli";".mll";".mly"] 
  then let path=Absolute_path.of_string(Root_directory.join German_constant.root x) in
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


let syz()=Modify_md_list.system_size (German_wrapper.data());;

let init=German_wrapper.initialize;;

let reco_without_backup ()=
  fst(German_wrapper.recompile());;


let rd ()=Alaskan_remove_debuggables.rd German_constant.root (German_wrapper.data());;
let sd=German_wrapper.start_debugging;;



let rv_without_backup x y=German_values_in_modules.rename_string_or_value (German_wrapper.data()) x y;;
let srv_without_backup x y=German_values_in_modules.replace_string (German_wrapper.data()) x y;;


let sv wal=German_values_in_modules.show_value_occurrences_in_modulesystem 
  wal (German_wrapper.data()) ;;
let vfm modname =German_values_in_modules.list_values_from_module_in_modulesystem 
    modname (German_wrapper.data()) ;;
let muv x=Modify_md_list.modules_using_value (German_wrapper.data()) x;;

let ed =German_wrapper.end_debugging;;


let vd=German_wrapper.view_definition;;
let fvd=Find_value_descendants.fvd 
  (Compute_all_ocaml_items.caoi(German_wrapper.data())) ;;

let rsh_without_backup ()=let _=German_wrapper.refresh() in ();;


let am ()=Modify_md_list.all_naked_modules (German_wrapper.data());;
  
    
let tw x=
  let hm=hmx x in
  let s_hm=Half_dressed_module.uprooted_version hm in
  let fn=(Root_directory.connectable_to_subpath(cdir))^s_hm in    
  Sys.command ("open -a /Applications/TextWrangler.app "^fn^".ml");;



let ucc ()=German_create_or_update_copied_compiler.ucc 
 (Root_directory.of_string "/Users/ewandelanoy/Documents/OCaml/Cherokee");;

let reco_with_optional_comment opt=
  let (bowl,short_paths)=German_wrapper.recompile () in
   (if bowl 
   then 
   let ordered_paths=Ordered_string.forget_order(Ordered_string.safe_set(short_paths)) in
   let diff=
    Dircopy_diff.veil
    (Recently_deleted.of_string_list [])
    (Recently_changed.of_string_list ordered_paths)
    (Recently_created.of_string_list []) in
   (
      German_wrapper.backup diff opt;
    German_wrapper.save_all() 
   ));;



let reco ()=reco_with_optional_comment None;;
let reco_with_comment s=reco_with_optional_comment (Some s);;  



let forget_file_with_backup x=
   let ap=fl x in
   let s_ap=Absolute_path.to_string ap in  
   let cut_ap=Root_directory.cut_beginning German_constant.root s_ap in
   let diff=
    Dircopy_diff.veil
    (Recently_deleted.of_string_list [cut_ap])
    (Recently_changed.of_string_list [])
    (Recently_created.of_string_list []) in
   (
    German_wrapper.forget_file ap;
    German_wrapper.backup diff None;
    German_wrapper.save_all() 
   ) ;; 

let forget_module_with_backup x=
    let short_paths=German_wrapper.forget_module (hmx x) in
    let ordered_paths=Ordered_string.forget_order(Ordered_string.safe_set(short_paths)) in
    let diff=
      Dircopy_diff.veil
      (Recently_deleted.of_string_list ordered_paths)
      (Recently_changed.of_string_list [])
      (Recently_created.of_string_list []) in
     (
      German_wrapper.backup diff None; 
      German_wrapper.save_all() 
     );; 
 
let fg x=
      if String.contains x '.'
      then forget_file_with_backup x
      else forget_module_with_backup x;;


let regi x=
  let path=Absolute_path.of_string(Root_directory.join German_constant.root x) in
  let mlx=Mlx_ended_absolute_path.of_path_and_root path German_constant.root in
  let short_path=Mlx_ended_absolute_path.short_path mlx in
  let diff=
    Dircopy_diff.veil
    (Recently_deleted.of_string_list [])
    (Recently_changed.of_string_list [])
    (Recently_created.of_string_list [short_path]) in
  (
    German_wrapper.register_mlx_file mlx;
    German_wrapper.backup diff None;
    German_wrapper.save_all() 
   );;

let rndir p=(German_wrapper.rename_directory p;reco());;

let relo x y=(relo_without_backup x y;reco());;
let ren  x y=(ren_without_backup  x y;reco());;
let rsh ()=
  let diff=German_wrapper.refresh () in
  (
    German_wrapper.backup diff None;
    German_wrapper.save_all() 
   );;




let rwc =reco_with_comment;;
let rv x y=(rv_without_backup x y;reco());;
let srv x y=(srv_without_backup x y;reco());;
let ureg x=(ureg_without_backup x;reco());;



