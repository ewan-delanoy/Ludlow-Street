(*

#use "/Users/ewandelanoy/Documents/OCaml/Ordinary/some_pervasives.ml";; 


*)





let cmts=Current_target_system.current;;
let ordinary=Directory_name.to_string Current_root_directory.current_root_directory;;

let cdir=Current_root_directory.current_root_directory;;
let current_registered_directories ()=
  Compute_modulesystem_directories.compute_modulesystem_directories 
  (Current_target_system.current_modulesystem());;

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



let fmr x=Mutable_target_system.find_module_registration cmts (hmx x);;
let abo x=Mutable_target_system.above cmts (hmx x);;
let bel x=Mutable_target_system.below cmts (hmx x);;
let dbel x=Mutable_target_system.directly_below cmts (hmx x);;
let ren x y=Mutable_target_system.rename_module cmts (hmx x) (No_slashes.of_string y);;
let relo x y=Mutable_target_system.relocate_module cmts (hmx x) y;;
let mmo x=Mutable_target_system.make_module_optional cmts (hmx x) ;;



let fg x=
   if String.contains x '.'
   then Mutable_target_system.forget_file cmts (fl x)
   else Mutable_target_system.forget_module cmts (hmx x);;
     

  

let regi x= 
  let mlx=Mlx_filename.of_path_and_root (fl x) cdir in
  Mutable_target_system.register cmts mlx;;

let rego x=Mutable_target_system.register_outside_file cmts (Absolute_path.of_string x);;

let ureg x=
  if List.exists (fun edg->Substring.ends_with x edg) [".ml";".mll";".mly"] 
  then let mlx=Mlx_filename.of_path_and_root (fl x) cdir in
       Mutable_target_system.unregister cmts mlx 
  else Mutable_target_system.unregister_module cmts (hmx x);;

let cf t1 t2=
   let ap1=fl t1 in
   let pre_s2=(Father_and_son.son (Father_and_son.invasive_father t2 '.') '/')^".ml" in
   let s1=Cull_string.cobeginning (String.length ordinary) (Absolute_path.to_string ap1) in
   let s2=(fun w->if w="" then pre_s2 else w^"/"^pre_s2) (Father_and_son.father s1 '/')  in
   let os1=ordinary^s1 and os2=ordinary^s2 in
   let _=Sys.command ("cp "^os1^" "^os2) in
   let txt1="#use\""^s1^"\";;"
   and txt2="#use\""^s2^"\";;" in
   let _=Replace_inside.replace_inside_file 
    (txt1,txt2) (Absolute_path.of_string os2)  in 
   Sys.command ("open -a /Applications/TextWrangler"^".app "^os2);;   

let recently_deleted_files ()=
  let temp1=More_unix.quick_complete_ls "/Users/ewandelanoy/Documents/OCaml/Githubbed_ocaml" in
  let temp2=List.filter (
   fun s->
   (not(Substring.begins_with s "/Users/ewandelanoy/Documents/OCaml/Githubbed_ocaml/.git/"))
   &&(s<>"/Users/ewandelanoy/Documents/OCaml/Githubbed_ocaml/README")
) temp1 in
let temp3=Image.image (Cull_string.cobeginning 51) temp2 in
let temp4=List.filter (
   fun t->not(Sys.file_exists("/Users/ewandelanoy/Documents/OCaml/Ordinary/"^t))
) temp3 in
temp4;;

let confirm_deletion l=
   Image.image (fun t->
   Sys.command ("rm /Users/ewandelanoy/Documents/OCaml/Githubbed_ocaml/"^t)) l;;  


let tw s=
  let temp1=Find_suitable_ending.find_file_location cdir (current_directories()) s in
  let s1=Absolute_path.to_string temp1 in
  Sys.command ("open -a /Applications/TextWrangler"^".app "^s1);;


let syz()=Mutable_target_system.system_size Current_target_system.current;;


let reco ()=Mutable_target_system.recompile Current_target_system.current;;


let rv=Current_target_system.rename_value;;
let bk=Current_target_system.backup;;
let sd=Current_target_system.start_debugging;;
let sv=Current_target_system.show_value_occurrences;;
let vfm=Current_target_system.values_from_module;;
let muv=Current_target_system.modules_using_value;;
let rd ()=Mutable_target_system.remove_debuggables cmts;;
let rcd =recently_deleted_files;;

let rsh=Current_target_system.refresh;;



let df=Current_target_system.deletable_files;;
let oim=Current_target_system.outdated_interesting_modules;;




