
(* 

#use"Country/Alaska/alaskan_create_target_system.ml";;


*)

module Private=struct

let display_circular_dependencies printer l cycles= 
  if cycles=[]
  then ()
  else
  let temp1=Image.image(fun cycle->
    let ttemp1=Image.image (fun j->printer (List.nth l (j-1))) cycle in
     String.concat " -> " ttemp1 
  ) cycles in
  let temp2="\n\n The following cycles have been detected : "^
    (String.concat "\n\n" temp1) in
  (print_string temp2;flush stdout);;
 
let init_dir=
  Subdirectory.connectable_to_subpath 
  (Coma_constant.kept_up_to_date_but_not_registered);;

let copy_special_files s_main_dir=
  let dname=Coma_constant.name_for_debugged_module in
  let _=Image.image(
   fun s->
    Unix_command.uc 
      ("mkdir -p "^s_main_dir^"/"^(Subdirectory.without_trailing_slash s))
  ) [
       Coma_constant.kept_up_to_date_but_not_registered;
       Coma_constant.temporary;
    ]
  in
  let _=Unix_command.uc ("mkdir -p "^s_main_dir^"/_build/") in
  let _=Image.image (fun s->
    Unix_command.uc ("touch "^s_main_dir^"/"^s)
     ) ([dname^".ml";
       ".ocamlinit"]
       @
       Coma_constant.up_to_date_but_not_registered_files
    ) in ();;

let put_default_content_in_special_files s_main_dir=
  (Io.overwrite_with 
  (Absolute_path.of_string (s_main_dir^"/.ocamlinit"))
  (
  "\n#use\""^Coma_constant.path_for_loadingsfile^"\""^Double_semicolon.ds^
  "\n#use\""^Coma_constant.path_for_printersfile^"\""^Double_semicolon.ds^
  "\nopen Needed_values;;"^
  "\ninitialize_toplevel();;"
   );
  Io.overwrite_with 
  (Absolute_path.of_string (s_main_dir^"/"^init_dir^"/my_printers.ml"))
  "\n\n (*Registered printers start here *) \n\n (*Registered printers end here *) \n\n");; 
  

let select_good_files s_main_dir=
   let ap1=Absolute_path.of_string s_main_dir in        
   let temp1=More_unix.complete_ls (Directory_name.of_string s_main_dir) in
   let s_ap1=Absolute_path.to_string ap1 in
   let n1=String.length(s_ap1) in
   let selector=(
   fun ap->
     let s=Absolute_path.to_string ap in
     let t=Cull_string.cobeginning n1 s in
     (List.exists (fun edg->Substring.ends_with s edg) [".ml";".mli";".mll";".mly"])
     &&
     (List.for_all (fun beg->not(Substring.begins_with t beg)) 
     (Image.image Subdirectory.connectable_to_subpath 
      [
        Coma_constant.kept_up_to_date_but_not_registered;
        Coma_constant.left_out_of_updating;
        Coma_constant.old_and_hardly_reusable;
        Coma_constant.temporary;
      ]
     ))
     &&
     (* When a mll or mly is present, the ml will automatically be registered also,
        see the alaskan_register_mlx_file module. *)
     (not(
           (Substring.ends_with s ".ml")
           &&
           (List.exists (fun edg->Sys.file_exists(s_ap1^s^edg)) ["l";"y"])
     ))
     &&
     (List.for_all (fun edg->not(Substring.ends_with s edg) ) 
     [".ocamlinit"]
     )
   ) in
   List.filter selector temp1;;
   
 let rec detect_identical_names (identical_names,l)=
   match l with 
   []->identical_names
  |(a,b)::others->
     let (temp1,temp2)=List.partition (fun t->snd(t)=b) others in
     if temp1<>[]
     then detect_identical_names(((a,b)::temp1)::identical_names,temp2)
     else detect_identical_names(identical_names,temp2);;  
     
 exception Identical_names of (((string*string) list) list);;    
     
 let clean_list_of_files main_dir l=
  (*
     raises an exception if there are different modules with
     identical names.
     Removes the files outside main_dir.
  *)
  let s_dir=Root_directory.connectable_to_subpath main_dir in
  let temp1=List.filter (fun ap->
    Substring.begins_with (Absolute_path.to_string ap) s_dir
  ) l in
  let temp2=Image.image (fun ap->
    let s=Absolute_path.to_string ap in
    (ap,Father_and_son.son s '/')
  ) temp1 in
  let temp3=detect_identical_names ([],temp2) in
  if temp3<>[]
  then let n1=String.length s_dir in
       let tempf1=(fun (x,y)->
           (Cull_string.cobeginning n1 (Absolute_path.to_string x),y)
        ) in
       let tempf2=Image.image (Image.image tempf1) in
       let temp4=tempf2 temp3 in
       raise(Identical_names(temp4))
  else temp2;;
  
let compute_dependencies l=
  let temp1=Ennig.index_everything l 
  and n=List.length l in
  let rec tempf=(fun (j1,(ap1,s1))->
    let ttemp1=Look_for_module_names.names_in_file ap1 in
    let ttemp2=Image.image Naked_module.to_string ttemp1 in
    let ttempf=(fun s_nm->
      Option.filter_and_unpack (fun 
      (k,(_,s))->
      if (Father_and_son.father s '.')=s_nm
      then Some(k)
      else None ) temp1
    ) in
    let ttemp3=Image.image ttempf ttemp2 in
    List.flatten  ttemp3
  )  in
  let tempg=(fun x-> let (_,(_,s))=x in
     if Substring.ends_with s ".mli"
     then let t=Cull_string.coending 1 s in
          match Option.seek (fun (_,(_,s1))->s1=t) temp1 with
           None->tempf x
          |Some(y)->tempf y 
     else tempf x
  ) in
  let table_for_coatoms=Image.image tempg temp1 in
  let coat=Memoized.make(fun j->List.nth table_for_coatoms (j-1)) in
  let (cycles,good_list)=
  	Reconstruct_linear_poset.reconstruct_linear_poset coat 
  	(Ennig.ennig 1 n) in
  let _=display_circular_dependencies
  (fun (j1,(ap1,s1))->s1) temp1 cycles in
  Image.image (fun (j,_)->snd(List.nth temp1 (j-1)) ) good_list;;
  
let from_prepared_list dir backup_dir l=
   let temp1=Option.filter_and_unpack (fun (ap,s)->
      Mlx_ended_absolute_path.try_from_path_and_root ap dir
   ) l in
   Alaskan_try_to_register.mlx_files (Modify_md_list.empty_one dir backup_dir) temp1;;

end;;   

let from_main_directory dir backup_dir =
	let old_s=Root_directory.connectable_to_subpath(dir) in
  let s_main_dir=Cull_string.coending 1 old_s in (* mind the trailing slash *)
  let _=
    (Private.copy_special_files s_main_dir;
     Private.put_default_content_in_special_files s_main_dir 
    ) in
	let temp1=Private.select_good_files s_main_dir in
    let temp2=Private.clean_list_of_files dir temp1 in
    let temp3=Private.compute_dependencies temp2 in
    let (failures,mdata1)=Private.from_prepared_list dir backup_dir temp3 in
    let pre_preqt=Modify_md_list.printer_equipped_types_from_data mdata1 in
    let (mdata2,new_tgts2,rejected_ones2)=Alaskan_make_ocaml_target.feydeau
       dir mdata1 []  in
   let preqt=Image.image (fun hm->(hm,not(List.mem hm rejected_ones2))) pre_preqt in 
 	(mdata2,new_tgts2,preqt);;

