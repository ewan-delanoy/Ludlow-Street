
(* 

#use"Country/Germany/german_rename_directory.ml";;

Renaming directories inside the Ocaml main directory.
Multi-level renaming is supported, i.e. you can 
rename A/B/C to D/E/F.

*)

let on_short_path (old_subdir,new_subdirname) s=
  (* let s_old_subdir=Subdirectory.connectable_to_subpath old_subdir in *)
  if Substring.begins_with s old_subdir
  then let sub_s=Cull_string.cobeginning (String.length old_subdir) s in
       new_subdirname^sub_s
  else s;;

let on_subdirectory (old_subdir,new_subdirname) sd=
   let s=Subdirectory.unveil sd in
   Subdirectory.of_string(on_short_path (old_subdir,new_subdirname) s);;

let on_half_dressed_module (old_subdir,new_subdirname) x=
   let (s,dir)=Half_dressed_module.unveil x in
   Half_dressed_module.of_string_and_root
   (on_short_path (old_subdir,new_subdirname) s)
   dir
;;

let on_ms_data (old_subdir,new_subdirname) x=
{                                                                 
      Modulesystem_data.name = on_half_dressed_module (old_subdir,new_subdirname) (x.Modulesystem_data.name);
      Modulesystem_data.ml_present = Modulesystem_data.ml_present x;
      Modulesystem_data.mli_present = Modulesystem_data.mli_present x;
      Modulesystem_data.mll_present = Modulesystem_data.mll_present x;
      Modulesystem_data.mly_present = Modulesystem_data.mly_present x;
      Modulesystem_data.ml_modification_time = Modulesystem_data.ml_modification_time x;
      Modulesystem_data.mli_modification_time = Modulesystem_data.mli_modification_time x;
      Modulesystem_data.mll_modification_time = Modulesystem_data.mll_modification_time x;
      Modulesystem_data.mly_modification_time = Modulesystem_data.mly_modification_time x;
      Modulesystem_data.needed_libraries = Modulesystem_data.needed_libraries x;
      Modulesystem_data.direct_fathers = Modulesystem_data.direct_fathers x;
      Modulesystem_data.all_ancestors = Modulesystem_data.all_ancestors x;
      Modulesystem_data.needed_directories = Modulesystem_data.needed_directories x;
};;


let on_absolute_path (old_subdir,new_subdirname) ap=
  let s_ap=Absolute_path.to_string ap in
  let old_fulldir=(Directory_name.to_string(German_constant.root))^old_subdir in
  if Substring.begins_with s_ap old_fulldir
  then let sub_s=Cull_string.cobeginning (String.length old_fulldir) s_ap in
       Absolute_path.of_string(new_subdirname^sub_s)
  else ap;;

let on_mlx_ended_absolute_path (old_subdir,new_subdirname) mlx=
  let old_ap=Mlx_ended_absolute_path.to_absolute_path mlx in
  let new_ap=on_absolute_path (old_subdir,new_subdirname) old_ap in
  Mlx_ended_absolute_path.of_path_and_root new_ap German_constant.root;;

let on_ocaml_target (old_subdir,new_subdirname)=function
  Ocaml_target.NO_DEPENDENCIES(mlx)->
  Ocaml_target.no_dependencies(on_mlx_ended_absolute_path (old_subdir,new_subdirname) mlx)
 |Ocaml_target.ML_FROM_MLL(hm)->Ocaml_target.ml_from_mll(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |Ocaml_target.ML_FROM_MLY(hm)->Ocaml_target.ml_from_mly(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |Ocaml_target.CMI(hm)->Ocaml_target.cmi(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |Ocaml_target.CMO(hm)->Ocaml_target.cmo(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |Ocaml_target.DCMO(hm)->Ocaml_target.dcmo(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |Ocaml_target.CMA(hm)->Ocaml_target.cma(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |Ocaml_target.CMX(hm)->Ocaml_target.cmx(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |Ocaml_target.EXECUTABLE(hm)->Ocaml_target.executable(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |Ocaml_target.DEBUGGABLE(hm)->Ocaml_target.debuggable(on_half_dressed_module (old_subdir,new_subdirname) hm)
 |Ocaml_target.TOPLEVEL(name,l)->
          let new_l=Image.image (on_half_dressed_module (old_subdir,new_subdirname)) l in
          Ocaml_target.TOPLEVEL(name,new_l);;

let on_half_dressed_modules (old_subdir,new_subdirname) l=
    Image.image (on_half_dressed_module (old_subdir,new_subdirname)) l ;; 

let on_data (old_subdir,new_subdirname) ldata=
   Image.image (on_ms_data (old_subdir,new_subdirname)) ldata;;
 
let on_subdirectories (old_subdir,new_subdirname) l_subdir=
   Image.image (on_subdirectory (old_subdir,new_subdirname)) l_subdir;; 
   
let on_up_to_date_targets (old_subdir,new_subdirname) l_tgts=
   Image.image (on_ocaml_target (old_subdir,new_subdirname)) l_tgts;;    

let on_outside_files (old_subdir,new_subdirname) l=
    Image.image (on_absolute_path (old_subdir,new_subdirname)) l ;; 

let on_deleted_files (old_subdir,new_subdirname) rl=
    let l=Recently_deleted.to_string_list rl in
    Recently_deleted.of_string_list(
     Image.image (on_short_path (old_subdir,new_subdirname)) l 
    );; 
   
let on_changed_files (old_subdir,new_subdirname) rl=
    let l=Recently_changed.to_string_list rl in
    Recently_changed.of_string_list(
     Image.image (on_short_path (old_subdir,new_subdirname)) l 
    );;    
   
let on_created_files (old_subdir,new_subdirname) rl=
    let l=Recently_created.to_string_list rl in
    Recently_created.of_string_list(
     Image.image (on_short_path (old_subdir,new_subdirname)) l 
    );;       
   
   
let on_delchacre_files (old_subdir,new_subdirname) l=
    Image.image (on_short_path (old_subdir,new_subdirname)) l ;; 
   


 
 
 
