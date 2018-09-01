
(* 

#use"Makefile_makers/modify_md_list.ml";;

*)

(* Getters  *)

let root x=x.Coma_state_t.root;;
let backup_dir x=x.Coma_state_t.dir_for_backup;;
   
let module_at_idx wmdata k = Small_array.get wmdata.Coma_state_t.modules k ;;
let subdir_at_idx wmdata k = Small_array.get wmdata.Coma_state_t.subdir_for_module k ;;
let principal_ending_at_idx wmdata k = Small_array.get wmdata.Coma_state_t.principal_ending_for_module k ;;
let mli_presence_at_idx wmdata k = Small_array.get wmdata.Coma_state_t.mli_presence_for_module k ;;
let principal_mt_at_idx wmdata k = Small_array.get wmdata.Coma_state_t.principal_mt_for_module k ;;
let mli_mt_at_idx wmdata k = Small_array.get wmdata.Coma_state_t.mli_mt_for_module k ;;
let needed_libs_at_idx wmdata k = Small_array.get wmdata.Coma_state_t.needed_libs_for_module k ;;
let direct_fathers_at_idx wmdata k = Small_array.get wmdata.Coma_state_t.direct_fathers_for_module k;;
let ancestors_at_idx wmdata k = Small_array.get wmdata.Coma_state_t.ancestors_for_module k ;; 
let needed_dirs_at_idx wmdata k = Small_array.get wmdata.Coma_state_t.needed_dirs_for_module k ;;

let directories x=x.Coma_state_t.directories;;
let targets x=x.Coma_state_t.targets;;
let preq_types x=x.Coma_state_t.printer_equipped_types;;

(* Setters  *)

let set_module_at_idx wmdata k v = Small_array.set wmdata.Coma_state_t.modules k v;;
let set_subdir_at_idx wmdata k v = Small_array.set wmdata.Coma_state_t.subdir_for_module k v ;;
let set_principal_ending_at_idx wmdata k v = Small_array.set wmdata.Coma_state_t.principal_ending_for_module k v ;;
let set_mli_presence_at_idx wmdata k v = Small_array.set wmdata.Coma_state_t.mli_presence_for_module k v ;;
let set_principal_mt_at_idx wmdata k v = Small_array.set wmdata.Coma_state_t.principal_mt_for_module k v ;;
let set_mli_mt_at_idx wmdata k v = Small_array.set wmdata.Coma_state_t.mli_mt_for_module k v ;;
let set_needed_libs_at_idx wmdata k v = Small_array.set wmdata.Coma_state_t.needed_libs_for_module k v ;;
let set_direct_fathers_at_idx wmdata k v = Small_array.set wmdata.Coma_state_t.direct_fathers_for_module k v ;;
let set_ancestors_at_idx wmdata k v = Small_array.set wmdata.Coma_state_t.ancestors_for_module k v ;; 
let set_needed_dirs_at_idx wmdata k v = Small_array.set wmdata.Coma_state_t.needed_dirs_for_module k v ;;

let set_directories x y=x.Coma_state_t.directories<- y;;
let set_targets x y=x.Coma_state_t.targets<- y;;
let set_preq_types x y=x.Coma_state_t.printer_equipped_types<- y;;





let empty_one x y={
     Coma_state_t.root =x;
     dir_for_backup =y;
     modules = Small_array.of_list [];
     subdir_for_module = Small_array.of_list [] ;
     principal_ending_for_module = Small_array.of_list [] ;
     mli_presence_for_module = Small_array.of_list [] ;
     principal_mt_for_module = Small_array.of_list [] ;
     mli_mt_for_module = Small_array.of_list [] ;
     needed_libs_for_module = Small_array.of_list [] ;
     direct_fathers_for_module = Small_array.of_list [];
     ancestors_for_module = Small_array.of_list [] ; 
     needed_dirs_for_module = Small_array.of_list [];
     directories =[];
     targets     =[];
     printer_equipped_types =[];
};;
  
let copy_mutables_from x y=(
     
     Small_array.copy_from x.Coma_state_t.modules y.Coma_state_t.modules;
     Small_array.copy_from x.Coma_state_t.subdir_for_module y.Coma_state_t.subdir_for_module;
     Small_array.copy_from x.Coma_state_t.principal_ending_for_module y.Coma_state_t.principal_ending_for_module;
     Small_array.copy_from x.Coma_state_t.mli_presence_for_module y.Coma_state_t.mli_presence_for_module;
     Small_array.copy_from x.Coma_state_t.principal_mt_for_module y.Coma_state_t.principal_mt_for_module;
     Small_array.copy_from x.Coma_state_t.mli_mt_for_module y.Coma_state_t.mli_mt_for_module;
     Small_array.copy_from x.Coma_state_t.needed_libs_for_module y.Coma_state_t.needed_libs_for_module;
     Small_array.copy_from x.Coma_state_t.direct_fathers_for_module y.Coma_state_t.direct_fathers_for_module;
     Small_array.copy_from x.Coma_state_t.ancestors_for_module y.Coma_state_t.ancestors_for_module;
     Small_array.copy_from x.Coma_state_t.subdir_for_module y.Coma_state_t.subdir_for_module;
     Small_array.copy_from x.Coma_state_t.needed_dirs_for_module y.Coma_state_t.needed_dirs_for_module;
     x.directories <- y.directories ;
     x.targets  <- y.targets ;
     x.printer_equipped_types <- y.printer_equipped_types ;
);;


let find_module_index wmdata nm=
  Small_array.leftmost_index_of_in
   nm (wmdata.Coma_state_t.modules);;   

let seek_module_index wmdata nm=
  try
  Some(find_module_index wmdata nm)
  with
  _->None;;   

let hm_at_idx wmdata k=
    let (Root_directory_t.R r)= root wmdata in
    {
      Half_dressed_module.bundle_main_dir = r;
      subdirectory = (Subdirectory.without_trailing_slash(subdir_at_idx wmdata k));
      naked_module = (Naked_module.to_string(module_at_idx wmdata k));
    } ;;
  
let hm_from_nm wmdata nm=
   let idx=find_module_index wmdata nm in
   hm_at_idx wmdata idx;;

let check_ending_in_at_idx edg wmdata idx=
   if edg=principal_ending_at_idx wmdata idx
   then true 
   else 
   if edg=Ocaml_ending.mli
   then mli_presence_at_idx wmdata idx
   else false;;

let acolytes_at_idx wmdata idx=
  let name=hm_at_idx wmdata idx in
  Option.filter_and_unpack (fun 
  edg->
     if check_ending_in_at_idx edg wmdata idx
     then Some(Mlx_ended_absolute_path.join name edg)
     else None
) Ocaml_ending.all_endings;;

let short_paths_at_idx wmdata idx=
   Image.image Mlx_ended_absolute_path.short_path (acolytes_at_idx wmdata idx);;
  

let registered_endings_at_idx wmdata idx=
  List.filter (fun edg->
  check_ending_in_at_idx edg wmdata idx 
  ) Ocaml_ending.all_endings;;


let check_for_single_ending_at_idx wmdata idx=
  if mli_presence_at_idx wmdata idx
  then (principal_ending_at_idx wmdata idx)=Ocaml_ending.mli
  else true ;;

let remove_in_each_at_index wmdata idx=
    (
      Small_array.remove_item_at_index wmdata.Coma_state_t.modules idx;
      Small_array.remove_item_at_index wmdata.Coma_state_t.subdir_for_module idx;
      Small_array.remove_item_at_index wmdata.Coma_state_t.principal_ending_for_module idx; 
      Small_array.remove_item_at_index wmdata.Coma_state_t.mli_presence_for_module idx; 
      Small_array.remove_item_at_index wmdata.Coma_state_t.principal_mt_for_module idx; 
      Small_array.remove_item_at_index wmdata.Coma_state_t.mli_mt_for_module idx; 
      Small_array.remove_item_at_index wmdata.Coma_state_t.needed_libs_for_module idx; 
      Small_array.remove_item_at_index wmdata.Coma_state_t.direct_fathers_for_module idx; 
      Small_array.remove_item_at_index wmdata.Coma_state_t.ancestors_for_module idx; 
      Small_array.remove_item_at_index wmdata.Coma_state_t.needed_dirs_for_module idx;
    );;
  
let push_right_in_each wmdata (hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned)=
  let nm=Half_dressed_module.naked_module hm
  and subdir=Half_dressed_module.subdirectory hm in
  (
    Small_array.push_right wmdata.Coma_state_t.modules nm;
    Small_array.push_right wmdata.Coma_state_t.subdir_for_module subdir;
    Small_array.push_right wmdata.Coma_state_t.principal_ending_for_module pr_end; 
    Small_array.push_right wmdata.Coma_state_t.mli_presence_for_module mlir; 
    Small_array.push_right wmdata.Coma_state_t.principal_mt_for_module prmt; 
    Small_array.push_right wmdata.Coma_state_t.mli_mt_for_module mlimt; 
    Small_array.push_right wmdata.Coma_state_t.needed_libs_for_module libned; 
    Small_array.push_right wmdata.Coma_state_t.direct_fathers_for_module dirfath; 
    Small_array.push_right wmdata.Coma_state_t.ancestors_for_module allanc; 
    Small_array.push_right wmdata.Coma_state_t.needed_dirs_for_module dirned;
  );;
   
let set_in_each wmdata idx (hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned)=
    let nm=Half_dressed_module.naked_module hm
    and subdir=Half_dressed_module.subdirectory hm in
    (
      Small_array.set wmdata.Coma_state_t.modules idx nm;
      Small_array.set wmdata.Coma_state_t.subdir_for_module idx subdir;
      Small_array.set wmdata.Coma_state_t.principal_ending_for_module idx pr_end; 
      Small_array.set wmdata.Coma_state_t.mli_presence_for_module idx mlir; 
      Small_array.set wmdata.Coma_state_t.principal_mt_for_module idx prmt; 
      Small_array.set wmdata.Coma_state_t.mli_mt_for_module idx mlimt; 
      Small_array.set wmdata.Coma_state_t.needed_libs_for_module idx libned; 
      Small_array.set wmdata.Coma_state_t.direct_fathers_for_module idx dirfath; 
      Small_array.set wmdata.Coma_state_t.ancestors_for_module idx allanc; 
      Small_array.set wmdata.Coma_state_t.needed_dirs_for_module idx dirned;
    );;  
    
let push_after_in_each wmdata idx (hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned)=
    let nm=Half_dressed_module.naked_module hm
    and subdir=Half_dressed_module.subdirectory hm in
    (
      Small_array.push_immediately_after_idx wmdata.Coma_state_t.modules nm idx;
      Small_array.push_immediately_after_idx wmdata.Coma_state_t.subdir_for_module subdir idx ;
      Small_array.push_immediately_after_idx wmdata.Coma_state_t.principal_ending_for_module pr_end idx; 
      Small_array.push_immediately_after_idx wmdata.Coma_state_t.mli_presence_for_module mlir idx; 
      Small_array.push_immediately_after_idx wmdata.Coma_state_t.principal_mt_for_module prmt idx; 
      Small_array.push_immediately_after_idx wmdata.Coma_state_t.mli_mt_for_module mlimt idx; 
      Small_array.push_immediately_after_idx wmdata.Coma_state_t.needed_libs_for_module libned idx; 
      Small_array.push_immediately_after_idx wmdata.Coma_state_t.direct_fathers_for_module dirfath idx; 
      Small_array.push_immediately_after_idx wmdata.Coma_state_t.ancestors_for_module allanc idx; 
      Small_array.push_immediately_after_idx wmdata.Coma_state_t.needed_dirs_for_module dirned idx;
    );;      

let reposition_in_each wmdata idx1 idx2=
    let rep=(fun x->
    Small_array.reposition_by_putting_snd_immediately_after_fst x idx1 idx2) in
     
    (
      rep wmdata.Coma_state_t.modules;
      rep wmdata.Coma_state_t.subdir_for_module;
      rep wmdata.Coma_state_t.principal_ending_for_module; 
      rep wmdata.Coma_state_t.mli_presence_for_module; 
      rep wmdata.Coma_state_t.principal_mt_for_module; 
      rep wmdata.Coma_state_t.mli_mt_for_module; 
      rep wmdata.Coma_state_t.needed_libs_for_module; 
      rep wmdata.Coma_state_t.direct_fathers_for_module; 
      rep wmdata.Coma_state_t.ancestors_for_module; 
      rep wmdata.Coma_state_t.needed_dirs_for_module;
    );;    

let reorder wmdata ordered_list_of_modules =
     let old_modules=Small_array.copy wmdata.Coma_state_t.modules
     and old_subdirs=Small_array.copy wmdata.Coma_state_t.subdir_for_module
     and old_pr_endings=Small_array.copy wmdata.Coma_state_t.principal_ending_for_module 
     and old_mli_presences=Small_array.copy wmdata.Coma_state_t.mli_presence_for_module 
     and old_pr_mts=Small_array.copy wmdata.Coma_state_t.principal_mt_for_module 
     and old_mli_mts=Small_array.copy wmdata.Coma_state_t.mli_mt_for_module 
     and old_libs=Small_array.copy wmdata.Coma_state_t.needed_libs_for_module 
     and old_fathers=Small_array.copy wmdata.Coma_state_t.direct_fathers_for_module 
     and old_ancestors=Small_array.copy wmdata.Coma_state_t.ancestors_for_module 
     and old_dirs=Small_array.copy wmdata.Coma_state_t.needed_dirs_for_module in
     let arr=Array.of_list ordered_list_of_modules in
      ( for k=1 to Array.length arr do
        let current_module=Array.get arr (k-1) in
        let idx=Small_array.leftmost_index_of_in current_module old_modules in
        Small_array.set wmdata.Coma_state_t.modules k  (Small_array.get old_modules idx) ;
        Small_array.set wmdata.Coma_state_t.subdir_for_module k (Small_array.get old_subdirs idx)  ;
        Small_array.set wmdata.Coma_state_t.principal_ending_for_module k (Small_array.get old_pr_endings idx)  ; 
        Small_array.set wmdata.Coma_state_t.mli_presence_for_module k (Small_array.get old_mli_presences idx)  ; 
        Small_array.set wmdata.Coma_state_t.principal_mt_for_module k (Small_array.get old_pr_mts idx)  ; 
        Small_array.set wmdata.Coma_state_t.mli_mt_for_module k (Small_array.get old_mli_mts idx)  ; 
        Small_array.set wmdata.Coma_state_t.needed_libs_for_module k (Small_array.get old_libs idx)  ; 
        Small_array.set wmdata.Coma_state_t.direct_fathers_for_module k (Small_array.get old_fathers idx)  ; 
        Small_array.set wmdata.Coma_state_t.ancestors_for_module k (Small_array.get old_ancestors idx)  ; 
        Small_array.set wmdata.Coma_state_t.needed_dirs_for_module k (Small_array.get old_dirs idx)  ;
      done;
      );;    

let size wmdata = Small_array.size wmdata.Coma_state_t.modules;;      

module Private=struct

let debuggable_targets_from_ancestor_data pr_end hm=
    match pr_end with
     Ocaml_ending.Mll-> 
        let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
             [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
    |Ocaml_ending.Mly-> 
        let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
        [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
    |Ocaml_ending.Ml-> 
             let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
             [ml_target;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
    |Ocaml_ending.Mli-> 
             let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
             [mli_target;Ocaml_target.cmi hm];;    
    
let immediate_ingredients_for_debuggable hm=
        [Ocaml_target.dcmo hm;Ocaml_target.debuggable hm];;  
    
end;;  

let debuggable_targets_from_ancestors wmdata ancestors=
    let temp1=Image.image (fun hm2->
           let idx2=find_module_index wmdata hm2 in
           let pr_end2=principal_ending_at_idx wmdata idx2 
           and hm2=hm_at_idx wmdata idx2 in
           Private.debuggable_targets_from_ancestor_data pr_end2 hm2
         ) ancestors in
    Preserve_initial_ordering.preserve_initial_ordering temp1;;

let find_needed_data_for_file wmdata fn=
      let temp1=Look_for_module_names.names_in_file fn in
      Small_array.indices_of_property_of_in 
      (fun nm->List.mem nm temp1)
      wmdata.Coma_state_t.modules;; 

let find_needed_data wmdata mlx=
      let fn=Mlx_ended_absolute_path.to_path mlx in
      find_needed_data_for_file wmdata fn;;         

let needed_dirs_and_libs_in_command is_optimized wmdata idx=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let s_root=Root_directory.connectable_to_subpath(root wmdata) in
   let dirs=
   "-I "^s_root^"_build"
  and libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    (needed_libs_at_idx wmdata idx)) in
    String.concat " " ["";dirs;libs;""];;

let needed_dirs_and_libs_for_several is_optimized wmdata l_idx=
   let extension=(if is_optimized then ".cmxa" else ".cma") in
   let pre_dirs1=Image.image 
     (fun idx->Tidel.diforchan(needed_dirs_at_idx wmdata idx)) l_idx in
   let pre_dirs2=Ordered.forget_order (Tidel.big_teuzin pre_dirs1) in
   let dirs=String.concat(" ")
    (Image.image(fun y->let z=Subdirectory.connectable_to_subpath(y) in 
    if z="" then "" else "-I "^z )
    pre_dirs2) in
   let pre_libs1=Image.image 
     (fun idx->Tidel.diforchan(needed_libs_at_idx wmdata idx)) l_idx in
   let pre_libs2=Ordered.forget_order (Tidel.big_teuzin pre_libs1) in 
   let libs=String.concat(" ")
    (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
    pre_libs2) in
    String.concat " " ["";dirs;libs;""];;

let ingredients_for_debuggable wmdata hm=
      let mlfile=Mlx_ended_absolute_path.join hm Ocaml_ending.Ml in
      let genealogy=find_needed_data wmdata mlfile in
      let dirfath=Image.image (module_at_idx wmdata) genealogy in
      let temp1=Image.image 
             (fun idx->
             Tidel.diforchan(ancestors_at_idx wmdata idx) 
             ) 
             genealogy in
       let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
       let temp3=Small_array.indices_of_property_of_in (
            fun nm->Tidel.elfenn nm temp2
       ) wmdata.Coma_state_t.modules in
       let allanc=Image.image (module_at_idx wmdata) temp3 in
      (debuggable_targets_from_ancestors wmdata allanc)
      @(Private.immediate_ingredients_for_debuggable hm);; 

let all_modules wmdata=
  let n=Small_array.size(wmdata.Coma_state_t.modules) in
  Ennig.doyle (hm_at_idx wmdata) 1 n;; 

let target_at_idx wmdata idx=
    let hm=hm_at_idx wmdata idx 
    and mlp=check_ending_in_at_idx Ocaml_ending.ml wmdata idx
    and mlip=check_ending_in_at_idx Ocaml_ending.mli wmdata idx
    and mllp=check_ending_in_at_idx Ocaml_ending.mll wmdata idx
    and mlyp=check_ending_in_at_idx Ocaml_ending.mly wmdata idx in
    let temp1=[
                mllp,Ocaml_target.ml_from_mll hm;
                mlyp,Ocaml_target.ml_from_mly hm;
           mlp||mlip,Ocaml_target.cmi hm;
           mlp||mlip,Ocaml_target.cmo hm;
           mlp||mlip,Ocaml_target.cma hm;
           mlp||mlip,Ocaml_target.cmx hm;
                 mlp,Ocaml_target.executable hm;
    ] in
    Option.filter_and_unpack 
      (fun x->if fst x 
              then Some(snd x) 
              else None) temp1;;  


let usual_targets wmdata=
  let n=Small_array.size(wmdata.Coma_state_t.modules) in
  let temp1=Ennig.doyle (target_at_idx wmdata) 1 n in
  List.flatten temp1;;

let outer_separator=Industrial_separator.modulesystem_data1;; 
let inner_separator=Industrial_separator.modulesystem_data2;; 

let archive wmdata=
  let (Root_directory_t.R t1)=wmdata.Coma_state_t.root 
  and (Root_directory_t.R t2)=wmdata.Coma_state_t.dir_for_backup  
  and  t3=wmdata.Coma_state_t.modules
  and  t4=wmdata.Coma_state_t.subdir_for_module
  and  t5=wmdata.Coma_state_t.principal_ending_for_module 
  and  t6=wmdata.Coma_state_t.mli_presence_for_module 
  and  t7=wmdata.Coma_state_t.principal_mt_for_module 
  and  t8=wmdata.Coma_state_t.mli_mt_for_module 
  and  t9=wmdata.Coma_state_t.needed_libs_for_module 
  and t10=wmdata.Coma_state_t.direct_fathers_for_module 
  and t11=wmdata.Coma_state_t.ancestors_for_module 
  and t12=wmdata.Coma_state_t.needed_dirs_for_module
  and t13=wmdata.Coma_state_t.directories 
  and t14=wmdata.Coma_state_t.targets 
  and t15=wmdata.Coma_state_t.printer_equipped_types in
  let list_arch=(fun old_arch a_list->
  Nonblank.make(String.concat inner_separator 
     (Image.image old_arch a_list)
  )) in 
  let arrlist_arch=(fun old_arch->
  Small_array.archive (list_arch old_arch)
  ) 
  and id=(fun s->s) in
  String.concat outer_separator
  [
    t1 ;
    t2 ;
   Small_array.archive (fun (Naked_module_t.N s)->s) t3;
   Small_array.archive (fun (Subdirectory_t.SD s)->s) t4;
   Small_array.archive Ocaml_ending.to_string t5;
   Small_array.archive string_of_bool t6;
   Small_array.archive id t7;
   Small_array.archive id t8;
   arrlist_arch Ocaml_library.to_string t9;
   arrlist_arch (fun (Naked_module_t.N s)->s) t10;
   arrlist_arch (fun (Naked_module_t.N s)->s) t11;
   arrlist_arch (fun (Subdirectory_t.SD s)->s) t12;
   list_arch (fun w->Nonblank.make(Subdirectory.without_trailing_slash w)) t13;
   list_arch Ocaml_target.archive t14;
   list_arch Half_dressed_module.archive_pair t15;
  ];;

  


     
let unarchive s=
    let temp1=Str.split (Str.regexp_string outer_separator) s in
    let  list_unarch=(fun old_unarch s->
      let ttemp2=Str.split
      (Str.regexp_string inner_separator) (Nonblank.decode s) in
      Image.image old_unarch ttemp2)
    and part=(fun j->List.nth temp1 (j-1)) in
    let arrlist_unarch=(fun old_unarch->
      Small_array.unarchive (list_unarch old_unarch)) 
    and id=(fun s->s) in
   {
    Coma_state_t.root = Root_directory_t.R(part 1);
    dir_for_backup = Root_directory_t.R(part 2);
    modules = Small_array.unarchive (fun s->Naked_module_t.N s) (part 3);
    subdir_for_module = Small_array.unarchive (fun s->Subdirectory_t.SD s) (part 4);
    principal_ending_for_module = Small_array.unarchive Ocaml_ending.of_string (part 5) ;
    mli_presence_for_module = Small_array.unarchive bool_of_string (part 6) ;
    principal_mt_for_module = Small_array.unarchive id (part 7) ;
    mli_mt_for_module = Small_array.unarchive id (part 8) ;
    needed_libs_for_module = arrlist_unarch Ocaml_library.of_string (part 9) ;
    direct_fathers_for_module = arrlist_unarch (fun s->Naked_module_t.N s) (part 10);
    ancestors_for_module = arrlist_unarch (fun s->Naked_module_t.N s) (part 11) ; 
    needed_dirs_for_module = arrlist_unarch (fun s->Subdirectory_t.SD s) (part 12);
    directories = list_unarch (fun v->Subdirectory.of_string(Nonblank.decode v)) (part 13);
    targets = list_unarch Ocaml_target.unarchive (part 14) ; 
    printer_equipped_types = list_unarch Half_dressed_module.unarchive_pair (part 15);

 };; 

 
 



exception Non_existent_mtime of Mlx_ended_absolute_path.t;;

let force_modification_time root_dir wmdata mlx=
      let hm=Mlx_ended_absolute_path.half_dressed_core mlx
      and edg=Mlx_ended_absolute_path.ending mlx in
      let nm=Half_dressed_module.naked_module hm in
      let idx=
        (try Small_array.leftmost_index_of_in
          nm wmdata.Coma_state_t.modules with 
        _->raise(Non_existent_mtime(mlx)) ) in
      let file=(Root_directory.connectable_to_subpath root_dir)^
               (Mlx_ended_absolute_path.to_string mlx) in
      let new_val=string_of_float((Unix.stat file).Unix.st_mtime)  in
      let _=(
        if edg=principal_ending_at_idx wmdata idx 
        then set_principal_mt_at_idx wmdata idx new_val
      ) in
      let _=(
        if edg=Ocaml_ending.mli 
        then set_mli_mt_at_idx wmdata idx new_val
      ) in     
      wmdata;;

let everyone_except_the_debugger wmdata=
        let n=Small_array.size wmdata.Coma_state_t.modules in
        let debugged_nm=Naked_module.of_string 
            Coma_constant.name_for_debugged_module in
        let debugged_idx=Small_array.leftmost_index_of_in 
           debugged_nm wmdata.Coma_state_t.modules in
        Option.filter_and_unpack (fun idx->
           if idx=debugged_idx
           then None
           else Some(hm_at_idx wmdata idx)
        ) (Ennig.ennig 1 n);;      
        


exception Non_registered_module of Half_dressed_module.t;;  
exception Derelict_children of Naked_module_t.t*(Naked_module_t.t list);;  
           
            
let unregister_module_on_monitored_modules wmdata hm=
  let nm=Half_dressed_module.naked_module hm in
  let n=Small_array.size wmdata.Coma_state_t.modules in
  let pre_desc=List.filter(
      fun idx->List.mem nm ( ancestors_at_idx wmdata idx )
  ) (Ennig.ennig 1 n) in
   if pre_desc<>[]
   then let temp1=Image.image ( module_at_idx wmdata ) pre_desc in
        raise(Derelict_children(nm,temp1))
   else
   let idx=
    (try Small_array.leftmost_index_of_in
      nm wmdata.Coma_state_t.modules with 
    _->raise(Non_registered_module(hm)) ) in
    let acolytes=acolytes_at_idx wmdata idx  in
   let _=remove_in_each_at_index wmdata idx in
   let short_paths=Image.image Mlx_ended_absolute_path.short_path acolytes in
   (wmdata,short_paths);;     
                    

exception Non_registered_file of Mlx_ended_absolute_path.t;;  
exception Abandoned_children of Mlx_ended_absolute_path.t*(Naked_module_t.t list);;
                      
                     
let unregister_mlx_file_on_monitored_modules wmdata mlxfile=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlxfile in
    let nm=Half_dressed_module.naked_module hm in
    let n=Small_array.size wmdata.Coma_state_t.modules in
    let pre_desc=List.filter(
      fun idx->List.mem nm ( ancestors_at_idx wmdata idx )
    ) (Ennig.ennig 1 n) in
    if pre_desc<>[]
    then let temp1=Image.image ( module_at_idx wmdata ) pre_desc in
        raise(Abandoned_children(mlxfile,temp1))
    else
    let idx=
      (try Small_array.leftmost_index_of_in
        nm wmdata.Coma_state_t.modules with 
      _->raise(Non_registered_file(mlxfile)) ) in
    let edg=Mlx_ended_absolute_path.ending mlxfile in
    if (not(check_ending_in_at_idx edg wmdata idx))
    then raise(Non_registered_file(mlxfile))
    else let _=(if check_for_single_ending_at_idx wmdata idx
                then remove_in_each_at_index wmdata idx
                else (* if we get here, there are two registered endings, one of which
                       is the mli *) 
                     if edg=Ocaml_ending.mli
                     then (
                       Small_array.set wmdata.Coma_state_t.mli_presence_for_module idx false;
                       Small_array.set wmdata.Coma_state_t.mli_mt_for_module idx "0.";
                     )
                     else 
                     let old_mt=Small_array.get wmdata.Coma_state_t.principal_mt_for_module idx in
                     (
                      Small_array.set wmdata.Coma_state_t.principal_ending_for_module idx Ocaml_ending.mli;
                      Small_array.set wmdata.Coma_state_t.principal_mt_for_module idx old_mt;
                    )
                ) in
                wmdata;;
            


let compute_subdirectories_list wmdata=
  let temp1=Small_array.image 
        Subdirectory.without_trailing_slash wmdata.Coma_state_t.subdir_for_module in
    let temp2=Ordered_string.diforchan temp1 in
    let temp3=Ordered_string.forget_order temp2 in
    Image.image Subdirectory.of_string temp3;;

let  check_registrations wmdata hm=
   let nm=Half_dressed_module.naked_module hm in 
    match seek_module_index wmdata nm with
      None->Ocaml_ending.exhaustive_uple (fun _->false)
    |Some(idx)->Ocaml_ending.exhaustive_uple 
      (fun edg->check_ending_in_at_idx edg wmdata idx);;

module PrivateTwo=struct

let find_needed_names wmdata mlx=
  let temp1=find_needed_data wmdata mlx in
  Image.image (Small_array.get wmdata.Coma_state_t.modules ) temp1;;  

let find_needed_libraries wmdata mlx genealogy=
  let fn=Mlx_ended_absolute_path.to_path mlx in
  let temp1=Look_for_module_names.names_in_file fn in
  List.filter
  (
    fun lib->
      if List.exists 
         (fun mdl->List.mem(Naked_module.of_string mdl)(temp1))
           (Ocaml_library.modules_telling_a_library_away lib)
      then true
      else List.exists 
           (fun k->List.mem lib (Small_array.get wmdata.Coma_state_t.needed_libs_for_module k) ) 
           genealogy
  )
  Ocaml_library.all_libraries;;


let find_needed_directories wmdata mlx genealogy=
  let temp1=Image.image 
    (fun t->Tidel.diforchan(Small_array.get wmdata.Coma_state_t.needed_dirs_for_module t)) 
      genealogy in
  let s_mlx=Mlx_ended_absolute_path.to_string mlx in
  let temp2=(fun bowl->
      if bowl 
      then let new_subdir=Subdirectory.of_string(Father_and_son.father s_mlx '/') in
           Tidel.singleton(new_subdir)::temp1
      else temp1
  )(String.contains s_mlx '/') in    
  let temp3=Tidel.big_teuzin temp2 in
  Ordered.forget_order temp3;;
              
                    
end;;  

let compute_principal_ending (mlr,mlir,mllr,mlyr)=
    let temp1=List.combine
      [mlr;mllr;mlyr]
      [Ocaml_ending.ml;Ocaml_ending.mll;Ocaml_ending.mly] in
    let temp2=Option.filter_and_unpack (
       fun (bowl,ending)->if bowl then Some(ending) else None 
    ) temp1 in
    if temp2=[] then Ocaml_ending.mli else List.hd temp2;;

let md_compute_modification_time hm edg=
  let dir=Half_dressed_module.bundle_main_dir hm in
  let mlx=Mlx_ended_absolute_path.join hm edg in
  let file=(Root_directory.connectable_to_subpath dir)^(Mlx_ended_absolute_path.to_string mlx) in
  if not(Sys.file_exists file) then "0." else
  let st=Unix.stat file in
  string_of_float(st.Unix.st_mtime);;

let md_compute_modification_times hm=
      Ocaml_ending.exhaustive_uple (md_compute_modification_time hm);;
    
let md_associated_modification_time  (ml_mt,mli_mt,mly_mt,mll_mt) edg=match edg with
     Ocaml_ending.Ml->ml_mt
    |Ocaml_ending.Mli->mli_mt
    |Ocaml_ending.Mll->mll_mt
    |Ocaml_ending.Mly->mly_mt;;  

let complete_info wmdata  mlx=
  let n=Small_array.size(wmdata.Coma_state_t.modules) in
  let (hm,edg)=Mlx_ended_absolute_path.decompose mlx in
  let genealogy=find_needed_data wmdata mlx in
  let (mlr,mlir,mllr,mlyr)=check_registrations wmdata hm
  and (mlmt,mlimt,mllmt,mlymt)=md_compute_modification_times hm in
  let pr_end=compute_principal_ending (mlr,mlir,mllr,mlyr) in
  let prmt=md_associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
  let dirfath=Image.image (Small_array.get wmdata.Coma_state_t.modules) genealogy in
  let temp1=Image.image 
        (fun t->Tidel.diforchan(Small_array.get wmdata.Coma_state_t.ancestors_for_module t)) 
        genealogy in
  let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
  let tempf=(fun t->
            let nam_t=Small_array.get wmdata.Coma_state_t.modules t in
            if Tidel.elfenn nam_t temp2
            then Some(nam_t)
            else None) in
  let allanc=Option.filter_and_unpack tempf (Ennig.ennig 1 n) in
  let libned=PrivateTwo.find_needed_libraries wmdata mlx genealogy
  and dirned=PrivateTwo.find_needed_directories wmdata mlx genealogy in
  (hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned);;

  let check_unix_presence hm edg=
    let (_,dir)=Half_dressed_module.unveil hm in
    let s_hm=Half_dressed_module.uprooted_version hm 
    and s_dir=Root_directory.connectable_to_subpath dir in
    Sys.file_exists(s_dir^s_hm^(Ocaml_ending.to_string edg));;

let  check_unix_presences hm=
    Ocaml_ending.exhaustive_uple (fun edg->check_unix_presence hm edg);;  

let registrations_for_lonely_ending =function
   Ocaml_ending.Ml->(true,false,false,false)
  |Ocaml_ending.Mli->(false,true,false,false)
  |Ocaml_ending.Mll->(false,false,true,false)
  |Ocaml_ending.Mly->(false,false,false,true);;  



let complete_info_during_new_module_registration wmdata  mlx=
  let n=Small_array.size(wmdata.Coma_state_t.modules) in
    let (hm,edg)=Mlx_ended_absolute_path.decompose mlx in
    let genealogy=find_needed_data wmdata mlx in
    let (mlp,mlir,mllr,mlyr)=registrations_for_lonely_ending edg
    and (mlmt,mlimt,mllmt,mlymt)=md_compute_modification_times hm in
    let pr_end=edg in
    let prmt=md_associated_modification_time (mlmt,mlimt,mllmt,mlymt) pr_end in
    let dirfath=Image.image (Small_array.get wmdata.Coma_state_t.modules) genealogy in
    let temp1=Image.image 
          (fun t->Tidel.diforchan(Small_array.get wmdata.Coma_state_t.ancestors_for_module t)) 
          genealogy in
    let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
    let tempf=(fun t->
              let nam_t=Small_array.get wmdata.Coma_state_t.modules t in
              if Tidel.elfenn nam_t temp2
              then Some(nam_t)
              else None) in
    let allanc=Option.filter_and_unpack tempf (Ennig.ennig 1 n) in
    let libned=PrivateTwo.find_needed_libraries wmdata mlx genealogy
    and dirned=PrivateTwo.find_needed_directories wmdata mlx genealogy in
    (hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned);;
  
  
  

exception Nonregistered_module of Naked_module_t.t;;



let rename_module_on_monitored_modules root_dir wmdata old_name new_name=
  let n=Small_array.size wmdata.Coma_state_t.modules in
  let old_nm=Half_dressed_module.naked_module old_name in
  let opt_idx=seek_module_index wmdata old_nm in
  if opt_idx=None
  then raise(Nonregistered_module(old_nm))
  else 
  let idx=Option.unpack opt_idx in
  let old_acolytes=acolytes_at_idx wmdata idx in
  let old_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) 
       old_acolytes in 
  let new_acolytes=Image.image 
     (fun mlx->Mlx_ended_absolute_path.do_file_renaming mlx new_name) 
     old_acolytes in
  let new_files=Image.image (fun mlx->Mlx_ended_absolute_path.short_path mlx) 
     new_acolytes in 
  let new_hm=Mlx_ended_absolute_path.half_dressed_core(List.hd new_acolytes) in
  let old_mname=Half_dressed_module.naked_module old_name
  and new_mname=Half_dressed_module.naked_module new_hm
  in
  let changer=Look_for_module_names.change_module_name_in_file
  old_mname new_mname in
  let separated_acolytes=Option.filter_and_unpack(
    fun k->
     if List.mem old_mname (Small_array.get wmdata.ancestors_for_module k)
    then Some(acolytes_at_idx wmdata k)
    else None
) (Ennig.ennig 1 n) in
  let all_acolytes=List.flatten separated_acolytes in
  let temp3=Image.image Mlx_ended_absolute_path.to_path all_acolytes in
  let temp4=Option.filter_and_unpack (
    fun s->try Some(Absolute_path.of_string s) with _->None
  ) [
      Coma_constant.name_for_printersfile;
    ] in
  let _=Image.image changer (temp3@temp4) in
  let s_root=Root_directory.connectable_to_subpath root_dir in     
  let _=Unix_command.uc
      ("rm -f "^s_root^"_build/"^
      (Half_dressed_module.uprooted_version old_name)^
      ".cm* ") in
  let principal_mt=md_compute_modification_time new_hm (principal_ending_at_idx wmdata idx)
  and mli_mt=md_compute_modification_time new_hm Ocaml_ending.mli in
  let _=(
    Small_array.set wmdata.Coma_state_t.modules idx new_mname;
    Small_array.set wmdata.Coma_state_t.principal_mt_for_module idx principal_mt;
    Small_array.set wmdata.Coma_state_t.mli_mt_for_module idx mli_mt; 
  ) in
  let replacer=Image.image(function x->if x=old_mname then new_mname else x) in
  let _=(
     for k=idx+1 to n do
      let old_dirfath=Small_array.get wmdata.Coma_state_t.direct_fathers_for_module k
      and old_ancestors=Small_array.get wmdata.Coma_state_t.ancestors_for_module k in
     Small_array.set wmdata.Coma_state_t.direct_fathers_for_module k (replacer old_dirfath) ;
     Small_array.set wmdata.Coma_state_t.ancestors_for_module k (replacer old_ancestors); 
     done;
  ) in
  (wmdata,(old_files,new_files));;


let recompute_complete_info_for_module wmdata hm=
      let nm=Half_dressed_module.naked_module hm in
      let idx=find_module_index wmdata nm in
      let edg=List.hd(registered_endings_at_idx wmdata idx) in
      let mlx=Mlx_ended_absolute_path.join hm edg in
      complete_info wmdata mlx;;

let recompute_module_info wmdata hm=
  let nm=Half_dressed_module.naked_module hm in
  let idx=find_module_index wmdata nm in
  let new_dt=recompute_complete_info_for_module wmdata hm in 
  let _=set_in_each wmdata idx new_dt in
  wmdata;;  

exception Nonregistered_module_during_relocation of Half_dressed_module.t;;  
          
let relocate_module_on_monitored_modules root_dir wmdata old_name new_subdir=
  let old_nm=Half_dressed_module.naked_module old_name in
  let opt_idx=seek_module_index wmdata old_nm in
  if opt_idx=None
  then raise(Nonregistered_module_during_relocation(old_name))
  else 
  let idx=Option.unpack opt_idx in 
  let old_acolytes=acolytes_at_idx wmdata idx in
  let old_files=Image.image Mlx_ended_absolute_path.short_path old_acolytes in 
  let new_acolytes=Image.image 
    (fun mlx->Mlx_ended_absolute_path.do_file_displacing mlx new_subdir) old_acolytes in
  let new_files=Image.image 
     (fun mlx->Mlx_ended_absolute_path.short_path mlx) new_acolytes in 
  let new_name=Mlx_ended_absolute_path.half_dressed_core
   (List.hd new_acolytes) in
  let s_root=Root_directory.connectable_to_subpath root_dir in     
    let _=Unix_command.uc
     ("rm -f "^s_root^"_build/"^(Half_dressed_module.uprooted_version old_name)^".cm* ") in
  let principal_mt=md_compute_modification_time new_name (principal_ending_at_idx wmdata idx)
  and mli_mt=md_compute_modification_time new_name Ocaml_ending.mli in
  let _=(
       Small_array.set wmdata.Coma_state_t.subdir_for_module idx new_subdir;
       Small_array.set wmdata.Coma_state_t.principal_mt_for_module idx principal_mt;
       Small_array.set wmdata.Coma_state_t.mli_mt_for_module idx mli_mt; 
  ) in   
  (wmdata,(old_files,new_files));;



let above wmdata hm=
  let nm=Half_dressed_module.naked_module hm in
  match seek_module_index wmdata nm with
  None->raise(Non_registered_module(hm))
  |Some(idx)->Small_array.get wmdata.Coma_state_t.ancestors_for_module idx;;

let below wmdata hm=
  let nm=Half_dressed_module.naked_module hm 
  and n=Small_array.size wmdata.Coma_state_t.modules in
  Option.filter_and_unpack(fun idx->
      if List.mem nm (Small_array.get wmdata.Coma_state_t.ancestors_for_module idx)
      then Some(Small_array.get wmdata.Coma_state_t.modules idx)
      else None) (Ennig.ennig 1 n);;  

let directly_below wmdata hm=
        let nm=Half_dressed_module.naked_module hm 
        and n=Small_array.size wmdata.Coma_state_t.modules in
        Option.filter_and_unpack(fun idx->
            if List.mem nm (Small_array.get wmdata.Coma_state_t.direct_fathers_for_module idx)
            then Some(Small_array.get wmdata.Coma_state_t.modules idx)
            else None) (Ennig.ennig 1 n);;        

let all_mlx_files wmdata=
  let n=Small_array.size wmdata.Coma_state_t.modules in
  List.flatten(Ennig.doyle(acolytes_at_idx wmdata) 1 n);;                

      
let all_mlx_paths wmdata=Image.image Mlx_ended_absolute_path.to_absolute_path 
        (all_mlx_files wmdata);;  

let all_short_paths wmdata=
    let n=Small_array.size wmdata.Coma_state_t.modules in
    List.flatten(Ennig.doyle(short_paths_at_idx wmdata) 1 n);;  

let files_containing_string wmdata some_string=
let temp1=all_mlx_paths wmdata in
List.filter (fun ap->Substring.is_a_substring_of 
  some_string (Io.read_whole_file ap)) temp1;;


let system_size wmdata=Small_array.size(wmdata.Coma_state_t.modules);;

exception Inconsistent_constraints of Naked_module_t.t*Naked_module_t.t;;
exception Bad_upper_constraint of Naked_module_t.t;;  


exception Nonregistered_module_during_reposition of Half_dressed_module.t;;  

 
let reposition_module wmdata hm (l_before,l_after)=
    let n=Small_array.size(wmdata.Coma_state_t.modules) in 
    let find_idx=find_module_index wmdata in
    let main_idx=find_idx hm
    and indices_before=Image.image find_idx l_before
    and indices_after=Image.image find_idx l_after in
    let max_before=(if indices_before=[] then 1 else Max.list indices_before)
    and min_after=(if indices_after=[] then n else Min.list indices_after)
    in
    if max_before>min_after
    then let hm_before=Small_array.get wmdata.Coma_state_t.modules max_before
         and hm_after=Small_array.get wmdata.Coma_state_t.modules min_after in
         raise(Inconsistent_constraints(hm_before,hm_after))
    else 
    if max_before>main_idx
    then let hm_before=Small_array.get wmdata.Coma_state_t.modules max_before in
         raise(Bad_upper_constraint(hm_before))
    else 
    let _=reposition_in_each wmdata max_before main_idx in 
    wmdata;;  

let rename_directory_on_data (old_subdir,new_subdirname) wmdata= 
  let ren_sub=Subdirectory.rename_endsubdirectory (old_subdir,new_subdirname) in 
  let toa=Small_array.apply_transformation_on_all in
  let _=(
   toa wmdata.Coma_state_t.subdir_for_module ren_sub;
   toa wmdata.Coma_state_t.needed_dirs_for_module (Image.image ren_sub);
  ) in
  wmdata;;



let find_value_definition wmdata s= 
  if not(String.contains s '.')
  then None
  else
  let j1=String.index(s)('.')+1 in
  let module_name=Cull_string.beginning (j1-1) s in
  let nm=Naked_module.of_string(String.uncapitalize_ascii(module_name)) in
  let opt=seek_module_index wmdata nm in
  if opt=None
  then None 
  else
  let idx1=Option.unpack opt in
  let hm1=hm_at_idx wmdata idx1 in
  let ap1=Mlx_ended_absolute_path.to_path(Mlx_ended_absolute_path.join hm1 
     Ocaml_ending.Ml) in
  let temp1=Read_ocaml_files.read_ocaml_files [ap1] in	 
  Option.seek (
     fun itm->Ocaml_gsyntax_item.name(itm)=s
  ) temp1;;


let all_naked_modules wmdata=
  Small_array.image Naked_module.to_string wmdata.Coma_state_t.modules;;     

let all_ml_absolute_paths wmdata=
  let n=Small_array.size wmdata.Coma_state_t.modules in   
Option.filter_and_unpack (fun idx->
  if not(check_ending_in_at_idx Ocaml_ending.ml wmdata idx)
  then None
  else 
  let hm=hm_at_idx wmdata idx in
  let mlx=Mlx_ended_absolute_path.join hm Ocaml_ending.ml in
  Some(Mlx_ended_absolute_path.to_absolute_path mlx)
) (Ennig.ennig 1 n);;

let modules_using_value wmdata value_name =
  let n=Small_array.size wmdata.Coma_state_t.modules in 
  Option.filter_and_unpack (fun idx->
  let hm=hm_at_idx wmdata idx
  and pr_end=Small_array.get wmdata.Coma_state_t.principal_ending_for_module idx in
  let mlx=Mlx_ended_absolute_path.join hm pr_end in
   let ap=Mlx_ended_absolute_path.to_path mlx in
   if Substring.is_a_substring_of 
       value_name (Io.read_whole_file ap)
   then Some hm
   else None ) (Ennig.ennig 1 n);;




let update_ancs_libs_and_dirs_at_idx wmdata idx=
  let hm=hm_at_idx wmdata idx  
  and pr_end=Small_array.get wmdata.Coma_state_t.principal_ending_for_module idx in
  let mlx=Mlx_ended_absolute_path.join hm pr_end in 
  let fathers=Small_array.get wmdata.Coma_state_t.direct_fathers_for_module idx in
  let separated_ancestors=Image.image 
  (fun nm2->
    let idx2=Small_array.leftmost_index_of_in nm2 wmdata.Coma_state_t.modules in
    Tidel.safe_set(Small_array.get wmdata.Coma_state_t.ancestors_for_module idx2)
  ) fathers in
  let all_ancestors=Tidel.big_teuzin((Tidel.safe_set fathers)::separated_ancestors) in
  let unordered_ancestor_indices=Tidel.image (
    fun nm3->Small_array.leftmost_index_of_in nm3 wmdata.Coma_state_t.modules
  ) all_ancestors in
  let genealogy=Ordered.forget_order(Tidel.diforchan unordered_ancestor_indices) in
  let new_libs=PrivateTwo.find_needed_libraries wmdata mlx genealogy
  and new_dirs=PrivateTwo.find_needed_directories wmdata mlx genealogy 
  and ordered_ancestors=Image.image (
    Small_array.get wmdata.Coma_state_t.modules
  ) genealogy in
  (
    Small_array.set wmdata.Coma_state_t.ancestors_for_module idx ordered_ancestors;
    Small_array.set wmdata.Coma_state_t.needed_libs_for_module idx new_libs;
    Small_array.set wmdata.Coma_state_t.needed_dirs_for_module idx new_dirs;
  );;

let update_ancs_libs_and_dirs wmdata=
  let n=Small_array.size wmdata.Coma_state_t.modules in
  for idx=1 to n do
    update_ancs_libs_and_dirs_at_idx wmdata idx
  done;;  
  
  


    



module PrivateThree=struct

    let message_about_circular_dependencies printer cycles= 
      if cycles=[]
      then ""
      else
      let temp1=Image.image(fun cycle->
        let ttemp1=Image.image printer cycle in
         String.concat " -> " ttemp1 
      ) cycles in
      let temp2=String.concat "\n\n" temp1 in
      temp2;;
    
    exception Circular_dependencies of string;;
    
    let treat_circular_dependencies tolerate_cycles printer cycles=
      if cycles=[]
      then ()
      else let msg=message_about_circular_dependencies printer cycles in  
           if tolerate_cycles
           then (print_string msg;flush stdout)     
           else raise(Circular_dependencies(msg));; 
           
    let message_about_changed_modules changed_modules=
      let temp1=Image.image Naked_module.to_string changed_modules in
      "\n\n\n"^
      "The following modules have been directly changed :\n"^
      (String.concat "\n" temp1)^
      "\n\n\n"
    ;;       
           
    let announce_changed_modules changed_modules=
      if changed_modules=[]
      then ()
      else (print_string(message_about_changed_modules changed_modules);flush stdout);;
             
    
    let put_md_list_back_in_order tolerate_cycles 
      wmdata initially_active_nms=
      let md_list=Small_array.to_list wmdata.Coma_state_t.modules in
      let coat=Memoized.make (fun nm->
        let idx=Small_array.leftmost_index_of_in nm wmdata.Coma_state_t.modules in
        Small_array.get wmdata.Coma_state_t.direct_fathers_for_module idx
      ) in
      let (cycles,reordered_list)=Reconstruct_linear_poset.reconstruct_linear_poset 
         coat md_list in
      let _=treat_circular_dependencies tolerate_cycles
           (fun nm->
           let idx=Small_array.leftmost_index_of_in nm wmdata.Coma_state_t.modules in 
           Half_dressed_module.uprooted_version( hm_at_idx wmdata idx) )
           cycles in     
      let _=reorder wmdata (Image.image fst reordered_list) in    
      let _=update_ancs_libs_and_dirs wmdata in 
      let n=Small_array.size wmdata.Coma_state_t.modules in
      let active_descendants=Option.filter_and_unpack (
          fun idx->
            let nm=Small_array.get wmdata.Coma_state_t.modules idx in
            if List.mem nm initially_active_nms
            then Some(nm)
            else
            if List.exists (fun nm2->List.mem nm2 initially_active_nms) 
                 (Small_array.get wmdata.Coma_state_t.ancestors_for_module idx)
            then Some(nm)
            else None
      ) (Ennig.ennig 1 n) in  
      (wmdata,active_descendants);;
     
end;; 
     
let md_recompute_modification_time hm edg=
 let dir=Half_dressed_module.bundle_main_dir hm in
  let mlx=Mlx_ended_absolute_path.join hm edg in
  let file=(Root_directory.connectable_to_subpath dir)^(Mlx_ended_absolute_path.to_string mlx) in
  if not(Sys.file_exists file) then "0." else
  let st=Unix.stat file in
  string_of_float(st.Unix.st_mtime);;
  

let quick_update wmdata idx=
  let hm=hm_at_idx wmdata idx 
  and pr_ending=Small_array.get wmdata.Coma_state_t.principal_ending_for_module idx in
  if (Half_dressed_module.uprooted_version hm)=Coma_constant.name_for_debugged_module
  then None
  else
  let mli_modif_time=md_recompute_modification_time hm Ocaml_ending.mli 
  and pr_modif_time=md_recompute_modification_time hm pr_ending 
  and old_mli_modif_time=Small_array.get wmdata.Coma_state_t.mli_mt_for_module idx
  and old_pr_modif_time=Small_array.get wmdata.Coma_state_t.principal_mt_for_module idx 
  in
  let new_values=(mli_modif_time,pr_modif_time)
  and old_values=(old_mli_modif_time,old_pr_modif_time) in
  if old_values=new_values
  then None
  else
  let mlx=Mlx_ended_absolute_path.join hm pr_ending in
  let direct_fathers=PrivateTwo.find_needed_names wmdata mlx in
  Some(
    pr_modif_time,
    mli_modif_time,
    direct_fathers
   )   
  ;;
    

let recompile_on_monitored_modules tolerate_cycles wmdata = 
  let n=Small_array.size wmdata.Coma_state_t.modules in
  let ref_for_changed_modules=ref[] 
  and ref_for_changed_shortpaths=ref[] in
  let declare_changed=(fun idx->
    let nm=Small_array.get wmdata.Coma_state_t.modules idx in
    ref_for_changed_modules:=nm::(!ref_for_changed_modules);
    ref_for_changed_shortpaths:=((!ref_for_changed_shortpaths)@
                        (short_paths_at_idx wmdata idx))
    ) in
  let _=List.iter (fun idx->
    match quick_update wmdata idx with
    None->()
    |Some(pr_modif_time,mli_modif_time,direct_fathers)->
    (
    declare_changed(idx);
    Small_array.set wmdata.Coma_state_t.principal_mt_for_module idx pr_modif_time;
    Small_array.set wmdata.Coma_state_t.mli_mt_for_module idx mli_modif_time;
    Small_array.set wmdata.Coma_state_t.direct_fathers_for_module idx direct_fathers
    )
)(Ennig.ennig 1 n) in
let changed_modules=List.rev(!ref_for_changed_modules) in
if changed_modules=[] then ((wmdata,[]),[]) else
let _=PrivateThree.announce_changed_modules changed_modules in
(PrivateThree.put_md_list_back_in_order tolerate_cycles 
  wmdata changed_modules,
(!ref_for_changed_shortpaths));;  

let printer_equipped_types_from_data wmdata=
  let n=Small_array.size wmdata.Coma_state_t.modules in
  Option.filter_and_unpack (
    fun idx->
    let hm=hm_at_idx wmdata idx
    and pr_end=Small_array.get wmdata.Coma_state_t.principal_ending_for_module idx in
    let mlx=Mlx_ended_absolute_path.join hm pr_end in
    let ap=Mlx_ended_absolute_path.to_absolute_path mlx in
    let text=Io.read_whole_file ap in
    if (Substring.is_a_substring_of ("let "^"print_out ") text)
    then Some(hm)
    else None
  ) (Ennig.ennig 1 n);;
 



exception Already_registered_file of Mlx_ended_absolute_path.t;;  
exception Overcrowding of Mlx_ended_absolute_path.t*(Ocaml_ending.t list);;
exception Bad_pair of Mlx_ended_absolute_path.t*Ocaml_ending.t;; 


let register_mlx_file_on_monitored_modules wmdata mlx_file =
          let n=Small_array.size wmdata.Coma_state_t.modules in
          let hm=Mlx_ended_absolute_path.half_dressed_core mlx_file
          and ending=Mlx_ended_absolute_path.ending mlx_file in 
          let nm=Half_dressed_module.naked_module hm in
          let opt_idx=seek_module_index wmdata nm in
          if opt_idx=None
          then  let info=complete_info_during_new_module_registration wmdata mlx_file in
                let _=push_right_in_each wmdata info in         
                wmdata
          else
          let idx=Option.unpack(opt_idx) in
          let edgs=registered_endings_at_idx wmdata idx in
          if List.length(edgs)>1
          then  raise(Overcrowding(mlx_file,edgs))
          else  
          if List.mem ending edgs
          then raise(Already_registered_file(mlx_file))
          else
          if (not(List.mem Ocaml_ending.mli (ending::edgs)))
          then raise(Bad_pair(mlx_file,List.hd edgs))
          else 
          let (hm,old_pr_end,old_mlir,prmt,mlimt,libned,dirfath,allanc,dirned)=complete_info wmdata mlx_file in
          let nm=Half_dressed_module.naked_module hm in
          let (pr_end,mlir)=(
            if ending=Ocaml_ending.mli
            then (old_pr_end,true)
            else (ending,old_mlir) 
          ) in
          let new_dt=(hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned) in
          if ending<>Ocaml_ending.ml
          then let _=set_in_each wmdata idx new_dt in         
               wmdata
          else 
          let temp3=List.rev(dirfath) in
          if temp3=[]
          then let _=set_in_each wmdata idx new_dt in         
               wmdata
          else  
          let last_father=List.hd(temp3) in
          let last_father_idx=Small_array.leftmost_index_of_in last_father wmdata.Coma_state_t.modules in
          let _=
            (
              for k=last_father_idx+1 to n 
              do
              let current_anc= Small_array.get wmdata.Coma_state_t.ancestors_for_module k in  
              if not(List.mem nm current_anc)
              then ()
              else  
                   let current_libs= Small_array.get wmdata.Coma_state_t.needed_libs_for_module k in
                   let new_ancestors=Small_array.filter_and_unpack(
                      fun nm2->
                      if (List.mem nm2 allanc)||(List.mem nm2 current_anc)
                      then Some(nm2)
                      else None
                    ) wmdata.Coma_state_t.modules 
                    and new_libs=List.filter (
                      fun lib->(List.mem lib libned)||(List.mem lib current_libs)
                    ) Ocaml_library.all_libraries in  
                    let ordered_dirs=Tidel.teuzin
                       (Tidel.safe_set(Small_array.get wmdata.Coma_state_t.needed_dirs_for_module k))
                       (Tidel.safe_set (dirned)) in
                    let new_dirs=Ordered.forget_order(ordered_dirs) in
                    Small_array.set wmdata.Coma_state_t.ancestors_for_module k new_ancestors;
                    Small_array.set wmdata.Coma_state_t.needed_libs_for_module k new_libs;
                    Small_array.set wmdata.Coma_state_t.needed_dirs_for_module k new_dirs;
              done;
              push_after_in_each wmdata last_father_idx new_dt;  
            )
          in
          wmdata;;


