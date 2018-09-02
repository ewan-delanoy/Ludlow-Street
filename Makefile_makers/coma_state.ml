
(* 

#use"Makefile_makers/ml";;

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

module Ingredients_for_ocaml_target=struct

  exception Unregistered_cmo  of Half_dressed_module.t;;
  exception Unregistered_dcmo of Half_dressed_module.t;;
  exception Unregistered_cmi  of Half_dressed_module.t;;
  exception Unregistered_cma  of Half_dressed_module.t;;
  exception Unregistered_cmx  of Half_dressed_module.t;;
  exception Unregistered_ml_from_mll of Half_dressed_module.t;;
  exception Unregistered_ml_from_mly of Half_dressed_module.t;;
  exception Unregistered_executable of Half_dressed_module.t;;
  exception Unregistered_debuggable of Half_dressed_module.t;;
  exception Unregistered_module of (Half_dressed_module.t);;
  exception NonMarkedIngredientsForToplevel of string;;
  
  
  let targets_from_ancestor_data wmdata idx=
    let hm=hm_at_idx wmdata idx in
    if check_ending_in_at_idx Ocaml_ending.mll wmdata idx
    then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
    else 
    if check_ending_in_at_idx Ocaml_ending.mly wmdata idx
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
    else
    if check_ending_in_at_idx Ocaml_ending.ml wmdata idx
    then 
         let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
         [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
    else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
         [mli_target;Ocaml_target.cmi hm];;  
  
  let targets_from_ancestors wmdata idx=
       let ancestors=ancestors_at_idx wmdata idx in
       let temp1=Image.image (fun nm2->
              let idx2=find_module_index wmdata nm2 in
              targets_from_ancestor_data wmdata idx2
            ) ancestors in
       Preserve_initial_ordering.preserve_initial_ordering temp1;;
  
  let optimized_targets_from_ancestor_data wmdata idx=
    let hm=hm_at_idx wmdata idx in
    if check_ending_in_at_idx Ocaml_ending.mll wmdata idx
    then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
    else 
    if check_ending_in_at_idx Ocaml_ending.mly wmdata idx
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
    else
    if check_ending_in_at_idx Ocaml_ending.ml wmdata idx
    then 
         let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
         [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
    else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
         [mli_target;Ocaml_target.cmi hm];;  
  
  let optimized_targets_from_ancestors wmdata idx=
       let ancestors=ancestors_at_idx wmdata idx in
       let temp1=Image.image (fun nm2->
            let idx2=find_module_index wmdata nm2 in
            optimized_targets_from_ancestor_data wmdata idx2
            ) ancestors in
       Preserve_initial_ordering.preserve_initial_ordering temp1;;
  
  let immediate_ingredients_for_ml_from_mll hm=
    let mll_target=Ocaml_target.no_dependencies
       (Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
    [mll_target];;
  
  let immediate_ingredients_for_ml_from_mly hm=
    let mly_target=Ocaml_target.no_dependencies
      (Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
    [mly_target];;
  
  let immediate_ingredients_for_cmi wmdata idx hm=
      if check_ending_in_at_idx Ocaml_ending.mll wmdata idx
      then let mll_target=Ocaml_target.no_dependencies
             (Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
           [mll_target;Ocaml_target.ml_from_mll hm]
      else 
      if check_ending_in_at_idx Ocaml_ending.mly wmdata idx
      then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
           [mly_target;Ocaml_target.ml_from_mly hm]
      else
    if check_ending_in_at_idx Ocaml_ending.mli wmdata idx
    then let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
         [mli_target]
    else let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
         [ml_target];; 
  
  let immediate_ingredients_for_cmo wmdata idx hm=
      if check_ending_in_at_idx Ocaml_ending.mll wmdata idx
      then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
           [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
      else 
      if check_ending_in_at_idx Ocaml_ending.mly wmdata idx
      then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
           [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
      else
    if check_ending_in_at_idx Ocaml_ending.ml wmdata idx
    then let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
         [ml_target;Ocaml_target.cmi hm]
    else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
         [mli_target;Ocaml_target.cmi hm];;  
  
  
  let immediate_ingredients_for_dcmo=immediate_ingredients_for_cmo;;
  
  let immediate_ingredients_for_cma=immediate_ingredients_for_cmo;;
  
  let immediate_ingredients_for_cmx wmdata idx hm=
      if check_ending_in_at_idx Ocaml_ending.mll wmdata idx
      then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
           [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
      else 
      if check_ending_in_at_idx Ocaml_ending.mly wmdata idx
      then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
           [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
      else
    if check_ending_in_at_idx Ocaml_ending.ml wmdata idx
    then let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
         [ml_target;Ocaml_target.cmi hm]
    else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
         [mli_target;Ocaml_target.cmi hm];;  
  
  
  let immediate_ingredients_for_executable hm=
   [Ocaml_target.cmx hm;Ocaml_target.executable hm];;  
  
  
  let ingredients_for_nodep mlx=[];;
  
  let ingredients_for_ml_from_mll wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_ml_from_mll(hm)) else 
    let idx=Option.unpack opt_idx in
    (targets_from_ancestors wmdata idx)@(immediate_ingredients_for_ml_from_mll hm);;
  
  let ingredients_for_ml_from_mly wmdata hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index wmdata nm in
      if opt_idx=None then raise(Unregistered_ml_from_mly(hm)) else 
      let idx=Option.unpack opt_idx in
      (targets_from_ancestors wmdata idx)@(immediate_ingredients_for_ml_from_mly hm);;
  
  
  let ingredients_for_cmi wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_cmi(hm)) else 
    let idx=Option.unpack opt_idx in
    (targets_from_ancestors wmdata idx)@(immediate_ingredients_for_cmi wmdata idx hm);;
  
  let ingredients_for_cmo wmdata hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index wmdata nm in
      if opt_idx=None then raise(Unregistered_cmo(hm)) else 
      let idx=Option.unpack opt_idx in
      (targets_from_ancestors wmdata idx)@
      (immediate_ingredients_for_cmo wmdata idx hm);;
  
  
  let ingredients_for_dcmo wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_dcmo(hm)) else 
    let idx=Option.unpack opt_idx in
    let ancestors=ancestors_at_idx wmdata idx  in
    (debuggable_targets_from_ancestors wmdata ancestors)@
    (immediate_ingredients_for_dcmo wmdata idx hm);;
  
  let ingredients_for_cma wmdata hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index wmdata nm in
      if opt_idx=None then raise(Unregistered_cma(hm)) else 
      let idx=Option.unpack opt_idx in
      (targets_from_ancestors wmdata idx)@
      (immediate_ingredients_for_cma wmdata idx hm);;
  
  let ingredients_for_cmx wmdata hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index wmdata nm in
      if opt_idx=None then raise(Unregistered_cma(hm)) else 
      let idx=Option.unpack opt_idx in
      (optimized_targets_from_ancestors wmdata idx)@
      (immediate_ingredients_for_cmx wmdata idx hm);;    
  
  let ingredients_for_executable wmdata hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index wmdata nm in
      if opt_idx=None then raise(Unregistered_executable(hm)) else 
      let idx=Option.unpack opt_idx in
      (optimized_targets_from_ancestors wmdata idx)@
      (immediate_ingredients_for_executable  hm);;   
  
  
  let ingredients_for_usual_element wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_executable(hm)) else 
    let idx=Option.unpack opt_idx in
    let mli_reg=check_ending_in_at_idx Ocaml_ending.mli wmdata idx
    and ml_reg=check_ending_in_at_idx Ocaml_ending.mli wmdata idx in
    if mli_reg&&(not ml_reg)
    then (ingredients_for_cmi wmdata hm)@[Ocaml_target.cmi hm]
    else (ingredients_for_cmo wmdata hm)@[Ocaml_target.cmo hm];;  
    
    
  let ingredients_for_ocaml_target wmdata=function
    Ocaml_target.NO_DEPENDENCIES(mlx)->[]
   |Ocaml_target.ML_FROM_MLL(hm)->ingredients_for_ml_from_mll wmdata hm
   |Ocaml_target.ML_FROM_MLY(hm)->ingredients_for_ml_from_mly wmdata hm
   |Ocaml_target.CMI(hm)->ingredients_for_cmi wmdata hm
   |Ocaml_target.CMO(hm)->ingredients_for_cmo wmdata hm
   |Ocaml_target.DCMO(hm)->ingredients_for_dcmo wmdata hm
   |Ocaml_target.CMA(hm)->ingredients_for_cma wmdata hm
   |Ocaml_target.CMX(hm)->ingredients_for_cmx wmdata hm
   |Ocaml_target.EXECUTABLE(hm)->ingredients_for_executable wmdata hm
   |Ocaml_target.DEBUGGABLE(hm)->ingredients_for_debuggable wmdata hm;;      
   
  
  
  let marked_ingredients_for_full_compilation wmdata name l=
    let temp1=Image.image (ingredients_for_usual_element wmdata) l in
    Preserve_initial_ordering.and_mark_endings temp1;;
  
  let module_dependency_for_nodep mlx=false;;
  let module_dependency_for_ml_from_mll wmdata l_hm hm1=
         if List.mem hm1 l_hm
         then true
         else  
         let nm1=Half_dressed_module.naked_module hm1 in
         let idx1=find_module_index wmdata nm1 in
         let anc1=ancestors_at_idx wmdata idx1 in
         List.exists 
          (fun z->List.mem (Half_dressed_module.naked_module z) anc1 ) 
          l_hm;;
  
  
  let module_dependency_for_ml_from_mly=module_dependency_for_ml_from_mll;; 
  let module_dependency_for_cmi=module_dependency_for_ml_from_mll;;
  let module_dependency_for_cmo=module_dependency_for_ml_from_mll;;
  let module_dependency_for_dcmo=module_dependency_for_ml_from_mll;;
  let module_dependency_for_cma=module_dependency_for_ml_from_mll;;                 
  let module_dependency_for_cmx=module_dependency_for_ml_from_mll;;  
  let module_dependency_for_executable=module_dependency_for_ml_from_mll;;  
  let module_dependency_for_debuggable=module_dependency_for_ml_from_mll;;  
  let module_dependency_for_toplevel wmdata l_hm name l_hm2=
    List.exists(fun hm2->
    (module_dependency_for_cmo wmdata l_hm hm2)||(List.mem hm2 l_hm)
    ) l_hm2;;
  
  
  let module_dependency_for_ocaml_target wmdata l_hm =function
    Ocaml_target.NO_DEPENDENCIES(mlx)->false
   |Ocaml_target.ML_FROM_MLL(hm)->module_dependency_for_ml_from_mll wmdata l_hm hm
   |Ocaml_target.ML_FROM_MLY(hm)->module_dependency_for_ml_from_mly wmdata l_hm hm
   |Ocaml_target.CMI(hm)->module_dependency_for_cmi wmdata l_hm hm
   |Ocaml_target.CMO(hm)->module_dependency_for_cmo wmdata l_hm hm
   |Ocaml_target.DCMO(hm)->module_dependency_for_dcmo wmdata l_hm hm
   |Ocaml_target.CMA(hm)->module_dependency_for_cma wmdata l_hm hm
   |Ocaml_target.CMX(hm)->module_dependency_for_cmx wmdata l_hm hm
   |Ocaml_target.EXECUTABLE(hm)->module_dependency_for_executable wmdata l_hm hm
   |Ocaml_target.DEBUGGABLE(hm)->module_dependency_for_debuggable wmdata l_hm hm;;
  
         
  
  
  
  let mlx_dependency_for_ocaml_target wmdata mlx tgt=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
    module_dependency_for_ocaml_target wmdata [hm] tgt;;
  
  let mlx_list_dependency_for_ocaml_target wmdata l_mlx tgt=
   List.exists (fun mlx->mlx_dependency_for_ocaml_target wmdata mlx tgt) l_mlx;;
  
  

end;;  

module Command_for_ocaml_target=struct

  let ocamlc="ocamlc  -bin-annot ";;
  let ocamlopt="ocamlopt  -bin-annot ";;
  let cee=" -c ";;
  
  exception Command_called_on_nodep of Mlx_ended_absolute_path.t;;
  exception Unregistered_cmo  of Half_dressed_module.t;;
  exception Unregistered_dcmo of Half_dressed_module.t;;
  exception Unregistered_cmi  of Half_dressed_module.t;;
  exception Unregistered_cma  of Half_dressed_module.t;;
  exception Unregistered_cmx  of Half_dressed_module.t;;
  exception Unregistered_ml_from_mll of Half_dressed_module.t;;
  exception Unregistered_ml_from_mly of Half_dressed_module.t;;
  exception Unregistered_executable of Half_dressed_module.t;;
  exception Unregistered_debuggable of Half_dressed_module.t;;
  exception Unregistered_modules_in_toplevel of string*(Half_dressed_module.t list);;  
  
  let ingr=Ingredients_for_ocaml_target.ingredients_for_ocaml_target;;
  
  let cmx_manager=function
   Ocaml_target.CMX(hm2)->
      let s_hm2=Half_dressed_module.to_shortened_string hm2 in
      Some("_build/"^s_hm2^".cmx")
   |_->None;;
  
  let dcmo_manager=function
   Ocaml_target.DCMO(hm2)->
      let s_hm2=Half_dressed_module.to_shortened_string hm2 in
      Some("_build/"^s_hm2^".d.cmo")
   |_->None;;
  
  let command_for_nodep mlx=[];;
  
  let command_for_ml_from_mll dir hm=
            let s_hm=Half_dressed_module.uprooted_version hm in
            let s_root=Root_directory.connectable_to_subpath dir in
            let s_fhm=s_root^s_hm in
            [
              "ocamllex  -o "^s_fhm^".ml "^s_fhm^".mll";
            ];;
   
  let command_for_ml_from_mly dir hm=
            let s_hm=Half_dressed_module.uprooted_version hm in
            let s_root=Root_directory.connectable_to_subpath dir in
            let s_fhm=s_root^s_hm in
            [
              "ocamlyacc "^s_fhm^".mly"
            ];;  
  
  let command_for_cmi dir wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_cmi(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_root=Root_directory.connectable_to_subpath(dir) in
    let s_hm=Half_dressed_module.uprooted_version hm in
    let s_fhm=s_root^s_hm in
    let mli_reg=check_ending_in_at_idx Ocaml_ending.mli wmdata idx in
    let ending=(if mli_reg then ".mli" else ".ml") in
    let central_cmd=
        "ocamlc  -bin-annot "^
        (needed_dirs_and_libs_in_command false wmdata idx)^
            " -c "^s_fhm^ending in
            let full_mli=s_root^s_hm^".mli" in
            if (not mli_reg)
               &&(Sys.file_exists(full_mli))
            then (* 
                   in this situation the mli file exists but is not registered.
                   So the modulesystem manager must treat it as though it didn't
                   exist. We temporarily rename it so that ocamlc will ignore it.
                  *)
                  let dummy_mli=s_root^"uvueaoqhkt.mli" in
                  [
                   "mv "^full_mli^" "^dummy_mli;
                   central_cmd;
                   "mv "^s_fhm^".cm* "^s_root^"_build/";
                   "mv "^dummy_mli^" "^full_mli
                  ] 
            else  [
                     central_cmd;
                     "mv "^s_fhm^".cm* "^s_root^"_build/"
                   ];;
  
  let command_for_cmo dir wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_cmo(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Half_dressed_module.uprooted_version hm in
    let s_root=Root_directory.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let dirs_and_libs=needed_dirs_and_libs_in_command false wmdata idx in
    [ 
      "ocamlc -bin-annot "^dirs_and_libs^" -o "^s_fhm^".cmo -c "^s_fhm^".ml";
      "mv "^s_fhm^".cm* "^s_root^"_build/"
    ];;
  
  let command_for_dcmo dir wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_cmo(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Half_dressed_module.uprooted_version hm in
    let s_root=Root_directory.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let dirs_and_libs=needed_dirs_and_libs_in_command false wmdata idx in
    [ 
      "ocamlc -bin-annot -g "^dirs_and_libs^" -o "^s_fhm^".d.cmo -c "^s_fhm^".ml";
      "mv "^s_fhm^".d.cm* "^s_root^"_build/"
    ];;
  
  let command_for_cma dir wmdata hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index wmdata nm in
      if opt_idx=None then raise(Unregistered_cma(hm)) else 
      let idx=Option.unpack opt_idx in
      let s_hm=Half_dressed_module.uprooted_version hm in
      let s_root=Root_directory.connectable_to_subpath(dir) in
      let s_fhm=s_root^s_hm in
      let dirs_and_libs=needed_dirs_and_libs_in_command true wmdata idx in
      [ 
        "ocamlopt -bin-annot -a "^dirs_and_libs^" -o "^s_fhm^".cma -c "^s_fhm^".ml";
        "mv "^s_fhm^".cm* "^s_root^"_build/"
      ];;  
  
  let command_for_cmx dir wmdata hm=
      let nm=Half_dressed_module.naked_module hm in
      let opt_idx=seek_module_index wmdata nm in
      if opt_idx=None then raise(Unregistered_cmx(hm)) else 
      let idx=Option.unpack opt_idx in
      let s_hm=Half_dressed_module.uprooted_version hm in
      let s_root=Root_directory.connectable_to_subpath(dir) in
      let s_fhm=s_root^s_hm in
      let dirs_and_libs=needed_dirs_and_libs_in_command true wmdata idx in
      [ 
        "ocamlopt -bin-annot "^dirs_and_libs^" -o "^s_fhm^".cmx -c "^s_fhm^".ml";
        "mv "^s_fhm^".cm* "^s_root^"_build/"
      ];;      
            
  let command_for_executable dir wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_executable(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Half_dressed_module.uprooted_version hm in
    let s_root=Root_directory.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let temp1=ingr wmdata (Ocaml_target.EXECUTABLE(hm)) in
    let temp2=Option.filter_and_unpack cmx_manager temp1 in
    let long_temp2=Image.image (fun t->s_root^t) temp2 in
    let dirs_and_libs=needed_dirs_and_libs_in_command true wmdata idx  in
    [ 
      "ocamlopt -bin-annot "^dirs_and_libs^" -o "^s_fhm^".ocaml_executable "^
        (String.concat " " long_temp2);
      "mv "^s_fhm^".cm* "^s_root^"_build/";
      "mv "^s_fhm^".ocaml_executable "^s_root^"_build/"
    ];;
            
  let command_for_debuggable dir wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_debuggable(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Half_dressed_module.uprooted_version hm in
    let s_root=Root_directory.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let temp1=ingr wmdata (Ocaml_target.DEBUGGABLE(hm)) in
    let temp2=Option.filter_and_unpack dcmo_manager temp1 in
    let long_temp2=Image.image (fun t->s_root^t) temp2 in
    let dirs_and_libs=needed_dirs_and_libs_in_command false wmdata idx in
    [ 
      "ocamlc -bin-annot -g "^dirs_and_libs^" -o "^s_fhm^".ocaml_debuggable "^
        (String.concat " " long_temp2);
      "mv "^s_fhm^".ocaml_debuggable "^s_root^"_build/"
    ];;          
    
   
  let command_for_ocaml_target dir wmdata tgt=
     match tgt with
    Ocaml_target.NO_DEPENDENCIES(mlx)->command_for_nodep mlx 
   |Ocaml_target.ML_FROM_MLL(hm)->command_for_ml_from_mll dir hm
   |Ocaml_target.ML_FROM_MLY(hm)->command_for_ml_from_mly dir hm
   |Ocaml_target.CMI(hm)->command_for_cmi dir wmdata hm
   |Ocaml_target.CMO(hm)->command_for_cmo dir wmdata hm
   |Ocaml_target.DCMO(hm)->command_for_dcmo dir wmdata hm
   |Ocaml_target.CMA(hm)->command_for_cma dir wmdata hm
   |Ocaml_target.CMX(hm)->command_for_cmx dir wmdata hm
   |Ocaml_target.EXECUTABLE(hm)->command_for_executable dir wmdata hm
   |Ocaml_target.DEBUGGABLE(hm)->command_for_debuggable dir wmdata hm;;
     
  
   
  

end;;

module Ocaml_target_making=struct

  let cmd_for_tgt=Command_for_ocaml_target.command_for_ocaml_target;;

  let ingr_for_tgt =Ingredients_for_ocaml_target.ingredients_for_ocaml_target;;
  let ingr_for_top =Ingredients_for_ocaml_target.marked_ingredients_for_full_compilation;;
  
  
  let is_up_to_date dir tgts tgt=
    if Ocaml_target.is_a_debuggable tgt
    then false
    else 
    if Ocaml_target.test_target_existence dir tgt
    then List.mem tgt tgts
    else false;;
  
  let unit_make dir (bowl,(mdata,tgts,rejected_ones)) tgt=
    if (not bowl)
    then (bowl,(mdata,tgts,rejected_ones))
    else
    if is_up_to_date dir tgts tgt
    then (true,(mdata,tgts,rejected_ones))
    else 
    let temp1=Image.image Unix_command.uc (cmd_for_tgt dir mdata tgt) in 
    if List.for_all (fun i->i=0) temp1
    then let tgts2=(
          if Ocaml_target.is_a_debuggable tgt 
          then tgts
          else tgt::tgts
         )  in
          match Ocaml_target.ml_from_lex_or_yacc_data tgt with
         None->(true,(mdata,tgts2,rejected_ones))
         |Some(mlx)->
                     let mdata2=force_modification_time dir mdata mlx in
                     (true,(mdata2,tgts2,rejected_ones))        
    else let rejected_ones2=(
          match Ocaml_target.main_module tgt with
          None->rejected_ones
          |Some(hm)->hm::rejected_ones
          )  in
         (false,(mdata,tgts,rejected_ones2));;
  
  let make dir (mdata,tgts,rejected_ones) tgt=
    let l=ingr_for_tgt mdata tgt in
    List.fold_left (unit_make dir)  (true,(mdata,tgts,rejected_ones)) l;;
    
  exception Ending_for_full_compilation_pusher;;  
  
  let pusher_for_full_compilation dir (successful_ones,to_be_treated,ts)=
    match to_be_treated with
    []->raise(Ending_for_full_compilation_pusher)
    |(tgt,is_an_ending_or_not)::others->
    let (bowl2,ts2)=unit_make dir (true,ts) tgt in
    if bowl2
    then let new_successful_ones=(
           if is_an_ending_or_not=Is_an_ending_or_not.Yes
           then let hm=Option.unpack(Ocaml_target.main_module tgt) in
                (*
                  Note that the cmi and cmo give the same hm
                *)
                if List.mem hm successful_ones
                then successful_ones
                else hm::successful_ones
           else successful_ones
         ) in
         (new_successful_ones,others,ts2)
    else let hm=Option.unpack(Ocaml_target.main_module tgt) in
         let root=Half_dressed_module.bundle_main_dir hm in
         let s_root=Root_directory.connectable_to_subpath root in
         let (mdata,_,_)=ts 
         and (mdata2,tgts2,rejected_ones2)=ts2 in
         let (rejects,remains)=List.partition
         (fun (tgtt,_)->
           Ingredients_for_ocaml_target.module_dependency_for_ocaml_target 
           mdata [hm] tgtt
         ) others in
         let _=Image.image (
           fun (tgtt,_)->
           if Ocaml_target.has_dependencies tgtt
           then let s_ap=s_root^"_build/"^
                    (Ocaml_target.to_shortened_string tgtt) in
                let _=Unix_command.uc("rm -f "^s_ap) in
                ()
         ) ((tgt,is_an_ending_or_not)::rejects) in
         let newly_rejected_ones=Option.filter_and_unpack 
         (fun (tgt,_)->Ocaml_target.main_module tgt) rejects in
         let rejected_ones2=Listennou.nonredundant_version(
           List.rev_append newly_rejected_ones rejected_ones2
         ) in
         (successful_ones,remains,(mdata2,tgts2,rejected_ones2));; 
  
  let rec  iterator_for_full_compilation dir (successful_ones,to_be_treated,ts)=
    match to_be_treated with
    []->(List.rev successful_ones,ts)
    |_->iterator_for_full_compilation dir (pusher_for_full_compilation dir (successful_ones,to_be_treated,ts));;
  
    
  
   
  let feydeau dir old_mdata old_tgts=
    let l=everyone_except_the_debugger old_mdata in
    let ts=(old_mdata,old_tgts,[]) in
    let temp1=ingr_for_top old_mdata "ecaml" l in
    let (successful_ones,ts2)=iterator_for_full_compilation dir ([],temp1,ts) in
    ts2;;
  
  
end;;  


let recompile x=
     let old_tgts=x.Coma_state_t.targets in
     let ((_,nms_to_be_updated),short_paths)=
        recompile_on_monitored_modules false x in
     if nms_to_be_updated=[] then (false,[]) else
     let new_dirs=compute_subdirectories_list x 
     and new_tgts1=Ocaml_target.still_up_to_date_targets nms_to_be_updated old_tgts in
     let checker=Ocaml_target.test_target_existence (root x) in
     let new_tgts=List.filter checker new_tgts1 in
     let (_,new_tgts2,rejected_ones2)=
       Ocaml_target_making.feydeau
       (root x) x new_tgts  in
      let new_preqt=Image.image(
        fun (hm,_)->(hm,not(List.mem hm rejected_ones2))
      )  (x.Coma_state_t.printer_equipped_types) in   
     let _=(
        x.Coma_state_t.directories <- new_dirs;
        x.Coma_state_t.targets <- new_tgts2;
        x.Coma_state_t.printer_equipped_types <- new_preqt;
     )  in
    (true,short_paths);;       

let add_printer_equipped_type x hm=
  set_preq_types x ((preq_types x)@[hm]);;

let remove_printer_equipped_type x hm=
  set_preq_types x (List.filter (fun hm2->hm2<>hm) (preq_types x));;

let uple_form x=
  (x,
   x.Coma_state_t.directories,
   x.Coma_state_t.targets,
   x.Coma_state_t.printer_equipped_types
   );;

let remove_debuggables x=
  let new_tgts=List.filter Ocaml_target.is_not_a_debuggable
         (targets x)  in 
         set_targets x new_tgts;;
    
let backup x diff opt=
  Alaskan_backup_target_system.backup 
  (root x,backup_dir x) diff opt;;

let unregister_mlx_file_on_targets root_dir (old_mdata,old_tgts) mlx=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
    let new_mdata=unregister_mlx_file_on_monitored_modules old_mdata mlx in
    let new_dirs=compute_subdirectories_list new_mdata
    and new_tgts=List.filter (fun tgt->
       match Ocaml_target.main_module tgt with
       None->false |Some(hm2)->hm2<>hm
    ) old_tgts in
    let (new_mdata2,new_tgts2,_)=Ocaml_target_making.feydeau
    root_dir new_mdata new_tgts  in
    (new_mdata2,new_dirs,new_tgts2);;   

exception FileWithDependencies of 
Mlx_ended_absolute_path.t*(Naked_module_t.t list);;


let forget_file_on_targets root_dir triple ap=
  let (wmdata,dirs,tgts)=triple in
  let hm=Half_dressed_module.of_path_and_root ap root_dir 
  and mlx=Mlx_ended_absolute_path.of_path_and_root ap root_dir  in
  let nm=Half_dressed_module.naked_module hm in
  match seek_module_index  wmdata nm with
   None->triple
  |Some(_)->
   let bel=below wmdata (Mlx_ended_absolute_path.half_dressed_core mlx) in
    if bel=[]
    then let s_hm=Half_dressed_module.uprooted_version hm in
         let fn=(Root_directory.connectable_to_subpath(root_dir))^s_hm in
         let _=Image.image
         (fun edg->Unix_command.uc("rm -f "^fn^edg^"*"))
         [".cm";".d.cm";".caml_debuggable"] in
         unregister_mlx_file_on_targets root_dir 
            (wmdata,tgts) mlx
    else raise(FileWithDependencies(mlx,bel));;



let forget_file x ap=
    let (_,new_dirs,new_tgts)= 
     forget_file_on_targets (root x)
       (x,directories x,targets x) ap in  
        (
          set_directories x new_dirs;
          set_targets x new_tgts;
        );;         

module Unregister_module=struct

let on_targets root_dir (old_mdata,old_tgts) hm=
    let (new_mdata,short_paths)=unregister_module_on_monitored_modules  old_mdata hm in
    let new_dirs=compute_subdirectories_list new_mdata 
    and new_tgts=List.filter (fun tgt->
      match Ocaml_target.main_module tgt with
      None->false |Some(hm2)->hm2<>hm
    ) old_tgts in
    let (new_mdata2,new_tgts2,_)=Ocaml_target_making.feydeau
    root_dir new_mdata new_tgts  in
     ((new_mdata2,new_dirs,new_tgts2),short_paths);;   
     
   

end;;          
   
exception ModuleWithDependenciesDuringForgetting of 
        Half_dressed_module.t*(Naked_module_t.t list);;
exception Non_registered_module_during_forgetting of Naked_module_t.t;;
      
let forget_module_on_targets root_dir (wmdata,dirs,tgts) hm=
        let nm=Half_dressed_module.naked_module hm in
        match seek_module_index  wmdata nm with
         None->raise(Non_registered_module_during_forgetting(nm))
        |Some(dt)->
         let bel=below wmdata hm in
          if bel=[]
          then let (answer,short_paths)=Unregister_module.on_targets root_dir 
                          (wmdata,tgts) hm in
               let sfn=Half_dressed_module.to_shortened_string hm in
               let _=Image.image
               (fun edg->
                let cmd="rm -f _build/"^sfn^edg in
                Unix_command.uc(cmd))
               [".cm*";".d.cm*";".caml_debuggable"] in
               let temp1=Image.image (fun t->
                  Absolute_path.of_string(Root_directory.join root_dir t)
               ) short_paths in
               let _=Image.image 
               (German_forget_unregistered_file.forget root_dir) temp1 in
               (answer,short_paths)
          else raise(ModuleWithDependenciesDuringForgetting(hm,bel));;
      

let forget_module x hm=
    let ((_,new_dirs,new_tgts),short_paths)= 
      forget_module_on_targets (root x)
      (x,directories x,targets x) hm in
      let _=(
          set_directories x new_dirs;
          set_targets x new_tgts;
      ) in
      short_paths;;          

let initialize x=
        let s_ap=Root_directory.join (root x)  Coma_constant.name_for_targetfile in
        let ap=Absolute_path.of_string s_ap in
        let the_archive=Io.read_whole_file ap in
        let the_bulky_one=unarchive the_archive in
        copy_mutables_from x the_bulky_one;;      

module Try_to_register=struct

  let mlx_file mdata mlx_file=
    try(Some(register_mlx_file_on_monitored_modules 
        mdata mlx_file)) with _->None;;  

module Private=struct

exception Pusher_exn;;

let pusher  (vdata,failures,yet_untreated)=
     match yet_untreated with
      []->raise(Pusher_exn)
      |mlx::others->
      (
        match mlx_file vdata mlx with
        None->(vdata,mlx::failures,others)
        |Some(nfs)->(nfs,failures,others)
      );; 

let rec iterator x=
   let (vdata,failures,yet_untreated)=x in
   match yet_untreated with
      []->(failures,vdata)
      |mlx::others->
      (
        match mlx_file vdata mlx with
        None->iterator(pusher x)
        |Some(nfs)->iterator(pusher x)
      );;   

end;;

let mlx_files mdata mlx_files=
   Private.iterator(mdata,[],mlx_files);;
 
  
   


end;;  


module Target_system_creation=struct

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
       Try_to_register.mlx_files (empty_one dir backup_dir) temp1;;
    
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
        let pre_preqt=printer_equipped_types_from_data mdata1 in
        let (mdata2,new_tgts2,rejected_ones2)=Ocaml_target_making.feydeau
           dir mdata1 []  in
       let preqt=Image.image (fun hm->(hm,not(List.mem hm rejected_ones2))) pre_preqt in 
       (mdata2,new_tgts2,preqt);;
    
    

end;;  

let delchacre_from_scratch (source_dir,dir_for_backup) mdata=
  let temp1=all_mlx_paths mdata in
  let temp3=temp1 in
  let temp4=Image.image (fun ap->Root_directory.cut_beginning 
   source_dir (Absolute_path.to_string ap)) temp3 in
 Prepare_dircopy_update.compute_diff
    (source_dir,temp4) dir_for_backup;;





let refresh x=
      let (new_mdata,new_tgts,new_ptypes)=
        Target_system_creation.from_main_directory 
             (root x)
             (backup_dir x)
         in 
        let new_dirs=compute_subdirectories_list new_mdata in
        let new_diff=delchacre_from_scratch (root x,backup_dir x) new_mdata in
        let _=
        (
          copy_mutables_from x new_mdata;
          set_directories x new_dirs;
          set_targets x new_tgts;
          set_preq_types x new_ptypes;
         ) in
         new_diff;; 

module Register_mlx_file=struct

let on_targets (old_mdata,old_dirs,old_tgts) mlx=
    let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
    let new_dir=Half_dressed_module.subdirectory hm in
   let new_mdata=register_mlx_file_on_monitored_modules old_mdata mlx in
   let new_dirs=
   (if List.mem new_dir old_dirs then old_dirs else old_dirs@[new_dir] )
   and new_tgts=
   (*
         The only outdated targets are the targets 
         corresponding to an identical module
         (for example when a mll or mly is added to
         an already registered ml) 
          *)
         List.filter (
          fun tgt->match Ocaml_target.main_module tgt with
                   None->true
                   |Some(hm2)->hm2<>hm
         ) old_tgts
    in
    (new_mdata,new_dirs,new_tgts);; 
   



end;;  


let register_mlx_file x mlx=
          let (new_mdata,new_dirs,new_tgts)= 
          Register_mlx_file.on_targets 
           (x,directories x,targets x) mlx in
         let (_,new_tgts2,_)=Ocaml_target_making.feydeau
               (root x) new_mdata new_tgts  in     
                
             (
              set_directories x new_dirs;
              set_targets x new_tgts2;   
              ) ;;             

let relocate_module_on_targets root_dir (old_mdata,old_tgts) old_name new_subdir= 
  let untouched_tgts=List.filter
   (fun tgt->not(Ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_mdata [old_name] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_name)) ) old_tgts in
  let (new_mdata,(old_files,new_files))=
    relocate_module_on_monitored_modules root_dir old_mdata old_name new_subdir in
  let (new_mdata2,new_tgts2,_)=Ocaml_target_making.feydeau
    root_dir new_mdata untouched_tgts  in
  ((new_mdata2,new_tgts2),(old_files,new_files));;   
 



let relocate_module x old_name new_subdir=
  let ((_,new_tgts),(old_files,new_files))=
    relocate_module_on_targets (root x)
       (x,targets x) 
       old_name new_subdir in
     (
      set_targets x new_tgts; 
     );;    

let rename_directory x (old_subdir,new_subdirname)=
      let _=Rename_endsubdirectory.in_unix_world 
       (root x) (old_subdir,new_subdirname) in
      let pair=(old_subdir,new_subdirname) in
      let _=rename_directory_on_data pair x
         
      and new_dirs=German_rename_directory.on_subdirectories pair 
        (directories x)
      and new_tgts=German_rename_directory.on_up_to_date_targets pair 
        (targets x)
      and new_peqt=German_rename_directory.on_printer_equipped_types pair 
        (preq_types x)
      in
         (
          
          set_directories x new_dirs;
          set_targets x new_tgts;
          set_preq_types x new_peqt;  
         );;   
      
let rename_module_on_targets root_dir (old_mdata,old_tgts) old_name new_name= 
  let untouched_tgts=List.filter
   (fun tgt->not(Ingredients_for_ocaml_target.module_dependency_for_ocaml_target
   old_mdata [old_name] tgt)&&(Ocaml_target.main_module(tgt)<>Some(old_name)) ) old_tgts in
  let (new_mdata,(old_files,new_files))=
     rename_module_on_monitored_modules root_dir old_mdata old_name new_name in
  let (new_mdata2,new_tgts2,_)=Ocaml_target_making.feydeau
 root_dir new_mdata untouched_tgts  in
  ((new_mdata2,new_tgts2),(old_files,new_files));;   
 

let rename_module x old_name new_name=
      let ((_,new_tgts),(old_files,new_files))=
        rename_module_on_targets (root x)
          (x,targets x) old_name new_name in  
         (
          set_targets x new_tgts;
         );;    

let pre_start_debugging root_dir (mdata,tgts)=
    let _=Alaskan_remove_debuggables.rd root_dir mdata in
    let dbg=Coma_constant.name_for_debugged_module in
	let rdir=compute_subdirectories_list mdata in
	let ap=Find_suitable_ending.find_file_location root_dir rdir 
	     (dbg^".ml") in
	let hm=Half_dressed_module.of_path_and_root ap root_dir in
	let mdata2=recompute_module_info mdata hm in
	let tgt=Ocaml_target.debuggable hm in
	let answer=Ocaml_target_making.make root_dir
	(mdata2,tgts,[]) tgt in
	let msg=(
	  if (fst answer)
	  then "\n\n Now, start \n\nocamldebug _build/"^dbg^".ocaml_debuggable\n\nin another terminal\n\n"
	  else "\n\n Something went wrong, see above. \n\n"
	) in
	let _=(
	  print_string msg;
	  flush stdout
	) in
	answer;;   
   

let start_debugging x=
          let (bowl,(new_mdata,new_tgts,_))=
            pre_start_debugging (root x)
            (x,targets x)  in
          if bowl
          then (
            set_targets x new_tgts 
               )
          else ();;

          


let unregister_mlx_file x mlx=
        let (_,new_dirs,new_tgts)= 
          unregister_mlx_file_on_targets (root x)
          (x,targets x) mlx in
          (
              set_directories x new_dirs;
              set_targets x new_tgts;
          ) ;;  




let unregister_module x hm=
        let ((_,new_dirs,new_tgts),short_paths)= 
         Unregister_module.on_targets (root x)
           (x,targets x) hm in
            (
              set_directories x new_dirs;
              set_targets x new_tgts;
            );;        


module Write_makefile=struct
  
  let slice_targets tgts=
    let temp1=Sliced_string.make_aggregates_if_possible 
                     (Separator.of_string " ") 
                     (Image.image Ocaml_target.to_string tgts) in
    Sliced_string.to_string_list temp1;;
  
  let slice_shortened_targets tgts=
    let temp1=Sliced_string.make_aggregates_if_possible 
                     (Separator.of_string " ") 
                     (Image.image Ocaml_target.to_shortened_string tgts) in
    Sliced_string.to_string_list temp1;;
  
  let write_usual_makefile_element main_root mdata tgt=
   let ingrs=Ingredients_for_ocaml_target.ingredients_for_ocaml_target 
      mdata tgt in
   let sliced_ingrs=slice_shortened_targets ingrs in
   let cmds=Command_for_ocaml_target.command_for_ocaml_target 
                         main_root mdata tgt in
   let s1=(Ocaml_target.to_shortened_string tgt)^" : " 
   and s2=String.concat " \\\n" sliced_ingrs
   and s3="\n\t"
   and s4=String.concat "\n\t" cmds in
   String.concat "" [s1;s2;s3;s4];;
   
  let write_full_compilation_makefile_element  wmdata=
    let main_root=root wmdata in
    let l=all_modules wmdata in
    let temp1=Image.image 
    (Ingredients_for_ocaml_target.ingredients_for_usual_element wmdata) l in
    let ingrs=Preserve_initial_ordering.preserve_initial_ordering temp1 in
    let sliced_ingrs=slice_shortened_targets ingrs in
    let l_idx=Image.image (fun hm->
      let nm=Half_dressed_module.naked_module hm in
      find_module_index wmdata nm) l  in
    let s_root=Root_directory.connectable_to_subpath(main_root) in
    let long_temp4=Image.image (fun idx->
               let hm=hm_at_idx wmdata idx in
               let s_hm=(Half_dressed_module.uprooted_version hm) in
               let short_s_hm=Father_and_son.son s_hm '/' in
               let ml_reg=check_ending_in_at_idx Ocaml_ending.ml wmdata idx in
               if ml_reg
               then s_root^"_build/"^short_s_hm^".cmo"
               else " "
    ) l_idx in   
    let long_s_lhm=String.concat " " long_temp4 in
    let dirs_and_libs=needed_dirs_and_libs_for_several false wmdata l_idx in
    let cmds=[ "ocamlmktop "^dirs_and_libs^" -o "^s_root^"ecaml "^long_s_lhm^" ";
            "mv "^s_root^"ecaml "^s_root^"_build/"] in
    let s1="ecaml : " 
    and s2=String.concat " \\\n" sliced_ingrs
    and s3="\n\t"
    and s4=String.concat "\n\t" cmds in
    String.concat "" [s1;s2;s3;s4];; 
   
  let write_makefile wmdata=
   let main_root=root wmdata in 
   let temp1=usual_targets wmdata in
   let temp2=Image.image (write_usual_makefile_element main_root wmdata) temp1 in
   let temp3=temp2@[write_full_compilation_makefile_element wmdata] in
   let temp5=slice_targets  temp1 in
   let temp6=String.concat " \\\n" temp5 in
   let temp7="clean :\n\trm -r -f "^temp6^"\n\n" in
   String.concat "\n\n" (temp3@[temp7]);;
  
     

end;;


module Save_all=struct

  module Private=struct

    let save_makefile (root,location_for_makefile) mdata=
      let s1="# This makefile was automatocally written by\n"^
      "# the write_makefile function in the ml_manager module. \n\n"^
      (Write_makefile.write_makefile mdata) in
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
    
    
  
    let save_targetfile (root,location_for_targetfile) mdata=
      let s1=archive mdata in
      let lt=Root_directory.force_join root location_for_targetfile in
      Io.overwrite_with (Absolute_path.of_string lt) s1;;
    
    end;;
    
    
    
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
        Private.save_targetfile (root,location_for_targetfile) mdata;
        Private.save_printersfile (root,location_for_printersfile) printer_equipped_types;
       );;
    
    
  

end;;  


module Create_or_update_copied_compiler=struct

  let prepare mdata (sourcedir,destdir)=
    let l1=all_short_paths mdata in
    let main_diff=Prepare_dircopy_update.compute_diff 
          (sourcedir,l1) destdir in
    Prepare_dircopy_update.commands_for_update (sourcedir,destdir) main_diff;;
  
  let file_for_backup="Country/Alaska/alaskan_backup_target_system.ml";;
  
  let replacement_for_special_file (sourcedir,destdir) filename=
    if filename=file_for_backup
    then ("let github_after_backup=ref(true)"^Double_semicolon.ds,
          "let github_after_backup=ref(false)"^Double_semicolon.ds)
    else (Root_directory.without_trailing_slash sourcedir,
          Root_directory.without_trailing_slash destdir);;
  
  let prepare_special_file (sourcedir,destdir) filename=
    let the_file=Absolute_path.create_file(Root_directory.join destdir filename) in
    Replace_inside.replace_inside_file
    (replacement_for_special_file (sourcedir,destdir) filename)
    the_file;;
  
  let init_dir=
      Subdirectory.connectable_to_subpath 
      (Coma_constant.kept_up_to_date_but_not_registered);;
  
  let up_to_date_but_not_registered_files=
     [
        Coma_constant.path_for_loadingsfile;
        Coma_constant.path_for_printersfile;
     ];;
  
  let ucc mdata (sourcedir,destdir)=
    let knr=Subdirectory.without_trailing_slash(Coma_constant.kept_up_to_date_but_not_registered) in
    let s_dir=Root_directory.connectable_to_subpath destdir in 
    let _=Unix_command.uc ("mkdir -p "^s_dir^"_build") in
    let _=Unix_command.uc ("mkdir -p "^s_dir^knr) in
    let _=Image.image (
        fun s->Unix_command.uc("touch "^s_dir^s)
    ) up_to_date_but_not_registered_files in
    let _=Image.image Unix_command.uc (prepare mdata (sourcedir,destdir)) in
    let _=Image.image (prepare_special_file (sourcedir,destdir))
      (
        up_to_date_but_not_registered_files@
      ["Country/Germany/german_wrapper.ml";file_for_backup]
      ) 
     in 
    Target_system_creation.from_main_directory destdir;;
         
         
  
  
  
  
  
  
     
     
      

end;;  
