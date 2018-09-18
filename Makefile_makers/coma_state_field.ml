
(* 

#use"Makefile_makers/coma_state_field.ml";;

Here, we put soma wrappers so that all direct manipulations of
the Coma_state_t.t datatype should be done here.

*)

(* Converters *)
let of_t x=x;;
let to_t x=x;;
(*
in debug mode, change the above to
let of_t (Coma_state_t.CS x)=x;;
let to_t x=(Coma_state_t.CS x);;
*)
(* End of converters *)


let root cs=(of_t cs).Coma_state_t.root;;
let backup_dir cs=(of_t cs).Coma_state_t.dir_for_backup;;
   
let module_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.modules k ;;
let subdir_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.subdir_for_module k ;;
let principal_ending_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.principal_ending_for_module k ;;
let mli_presence_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.mli_presence_for_module k ;;
let principal_mt_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.principal_mt_for_module k ;;
let mli_mt_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.mli_mt_for_module k ;;
let needed_libs_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.needed_libs_for_module k ;;
let direct_fathers_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.direct_fathers_for_module k;;
let ancestors_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.ancestors_for_module k ;; 
let needed_dirs_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.needed_dirs_for_module k ;;
let product_up_to_date_at_idx cs k = Small_array.get (of_t cs).Coma_state_t.product_up_to_date_for_module k ;;
let directories cs=(of_t cs).Coma_state_t.directories;;
let targets cs=(of_t cs).Coma_state_t.targets;;
let preq_types cs=(of_t cs).Coma_state_t.printer_equipped_types;;


let modules cs= (of_t cs).Coma_state_t.modules;;
let subdirs cs= (of_t cs).Coma_state_t.subdir_for_module;;
let needed_dirs cs= (of_t cs).Coma_state_t.needed_dirs_for_module;;

(* Setters  *)

let set_module_at_idx cs k v = Small_array.set (of_t cs).Coma_state_t.modules k v;;
let set_subdir_at_idx cs k v = Small_array.set (of_t cs).Coma_state_t.subdir_for_module k v ;;
let set_principal_ending_at_idx cs k v = Small_array.set (of_t cs).Coma_state_t.principal_ending_for_module k v ;;
let set_mli_presence_at_idx cs k v = Small_array.set (of_t cs).Coma_state_t.mli_presence_for_module k v ;;
let set_principal_mt_at_idx cs k v = Small_array.set (of_t cs).Coma_state_t.principal_mt_for_module k v ;;
let set_mli_mt_at_idx cs k v = Small_array.set (of_t cs).Coma_state_t.mli_mt_for_module k v ;;
let set_needed_libs_at_idx cs k v = Small_array.set (of_t cs).Coma_state_t.needed_libs_for_module k v ;;
let set_direct_fathers_at_idx cs k v = Small_array.set (of_t cs).Coma_state_t.direct_fathers_for_module k v ;;
let set_ancestors_at_idx cs k v = Small_array.set (of_t cs).Coma_state_t.ancestors_for_module k v ;; 
let set_needed_dirs_at_idx cs k v = Small_array.set (of_t cs).Coma_state_t.needed_dirs_for_module k v ;;
let set_product_up_to_date_at_idx cs k = Small_array.set (of_t cs).Coma_state_t.product_up_to_date_for_module k ;;
let set_directories cs dirs=(of_t cs).Coma_state_t.directories<- dirs;;
let set_targets cs tgts=(of_t cs).Coma_state_t.targets<- tgts;;
let set_preq_types cs preqt=(of_t cs).Coma_state_t.printer_equipped_types<-preqt;;





let empty_one x y=to_t({
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
     product_up_to_date_for_module = Small_array.of_list [];
     directories =[];
     targets     =[];
     printer_equipped_types =[];
});;
  
let copy_mutables_from wrapped_cs_x wrapped_cs_y=
  let cs_x=of_t wrapped_cs_x 
  and cs_y=of_t wrapped_cs_y in

  (
     
     Small_array.copy_from cs_x.Coma_state_t.modules cs_y.Coma_state_t.modules;
     Small_array.copy_from cs_x.Coma_state_t.subdir_for_module cs_y.Coma_state_t.subdir_for_module;
     Small_array.copy_from cs_x.Coma_state_t.principal_ending_for_module cs_y.Coma_state_t.principal_ending_for_module;
     Small_array.copy_from cs_x.Coma_state_t.mli_presence_for_module cs_y.Coma_state_t.mli_presence_for_module;
     Small_array.copy_from cs_x.Coma_state_t.principal_mt_for_module cs_y.Coma_state_t.principal_mt_for_module;
     Small_array.copy_from cs_x.Coma_state_t.mli_mt_for_module cs_y.Coma_state_t.mli_mt_for_module;
     Small_array.copy_from cs_x.Coma_state_t.needed_libs_for_module cs_y.Coma_state_t.needed_libs_for_module;
     Small_array.copy_from cs_x.Coma_state_t.direct_fathers_for_module cs_y.Coma_state_t.direct_fathers_for_module;
     Small_array.copy_from cs_x.Coma_state_t.ancestors_for_module cs_y.Coma_state_t.ancestors_for_module;
     Small_array.copy_from cs_x.Coma_state_t.subdir_for_module cs_y.Coma_state_t.subdir_for_module;
     Small_array.copy_from cs_x.Coma_state_t.needed_dirs_for_module cs_y.Coma_state_t.needed_dirs_for_module;
     Small_array.copy_from cs_x.Coma_state_t.product_up_to_date_for_module cs_y.Coma_state_t.product_up_to_date_for_module;
     cs_x.directories <- cs_y.directories ;
     cs_x.targets  <- cs_y.targets ;
     cs_x.printer_equipped_types <- cs_y.printer_equipped_types ;
);;




let remove_in_each_at_index wrapped_cs idx=
    let cs=of_t wrapped_cs in
    (
      Small_array.remove_item_at_index cs.Coma_state_t.modules idx;
      Small_array.remove_item_at_index cs.Coma_state_t.subdir_for_module idx;
      Small_array.remove_item_at_index cs.Coma_state_t.principal_ending_for_module idx; 
      Small_array.remove_item_at_index cs.Coma_state_t.mli_presence_for_module idx; 
      Small_array.remove_item_at_index cs.Coma_state_t.principal_mt_for_module idx; 
      Small_array.remove_item_at_index cs.Coma_state_t.mli_mt_for_module idx; 
      Small_array.remove_item_at_index cs.Coma_state_t.needed_libs_for_module idx; 
      Small_array.remove_item_at_index cs.Coma_state_t.direct_fathers_for_module idx; 
      Small_array.remove_item_at_index cs.Coma_state_t.ancestors_for_module idx; 
      Small_array.remove_item_at_index cs.Coma_state_t.needed_dirs_for_module idx;
      Small_array.remove_item_at_index cs.Coma_state_t.product_up_to_date_for_module idx;
    );;
  
let push_right_in_each wrapped_cs (hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
  let nm=Half_dressed_module.naked_module hm
  and subdir=Half_dressed_module.subdirectory hm 
  and  cs=of_t wrapped_cs in
  (
    Small_array.push_right cs.Coma_state_t.modules nm;
    Small_array.push_right cs.Coma_state_t.subdir_for_module subdir;
    Small_array.push_right cs.Coma_state_t.principal_ending_for_module pr_end; 
    Small_array.push_right cs.Coma_state_t.mli_presence_for_module mlir; 
    Small_array.push_right cs.Coma_state_t.principal_mt_for_module prmt; 
    Small_array.push_right cs.Coma_state_t.mli_mt_for_module mlimt; 
    Small_array.push_right cs.Coma_state_t.needed_libs_for_module libned; 
    Small_array.push_right cs.Coma_state_t.direct_fathers_for_module dirfath; 
    Small_array.push_right cs.Coma_state_t.ancestors_for_module allanc; 
    Small_array.push_right cs.Coma_state_t.needed_dirs_for_module dirned;
    Small_array.push_right cs.Coma_state_t.product_up_to_date_for_module upy;
  );;
   
let set_in_each wrapped_cs idx 
   (hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Half_dressed_module.naked_module hm
    and subdir=Half_dressed_module.subdirectory hm 
    and  cs=of_t wrapped_cs in
    (
      Small_array.set cs.Coma_state_t.modules idx nm;
      Small_array.set cs.Coma_state_t.subdir_for_module idx subdir;
      Small_array.set cs.Coma_state_t.principal_ending_for_module idx pr_end; 
      Small_array.set cs.Coma_state_t.mli_presence_for_module idx mlir; 
      Small_array.set cs.Coma_state_t.principal_mt_for_module idx prmt; 
      Small_array.set cs.Coma_state_t.mli_mt_for_module idx mlimt; 
      Small_array.set cs.Coma_state_t.needed_libs_for_module idx libned; 
      Small_array.set cs.Coma_state_t.direct_fathers_for_module idx dirfath; 
      Small_array.set cs.Coma_state_t.ancestors_for_module idx allanc; 
      Small_array.set cs.Coma_state_t.needed_dirs_for_module idx dirned;
      Small_array.set cs.Coma_state_t.product_up_to_date_for_module idx upy;
    );;  
    
let push_after_in_each wrapped_cs idx 
    (hm,pr_end,mlir,prmt,mlimt,libned,dirfath,allanc,dirned,upy)=
    let nm=Half_dressed_module.naked_module hm
    and subdir=Half_dressed_module.subdirectory hm 
    and  cs=of_t wrapped_cs in
    (
      Small_array.push_immediately_after_idx cs.Coma_state_t.modules nm idx;
      Small_array.push_immediately_after_idx cs.Coma_state_t.subdir_for_module subdir idx ;
      Small_array.push_immediately_after_idx cs.Coma_state_t.principal_ending_for_module pr_end idx; 
      Small_array.push_immediately_after_idx cs.Coma_state_t.mli_presence_for_module mlir idx; 
      Small_array.push_immediately_after_idx cs.Coma_state_t.principal_mt_for_module prmt idx; 
      Small_array.push_immediately_after_idx cs.Coma_state_t.mli_mt_for_module mlimt idx; 
      Small_array.push_immediately_after_idx cs.Coma_state_t.needed_libs_for_module libned idx; 
      Small_array.push_immediately_after_idx cs.Coma_state_t.direct_fathers_for_module dirfath idx; 
      Small_array.push_immediately_after_idx cs.Coma_state_t.ancestors_for_module allanc idx; 
      Small_array.push_immediately_after_idx cs.Coma_state_t.needed_dirs_for_module dirned idx;
      Small_array.push_immediately_after_idx cs.Coma_state_t.product_up_to_date_for_module upy idx;
    );;      

let reposition_in_each wrapped_cs idx1 idx2=
    let rep=(fun x->
    Small_array.reposition_by_putting_snd_immediately_after_fst x idx1 idx2) 
    and  cs=of_t wrapped_cs in
     
    (
      rep cs.Coma_state_t.modules;
      rep cs.Coma_state_t.subdir_for_module;
      rep cs.Coma_state_t.principal_ending_for_module; 
      rep cs.Coma_state_t.mli_presence_for_module; 
      rep cs.Coma_state_t.principal_mt_for_module; 
      rep cs.Coma_state_t.mli_mt_for_module; 
      rep cs.Coma_state_t.needed_libs_for_module; 
      rep cs.Coma_state_t.direct_fathers_for_module; 
      rep cs.Coma_state_t.ancestors_for_module; 
      rep cs.Coma_state_t.needed_dirs_for_module;
      rep cs.Coma_state_t.product_up_to_date_for_module;
    );;    

let reorder wrapped_cs ordered_list_of_modules =
     let  cs=of_t wrapped_cs in
     let old_modules=Small_array.copy cs.Coma_state_t.modules
     and old_subdirs=Small_array.copy cs.Coma_state_t.subdir_for_module
     and old_pr_endings=Small_array.copy cs.Coma_state_t.principal_ending_for_module 
     and old_mli_presences=Small_array.copy cs.Coma_state_t.mli_presence_for_module 
     and old_pr_mts=Small_array.copy cs.Coma_state_t.principal_mt_for_module 
     and old_mli_mts=Small_array.copy cs.Coma_state_t.mli_mt_for_module 
     and old_libs=Small_array.copy cs.Coma_state_t.needed_libs_for_module 
     and old_fathers=Small_array.copy cs.Coma_state_t.direct_fathers_for_module 
     and old_ancestors=Small_array.copy cs.Coma_state_t.ancestors_for_module 
     and old_dirs=Small_array.copy cs.Coma_state_t.needed_dirs_for_module 
     and old_uppies=Small_array.copy cs.Coma_state_t.product_up_to_date_for_module in
     let arr=Array.of_list ordered_list_of_modules in
      ( for k=1 to Array.length arr do
        let current_module=Array.get arr (k-1) in
        let idx=Small_array.leftmost_index_of_in current_module old_modules in
        Small_array.set cs.Coma_state_t.modules k  (Small_array.get old_modules idx) ;
        Small_array.set cs.Coma_state_t.subdir_for_module k (Small_array.get old_subdirs idx)  ;
        Small_array.set cs.Coma_state_t.principal_ending_for_module k (Small_array.get old_pr_endings idx)  ; 
        Small_array.set cs.Coma_state_t.mli_presence_for_module k (Small_array.get old_mli_presences idx)  ; 
        Small_array.set cs.Coma_state_t.principal_mt_for_module k (Small_array.get old_pr_mts idx)  ; 
        Small_array.set cs.Coma_state_t.mli_mt_for_module k (Small_array.get old_mli_mts idx)  ; 
        Small_array.set cs.Coma_state_t.needed_libs_for_module k (Small_array.get old_libs idx)  ; 
        Small_array.set cs.Coma_state_t.direct_fathers_for_module k (Small_array.get old_fathers idx)  ; 
        Small_array.set cs.Coma_state_t.ancestors_for_module k (Small_array.get old_ancestors idx)  ; 
        Small_array.set cs.Coma_state_t.needed_dirs_for_module k (Small_array.get old_dirs idx)  ;
        Small_array.set cs.Coma_state_t.product_up_to_date_for_module k (Small_array.get old_uppies idx);
      done;
      );;    

let outer_separator=Industrial_separator.modulesystem_data1;; 
let inner_separator=Industrial_separator.modulesystem_data2;; 
      
let archive wrapped_cs=
        let cs=of_t wrapped_cs in
        let (Root_directory_t.R t1)=cs.Coma_state_t.root 
        and (Root_directory_t.R t2)=cs.Coma_state_t.dir_for_backup  
        and  t3=(modules wrapped_cs)
        and  t4=cs.Coma_state_t.subdir_for_module
        and  t5=cs.Coma_state_t.principal_ending_for_module 
        and  t6=cs.Coma_state_t.mli_presence_for_module 
        and  t7=cs.Coma_state_t.principal_mt_for_module 
        and  t8=cs.Coma_state_t.mli_mt_for_module 
        and  t9=cs.Coma_state_t.needed_libs_for_module 
        and t10=cs.Coma_state_t.direct_fathers_for_module 
        and t11=cs.Coma_state_t.ancestors_for_module 
        and t12=cs.Coma_state_t.needed_dirs_for_module
        and t13=cs.Coma_state_t.product_up_to_date_for_module
        and t14=cs.Coma_state_t.directories 
        and t15=cs.Coma_state_t.targets 
        and t16=cs.Coma_state_t.printer_equipped_types in
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
         Small_array.archive string_of_bool t13;
         list_arch (fun w->Nonblank.make(Subdirectory.without_trailing_slash w)) t14;
         list_arch Ocaml_target.archive t15;
         list_arch Half_dressed_module.archive_pair t16;
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
          to_t(
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
          product_up_to_date_for_module = Small_array.unarchive bool_of_string (part 13) ;
          directories = list_unarch (fun v->Subdirectory.of_string(Nonblank.decode v)) (part 14);
          targets = list_unarch Ocaml_target.unarchive (part 15) ; 
          printer_equipped_types = list_unarch Half_dressed_module.unarchive_pair (part 16);
      
       });; 
      
       
       
      
