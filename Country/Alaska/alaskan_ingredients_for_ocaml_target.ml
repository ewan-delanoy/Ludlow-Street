(*

It is assumed that no "manual tampering" is made,
e.g. manual rewriting of a ml coming from a mll, etc.


#use"Country/Alaska/alaskan_ingredients_for_ocaml_target.ml";;

*)



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
  let hm=Modify_md_list.hm_at_idx wmdata idx in
  if Modify_md_list.check_ending_in_at_idx Ocaml_ending.mll wmdata idx
  then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else 
  if Modify_md_list.check_ending_in_at_idx Ocaml_ending.mly wmdata idx
  then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else
  if Modify_md_list.check_ending_in_at_idx Ocaml_ending.ml wmdata idx
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  

let targets_from_ancestors wmdata idx=
     let ancestors=Modify_md_list.ancestors_at_idx wmdata idx in
     let temp1=Image.image (fun nm2->
            let idx2=Modify_md_list.find_module_index wmdata nm2 in
            targets_from_ancestor_data wmdata idx2
          ) ancestors in
     Preserve_initial_ordering.preserve_initial_ordering temp1;;

let optimized_targets_from_ancestor_data wmdata idx=
  let hm=Modify_md_list.hm_at_idx wmdata idx in
  if Modify_md_list.check_ending_in_at_idx Ocaml_ending.mll wmdata idx
  then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else 
  if Modify_md_list.check_ending_in_at_idx Ocaml_ending.mly wmdata idx
  then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else
  if Modify_md_list.check_ending_in_at_idx Ocaml_ending.ml wmdata idx
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  

let optimized_targets_from_ancestors wmdata idx=
     let ancestors=Modify_md_list.ancestors_at_idx wmdata idx in
     let temp1=Image.image (fun nm2->
          let idx2=Modify_md_list.find_module_index wmdata nm2 in
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
    if Modify_md_list.check_ending_in_at_idx Ocaml_ending.mll wmdata idx
    then let mll_target=Ocaml_target.no_dependencies
           (Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm]
    else 
    if Modify_md_list.check_ending_in_at_idx Ocaml_ending.mly wmdata idx
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm]
    else
  if Modify_md_list.check_ending_in_at_idx Ocaml_ending.mli wmdata idx
  then let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target]
  else let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target];; 

let immediate_ingredients_for_cmo wmdata idx hm=
    if Modify_md_list.check_ending_in_at_idx Ocaml_ending.mll wmdata idx
    then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
    else 
    if Modify_md_list.check_ending_in_at_idx Ocaml_ending.mly wmdata idx
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
    else
  if Modify_md_list.check_ending_in_at_idx Ocaml_ending.ml wmdata idx
  then let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  


let immediate_ingredients_for_dcmo=immediate_ingredients_for_cmo;;

let immediate_ingredients_for_cma=immediate_ingredients_for_cmo;;

let immediate_ingredients_for_cmx wmdata idx hm=
    if Modify_md_list.check_ending_in_at_idx Ocaml_ending.mll wmdata idx
    then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
    else 
    if Modify_md_list.check_ending_in_at_idx Ocaml_ending.mly wmdata idx
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
    else
  if Modify_md_list.check_ending_in_at_idx Ocaml_ending.ml wmdata idx
  then let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  


let immediate_ingredients_for_executable hm=
 [Ocaml_target.cmx hm;Ocaml_target.executable hm];;  


let ingredients_for_nodep mlx=[];;

let ingredients_for_ml_from_mll wmdata hm=
  let nm=Half_dressed_module.naked_module hm in
  let opt_idx=Modify_md_list.seek_module_index wmdata nm in
  if opt_idx=None then raise(Unregistered_ml_from_mll(hm)) else 
  let idx=Option.unpack opt_idx in
  (targets_from_ancestors wmdata idx)@(immediate_ingredients_for_ml_from_mll hm);;

let ingredients_for_ml_from_mly wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=Modify_md_list.seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_ml_from_mly(hm)) else 
    let idx=Option.unpack opt_idx in
    (targets_from_ancestors wmdata idx)@(immediate_ingredients_for_ml_from_mly hm);;


let ingredients_for_cmi wmdata hm=
  let nm=Half_dressed_module.naked_module hm in
  let opt_idx=Modify_md_list.seek_module_index wmdata nm in
  if opt_idx=None then raise(Unregistered_cmi(hm)) else 
  let idx=Option.unpack opt_idx in
  (targets_from_ancestors wmdata idx)@(immediate_ingredients_for_cmi wmdata idx hm);;

let ingredients_for_cmo wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=Modify_md_list.seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_cmo(hm)) else 
    let idx=Option.unpack opt_idx in
    (targets_from_ancestors wmdata idx)@
    (immediate_ingredients_for_cmo wmdata idx hm);;


let ingredients_for_dcmo wmdata hm=
  let nm=Half_dressed_module.naked_module hm in
  let opt_idx=Modify_md_list.seek_module_index wmdata nm in
  if opt_idx=None then raise(Unregistered_dcmo(hm)) else 
  let idx=Option.unpack opt_idx in
  let ancestors=Modify_md_list.ancestors_at_idx wmdata idx  in
  (Modify_md_list.debuggable_targets_from_ancestors wmdata ancestors)@
  (immediate_ingredients_for_dcmo wmdata idx hm);;

let ingredients_for_cma wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=Modify_md_list.seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_cma(hm)) else 
    let idx=Option.unpack opt_idx in
    (targets_from_ancestors wmdata idx)@
    (immediate_ingredients_for_cma wmdata idx hm);;

let ingredients_for_cmx wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=Modify_md_list.seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_cma(hm)) else 
    let idx=Option.unpack opt_idx in
    (optimized_targets_from_ancestors wmdata idx)@
    (immediate_ingredients_for_cmx wmdata idx hm);;    

let ingredients_for_executable wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=Modify_md_list.seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_executable(hm)) else 
    let idx=Option.unpack opt_idx in
    (optimized_targets_from_ancestors wmdata idx)@
    (immediate_ingredients_for_executable  hm);;   


let ingredients_for_usual_element wmdata hm=
  let nm=Half_dressed_module.naked_module hm in
  let opt_idx=Modify_md_list.seek_module_index wmdata nm in
  if opt_idx=None then raise(Unregistered_executable(hm)) else 
  let idx=Option.unpack opt_idx in
  let mli_reg=Modify_md_list.check_ending_in_at_idx Ocaml_ending.mli wmdata idx
  and ml_reg=Modify_md_list.check_ending_in_at_idx Ocaml_ending.mli wmdata idx in
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
 |Ocaml_target.DEBUGGABLE(hm)->Modify_md_list.ingredients_for_debuggable wmdata hm;;      
 


let marked_ingredients_for_full_compilation wmdata name l=
  let temp1=Image.image (ingredients_for_usual_element wmdata) l in
  Preserve_initial_ordering.and_mark_endings temp1;;

let module_dependency_for_nodep mlx=false;;
let module_dependency_for_ml_from_mll wmdata l_hm hm1=
       if List.mem hm1 l_hm
       then true
       else  
       let nm1=Half_dressed_module.naked_module hm1 in
       let idx1=Modify_md_list.find_module_index wmdata nm1 in
       let anc1=Modify_md_list.ancestors_at_idx wmdata idx1 in
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

