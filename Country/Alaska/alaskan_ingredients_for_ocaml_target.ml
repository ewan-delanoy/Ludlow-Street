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


let targets_from_ancestor_data dt=
  let hm=Modulesystem_data.name dt in
  if Modulesystem_data.mll_registered dt
  then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else 
  if Modulesystem_data.mly_registered dt
  then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else
  if Modulesystem_data.ml_registered dt
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  

let targets_from_ancestors mdata dt=
     let ancestors=Modulesystem_data.all_ancestors dt in
     let temp1=Image.image (fun hm2->
            let opt2=Modify_md_list.find_module_registration mdata hm2 in
            let dt2=Option.unpack opt2 in
            targets_from_ancestor_data dt2
          ) ancestors in
     Preserve_initial_ordering.preserve_initial_ordering temp1;;



let optimized_targets_from_ancestor_data dt=
  let hm=Modulesystem_data.name dt in
  if Modulesystem_data.mll_registered dt
  then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else 
  if Modulesystem_data.mly_registered dt
  then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else
  if Modulesystem_data.ml_registered dt
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  

let optimized_targets_from_ancestors mdata dt=
     let ancestors=Modulesystem_data.all_ancestors dt in
     let temp1=Image.image (fun hm2->
            let opt2=Modify_md_list.find_module_registration mdata hm2 in
            let dt2=Option.unpack opt2 in
            optimized_targets_from_ancestor_data dt2
          ) ancestors in
     Preserve_initial_ordering.preserve_initial_ordering temp1;;

let immediate_ingredients_for_ml_from_mll hm=
  let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
  [mll_target];;

let immediate_ingredients_for_ml_from_mly hm=
  let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
  [mly_target];;

let immediate_ingredients_for_cmi dt hm=
    if Modulesystem_data.mll_registered dt
    then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm]
    else 
    if Modulesystem_data.mly_registered dt
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm]
    else
  if Modulesystem_data.mli_registered dt
  then let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target]
  else let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target];; 

let immediate_ingredients_for_cmo dt hm=
    if Modulesystem_data.mll_registered dt
    then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
    else 
    if Modulesystem_data.mly_registered dt
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
    else
  if Modulesystem_data.ml_registered dt
  then let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  


let immediate_ingredients_for_dcmo=immediate_ingredients_for_cmo;;

let immediate_ingredients_for_cma=immediate_ingredients_for_cmo;;

let immediate_ingredients_for_cmx dt hm=
    if Modulesystem_data.mll_registered dt
    then let mll_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
    else 
    if Modulesystem_data.mly_registered dt
    then let mly_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
    else
  if Modulesystem_data.ml_registered dt
  then let ml_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_ended_absolute_path.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  


let immediate_ingredients_for_executable hm=
 [Ocaml_target.cmx hm;Ocaml_target.executable hm];;  


let ingredients_for_nodep mlx=[];;

let ingredients_for_ml_from_mll mdata hm=
  let opt=Modify_md_list.find_module_registration mdata hm in
  if opt=None then raise(Unregistered_ml_from_mll(hm)) else 
  let dt=Option.unpack opt in
  (targets_from_ancestors mdata dt)@(immediate_ingredients_for_ml_from_mll hm);;

let ingredients_for_ml_from_mly mdata hm=
  let opt=Modify_md_list.find_module_registration mdata hm in
  if opt=None then raise(Unregistered_ml_from_mly(hm)) else 
  let dt=Option.unpack opt in
  (targets_from_ancestors mdata dt)@(immediate_ingredients_for_ml_from_mly hm);;

let ingredients_for_cmi mdata hm=
          let opt=Modify_md_list.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmi(hm)) else 
          let dt=Option.unpack opt in
          (targets_from_ancestors mdata dt)@(immediate_ingredients_for_cmi dt hm);;

let ingredients_for_cmo mdata hm=
          let opt=Modify_md_list.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmo(hm)) else 
          let dt=Option.unpack opt in
          (targets_from_ancestors mdata dt)@(immediate_ingredients_for_cmo dt hm);;

let ingredients_for_dcmo mdata hm=
          let opt=Modify_md_list.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_dcmo(hm)) else 
          let dt=Option.unpack opt in
          let ancestors=Modulesystem_data.all_ancestors dt in
          (Modify_md_list.debuggable_targets_from_ancestors mdata ancestors)@(immediate_ingredients_for_dcmo dt hm);;

let ingredients_for_cma mdata hm=
          let opt=Modify_md_list.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cma(hm)) else 
          let dt=Option.unpack opt in
          (targets_from_ancestors mdata dt)@(immediate_ingredients_for_cma dt hm);;

let ingredients_for_cmx mdata hm=
          let opt=Modify_md_list.find_module_registration mdata hm in
          if opt=None then raise(Unregistered_cmx(hm)) else 
          let dt=Option.unpack opt in
          (optimized_targets_from_ancestors mdata dt)@(immediate_ingredients_for_cmx dt hm);;
 
let ingredients_for_executable mdata hm=
  let opt=Modify_md_list.find_module_registration mdata hm in
  if opt=None then raise(Unregistered_executable(hm)) else 
  let dt=Option.unpack opt in
  (optimized_targets_from_ancestors mdata dt)
  @(immediate_ingredients_for_executable hm);; 


let ingredients_for_usual_element mdata hm=
   let opt=Modify_md_list.find_module_registration mdata hm in
  if opt=None then raise(Unregistered_module(hm)) else 
  let dt=Option.unpack opt in
  if (Modulesystem_data.mli_registered dt)&&(not(Modulesystem_data.ml_registered dt))
  then (ingredients_for_cmi mdata hm)@[Ocaml_target.cmi hm]
  else (ingredients_for_cmo mdata hm)@[Ocaml_target.cmo hm];;  
  
  
let ingredients_for_ocaml_target mdata=function
  Ocaml_target.NO_DEPENDENCIES(mlx)->[]
 |Ocaml_target.ML_FROM_MLL(hm)->ingredients_for_ml_from_mll mdata hm
 |Ocaml_target.ML_FROM_MLY(hm)->ingredients_for_ml_from_mly mdata hm
 |Ocaml_target.CMI(hm)->ingredients_for_cmi mdata hm
 |Ocaml_target.CMO(hm)->ingredients_for_cmo mdata hm
 |Ocaml_target.DCMO(hm)->ingredients_for_dcmo mdata hm
 |Ocaml_target.CMA(hm)->ingredients_for_cma mdata hm
 |Ocaml_target.CMX(hm)->ingredients_for_cmx mdata hm
 |Ocaml_target.EXECUTABLE(hm)->ingredients_for_executable mdata hm
 |Ocaml_target.DEBUGGABLE(hm)->Modify_md_list.ingredients_for_debuggable mdata hm;;      
 


let marked_ingredients_for_full_compilation mdata name l=
  let temp1=Image.image (ingredients_for_usual_element mdata) l in
  Preserve_initial_ordering.and_mark_endings temp1;;

let module_dependency_for_nodep mlx=false;;
let module_dependency_for_ml_from_mll mdata l_hm hm1=
       if List.mem hm1 l_hm
       then true
       else  
       let dt1=Option.unpack(Modify_md_list.find_module_registration mdata hm1) in
       let anc1=Modulesystem_data.all_ancestors dt1 in
       List.exists (fun z->List.mem z anc1 ) l_hm;;
let module_dependency_for_ml_from_mly=module_dependency_for_ml_from_mll;; 
let module_dependency_for_cmi=module_dependency_for_ml_from_mll;;
let module_dependency_for_cmo=module_dependency_for_ml_from_mll;;
let module_dependency_for_dcmo=module_dependency_for_ml_from_mll;;
let module_dependency_for_cma=module_dependency_for_ml_from_mll;;                 
let module_dependency_for_cmx=module_dependency_for_ml_from_mll;;  
let module_dependency_for_executable=module_dependency_for_ml_from_mll;;  
let module_dependency_for_debuggable=module_dependency_for_ml_from_mll;;  
let module_dependency_for_toplevel mdata l_hm name l_hm2=
  List.exists(fun hm2->
  (module_dependency_for_cmo mdata l_hm hm2)||(List.mem hm2 l_hm)
  ) l_hm2;;


let module_dependency_for_ocaml_target mdata l_hm =function
  Ocaml_target.NO_DEPENDENCIES(mlx)->false
 |Ocaml_target.ML_FROM_MLL(hm)->module_dependency_for_ml_from_mll mdata l_hm hm
 |Ocaml_target.ML_FROM_MLY(hm)->module_dependency_for_ml_from_mly mdata l_hm hm
 |Ocaml_target.CMI(hm)->module_dependency_for_cmi mdata l_hm hm
 |Ocaml_target.CMO(hm)->module_dependency_for_cmo mdata l_hm hm
 |Ocaml_target.DCMO(hm)->module_dependency_for_dcmo mdata l_hm hm
 |Ocaml_target.CMA(hm)->module_dependency_for_cma mdata l_hm hm
 |Ocaml_target.CMX(hm)->module_dependency_for_cmx mdata l_hm hm
 |Ocaml_target.EXECUTABLE(hm)->module_dependency_for_executable mdata l_hm hm
 |Ocaml_target.DEBUGGABLE(hm)->module_dependency_for_debuggable mdata l_hm hm;;

       



let mlx_dependency_for_ocaml_target mdata mlx tgt=
  let hm=Mlx_ended_absolute_path.half_dressed_core mlx in
  module_dependency_for_ocaml_target mdata [hm] tgt;;

let mlx_list_dependency_for_ocaml_target mdata l_mlx tgt=
 List.exists (fun mlx->mlx_dependency_for_ocaml_target mdata mlx tgt) l_mlx;;

