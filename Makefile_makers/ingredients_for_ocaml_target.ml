(*

It is assumed that no "manual tampering" is made,
e.g. manual rewriting of a ml coming from a mll, etc.


#use"Makefile_makers/ingredients_for_ocaml_target.ml";;

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
exception Unregistered_module_in_toplevel of string*(Half_dressed_module.t);;
exception NonMarkedIngredientsForToplevel of string;;


let targets_from_ancestor_data dt=
  let hm=Modulesystem_data.name dt in
  if Modulesystem_data.mll_present dt
  then let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else 
  if Modulesystem_data.mly_present dt
  then let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else
  if Modulesystem_data.ml_present dt
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmo hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  


let debuggable_targets_from_ancestor_data dt=
  let hm=Modulesystem_data.name dt in
  if Modulesystem_data.mll_present dt
  then let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
  else 
  if Modulesystem_data.mly_present dt
  then let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
  else
  if Modulesystem_data.ml_present dt
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.dcmo hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  

let targets_from_ancestors fs dt=
     let ancestors=Modulesystem_data.all_ancestors dt in
     let temp1=Image.image (fun hm2->
            let opt2=Modulesystem.find_module_registration fs hm2 in
            let dt2=Option.unpack opt2 in
            targets_from_ancestor_data dt2
          ) ancestors in
     Preserve_initial_ordering.preserve_initial_ordering temp1;;

let debuggable_targets_from_ancestors fs ancestors=
     let temp1=Image.image (fun hm2->
            let opt2=Modulesystem.find_module_registration fs hm2 in
            let dt2=Option.unpack opt2 in
            debuggable_targets_from_ancestor_data dt2
          ) ancestors in
     Preserve_initial_ordering.preserve_initial_ordering temp1;;

let optimized_targets_from_ancestor_data dt=
  let hm=Modulesystem_data.name dt in
  if Modulesystem_data.mll_present dt
  then let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
       [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else 
  if Modulesystem_data.mly_present dt
  then let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
       [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else
  if Modulesystem_data.ml_present dt
  then 
       let ml_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm;Ocaml_target.cmx hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  

let optimized_targets_from_ancestors fs dt=
     let ancestors=Modulesystem_data.all_ancestors dt in
     let temp1=Image.image (fun hm2->
            let opt2=Modulesystem.find_module_registration fs hm2 in
            let dt2=Option.unpack opt2 in
            optimized_targets_from_ancestor_data dt2
          ) ancestors in
     Preserve_initial_ordering.preserve_initial_ordering temp1;;

let immediate_ingredients_for_ml_from_mll hm=
  let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
  [mll_target];;

let immediate_ingredients_for_ml_from_mly hm=
  let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
  [mly_target];;

let immediate_ingredients_for_cmi dt hm=
    if Modulesystem_data.mll_present dt
    then let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm]
    else 
    if Modulesystem_data.mly_present dt
    then let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm]
    else
  if Modulesystem_data.mli_present dt
  then let mli_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mli) in
       [mli_target]
  else let ml_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.ml) in
       [ml_target];; 

let immediate_ingredients_for_cmo dt hm=
    if Modulesystem_data.mll_present dt
    then let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
    else 
    if Modulesystem_data.mly_present dt
    then let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
    else
  if Modulesystem_data.ml_present dt
  then let ml_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  


let immediate_ingredients_for_dcmo=immediate_ingredients_for_cmo;;

let immediate_ingredients_for_cma=immediate_ingredients_for_cmo;;

let immediate_ingredients_for_cmx dt hm=
    if Modulesystem_data.mll_present dt
    then let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
         [mll_target;Ocaml_target.ml_from_mll hm;Ocaml_target.cmi hm]
    else 
    if Modulesystem_data.mly_present dt
    then let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
         [mly_target;Ocaml_target.ml_from_mly hm;Ocaml_target.cmi hm]
    else
  if Modulesystem_data.ml_present dt
  then let ml_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.ml) in
       [ml_target;Ocaml_target.cmi hm]
  else let mli_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mli) in
       [mli_target;Ocaml_target.cmi hm];;  


let immediate_ingredients_for_executable hm=
 [Ocaml_target.cmx hm;Ocaml_target.executable hm];;  

let immediate_ingredients_for_debuggable hm=
  [Ocaml_target.dcmo hm;Ocaml_target.debuggable hm];;  

let ingredients_for_nodep mlx=[];;

let ingredients_for_ml_from_mll fs hm=
  let opt=Modulesystem.find_module_registration fs hm in
  if opt=None then raise(Unregistered_ml_from_mll(hm)) else 
  let dt=Option.unpack opt in
  (targets_from_ancestors fs dt)@(immediate_ingredients_for_ml_from_mll hm);;

let ingredients_for_ml_from_mly fs hm=
  let opt=Modulesystem.find_module_registration fs hm in
  if opt=None then raise(Unregistered_ml_from_mly(hm)) else 
  let dt=Option.unpack opt in
  (targets_from_ancestors fs dt)@(immediate_ingredients_for_ml_from_mly hm);;

let ingredients_for_cmi fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cmi(hm)) else 
          let dt=Option.unpack opt in
          (targets_from_ancestors fs dt)@(immediate_ingredients_for_cmi dt hm);;

let ingredients_for_cmo fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cmo(hm)) else 
          let dt=Option.unpack opt in
          (targets_from_ancestors fs dt)@(immediate_ingredients_for_cmo dt hm);;

let ingredients_for_dcmo fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_dcmo(hm)) else 
          let dt=Option.unpack opt in
          let ancestors=Modulesystem_data.all_ancestors dt in
          (debuggable_targets_from_ancestors fs ancestors)@(immediate_ingredients_for_dcmo dt hm);;

let ingredients_for_cma fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cma(hm)) else 
          let dt=Option.unpack opt in
          (targets_from_ancestors fs dt)@(immediate_ingredients_for_cma dt hm);;

let ingredients_for_cmx fs hm=
          let opt=Modulesystem.find_module_registration fs hm in
          if opt=None then raise(Unregistered_cmx(hm)) else 
          let dt=Option.unpack opt in
          (optimized_targets_from_ancestors fs dt)@(immediate_ingredients_for_cmx dt hm);;
 
let ingredients_for_executable fs hm=
  let opt=Modulesystem.find_module_registration fs hm in
  if opt=None then raise(Unregistered_executable(hm)) else 
  let dt=Option.unpack opt in
  (optimized_targets_from_ancestors fs dt)
  @(immediate_ingredients_for_executable hm);; 
  
let ingredients_for_debuggable fs hm=
  let l=Modulesystem.all_filedata fs in 
  let ml=Mlx_filename.join hm Ocaml_ending.Ml in
  let genealogy=Read_info_on_file_in_system.find_needed_data l ml in
  let dirfath=Image.image (Modulesystem_data.name) genealogy in
  let temp1=Image.image 
   			(fun t->Tidel.diforchan(Modulesystem_data.all_ancestors t)) 
   			genealogy in
   let temp2=Tidel.big_teuzin ((Tidel.diforchan(dirfath) )::temp1) in
   let tempf=(fun t->
   					let nam_t=Modulesystem_data.name t in
   					if Tidel.elfenn nam_t temp2
   					then Some(nam_t)
   					else None) in
   let allanc=Option.filter_and_unpack tempf l in
  (debuggable_targets_from_ancestors fs allanc)
  @(immediate_ingredients_for_debuggable hm);;    
  
let ingredients_for_toplevel_element fs name hm=
   let opt=Modulesystem.find_module_registration fs hm in
  if opt=None then raise(Unregistered_module_in_toplevel(name,hm)) else 
  let dt=Option.unpack opt in
  if (Modulesystem_data.mli_present dt)&&(not(Modulesystem_data.ml_present dt))
  then (ingredients_for_cmi fs hm)@[Ocaml_target.cmi hm]
  else (ingredients_for_cmo fs hm)@[Ocaml_target.cmo hm];;  
  
let ingredients_for_toplevel fs name l=
  let temp1=Image.image (ingredients_for_toplevel_element fs name) l in
  Preserve_initial_ordering.preserve_initial_ordering temp1;;
      
 
let ingredients_for_ocaml_target fs tgt=
  Ocaml_target.pattern_matching
    (ingredients_for_nodep)
    (ingredients_for_ml_from_mll  fs)
    (ingredients_for_ml_from_mly  fs)
  	(ingredients_for_cmi fs)
    (ingredients_for_cmo fs)
    (ingredients_for_dcmo fs)
    (ingredients_for_cma fs)
    (ingredients_for_cmx fs)
    (ingredients_for_executable  fs)
    (ingredients_for_debuggable  fs)
    (ingredients_for_toplevel fs)
    tgt;;



let marked_ingredients_for_unprepared_toplevel fs name l=
  let temp1=Image.image (ingredients_for_toplevel_element fs name) l in
  Preserve_initial_ordering.and_mark_endings temp1;;

let module_dependency_for_nodep mlx=false;;
let module_dependency_for_ml_from_mll fs l_hm hm1=
       if List.mem hm1 l_hm
       then true
       else  
       let dt1=Option.unpack(Modulesystem.find_module_registration fs hm1) in
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
let module_dependency_for_toplevel fs l_hm name l_hm2=
  List.exists(fun hm2->
  (module_dependency_for_cmo fs l_hm hm2)||(List.mem hm2 l_hm)
  ) l_hm2;;

let module_dependency_for_ocaml_target fs l_hm tgt=
  Ocaml_target.pattern_matching
    (module_dependency_for_nodep)
    (module_dependency_for_ml_from_mll  fs l_hm)
    (module_dependency_for_ml_from_mly  fs l_hm)
  	(module_dependency_for_cmi  fs l_hm)
    (module_dependency_for_cmo  fs l_hm)
    (module_dependency_for_dcmo fs l_hm)
    (module_dependency_for_cma  fs l_hm)
    (module_dependency_for_cmx  fs l_hm)
    (module_dependency_for_executable  fs l_hm)
    (module_dependency_for_debuggable  fs l_hm)
    (module_dependency_for_toplevel fs l_hm)
    tgt;;



let mlx_dependency_for_ocaml_target fs mlx tgt=
  let hm=Mlx_filename.half_dressed_core mlx in
  module_dependency_for_ocaml_target fs [hm] tgt;;

let mlx_list_dependency_for_ocaml_target fs l_mlx tgt=
 List.exists (fun mlx->mlx_dependency_for_ocaml_target fs mlx tgt) l_mlx;;
