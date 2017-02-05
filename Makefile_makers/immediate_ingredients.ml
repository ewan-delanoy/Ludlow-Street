(*


#use"Makefile_makers/immediate_ingredients.ml";;

*)



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



let for_ml_from_mll hm=
  let mll_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mll) in
  [mll_target];;

let for_ml_from_mly hm=
  let mly_target=Ocaml_target.no_dependencies(Mlx_filename.join hm Ocaml_ending.mly) in
  [mly_target];;

let for_cmi dt hm=
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

let for_cmo dt hm=
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


let for_dcmo=for_cmo;;

let for_cma=for_cmo;;

let for_cmx dt hm=
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


let for_executable hm=
 [Ocaml_target.cmx hm;Ocaml_target.executable hm];;  

let for_debuggable hm=
  [Ocaml_target.dcmo hm;Ocaml_target.debuggable hm];;  


