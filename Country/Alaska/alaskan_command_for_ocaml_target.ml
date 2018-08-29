(*

#use"Country/Alaska/alaskan_command_for_ocaml_target.ml";;

*)


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

let ingr=Alaskan_ingredients_for_ocaml_target.ingredients_for_ocaml_target;;

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
  let opt_idx=Modify_md_list.seek_module_index wmdata nm in
  if opt_idx=None then raise(Unregistered_cmi(hm)) else 
  let idx=Option.unpack opt_idx in
  let s_root=Root_directory.connectable_to_subpath(dir) in
  let s_hm=Half_dressed_module.uprooted_version hm in
  let s_fhm=s_root^s_hm in
  let mli_reg=Modify_md_list.check_ending_in_at_idx Ocaml_ending.mli wmdata idx in
  let ending=(if mli_reg then ".mli" else ".ml") in
  let central_cmd=
      "ocamlc  -bin-annot "^
      (Modify_md_list.needed_dirs_and_libs_in_command false wmdata idx)^
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
  let opt_idx=Modify_md_list.seek_module_index wmdata nm in
  if opt_idx=None then raise(Unregistered_cmo(hm)) else 
  let idx=Option.unpack opt_idx in
  let s_hm=Half_dressed_module.uprooted_version hm in
  let s_root=Root_directory.connectable_to_subpath(dir) in
  let s_fhm=s_root^s_hm in
  let dirs_and_libs=Modify_md_list.needed_dirs_and_libs_in_command false wmdata idx in
  [ 
    "ocamlc -bin-annot "^dirs_and_libs^" -o "^s_fhm^".cmo -c "^s_fhm^".ml";
    "mv "^s_fhm^".cm* "^s_root^"_build/"
  ];;

let command_for_dcmo dir wmdata hm=
  let nm=Half_dressed_module.naked_module hm in
  let opt_idx=Modify_md_list.seek_module_index wmdata nm in
  if opt_idx=None then raise(Unregistered_cmo(hm)) else 
  let idx=Option.unpack opt_idx in
  let s_hm=Half_dressed_module.uprooted_version hm in
  let s_root=Root_directory.connectable_to_subpath(dir) in
  let s_fhm=s_root^s_hm in
  let dirs_and_libs=Modify_md_list.needed_dirs_and_libs_in_command false wmdata idx in
  [ 
    "ocamlc -bin-annot -g "^dirs_and_libs^" -o "^s_fhm^".d.cmo -c "^s_fhm^".ml";
    "mv "^s_fhm^".d.cm* "^s_root^"_build/"
  ];;

let command_for_cma dir wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=Modify_md_list.seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_cma(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Half_dressed_module.uprooted_version hm in
    let s_root=Root_directory.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let dirs_and_libs=Modify_md_list.needed_dirs_and_libs_in_command true wmdata idx in
    [ 
      "ocamlopt -bin-annot -a "^dirs_and_libs^" -o "^s_fhm^".cma -c "^s_fhm^".ml";
      "mv "^s_fhm^".cm* "^s_root^"_build/"
    ];;  

let command_for_cmx dir wmdata hm=
    let nm=Half_dressed_module.naked_module hm in
    let opt_idx=Modify_md_list.seek_module_index wmdata nm in
    if opt_idx=None then raise(Unregistered_cmx(hm)) else 
    let idx=Option.unpack opt_idx in
    let s_hm=Half_dressed_module.uprooted_version hm in
    let s_root=Root_directory.connectable_to_subpath(dir) in
    let s_fhm=s_root^s_hm in
    let dirs_and_libs=Modify_md_list.needed_dirs_and_libs_in_command true wmdata idx in
    [ 
      "ocamlopt -bin-annot "^dirs_and_libs^" -o "^s_fhm^".cmx -c "^s_fhm^".ml";
      "mv "^s_fhm^".cm* "^s_root^"_build/"
    ];;      
          
let command_for_executable dir wmdata hm=
  let nm=Half_dressed_module.naked_module hm in
  let opt_idx=Modify_md_list.seek_module_index wmdata nm in
  if opt_idx=None then raise(Unregistered_executable(hm)) else 
  let idx=Option.unpack opt_idx in
  let s_hm=Half_dressed_module.uprooted_version hm in
  let s_root=Root_directory.connectable_to_subpath(dir) in
  let s_fhm=s_root^s_hm in
  let temp1=ingr wmdata (Ocaml_target.EXECUTABLE(hm)) in
  let temp2=Option.filter_and_unpack cmx_manager temp1 in
  let long_temp2=Image.image (fun t->s_root^t) temp2 in
  let dirs_and_libs=Modify_md_list.needed_dirs_and_libs_in_command true wmdata idx  in
  [ 
    "ocamlopt -bin-annot "^dirs_and_libs^" -o "^s_fhm^".ocaml_executable "^
      (String.concat " " long_temp2);
    "mv "^s_fhm^".cm* "^s_root^"_build/";
    "mv "^s_fhm^".ocaml_executable "^s_root^"_build/"
  ];;
          
let command_for_debuggable dir wmdata hm=
  let nm=Half_dressed_module.naked_module hm in
  let opt_idx=Modify_md_list.seek_module_index wmdata nm in
  if opt_idx=None then raise(Unregistered_debuggable(hm)) else 
  let idx=Option.unpack opt_idx in
  let s_hm=Half_dressed_module.uprooted_version hm in
  let s_root=Root_directory.connectable_to_subpath(dir) in
  let s_fhm=s_root^s_hm in
  let temp1=ingr wmdata (Ocaml_target.DEBUGGABLE(hm)) in
  let temp2=Option.filter_and_unpack dcmo_manager temp1 in
  let long_temp2=Image.image (fun t->s_root^t) temp2 in
  let dirs_and_libs=Modify_md_list.needed_dirs_and_libs_in_command false wmdata idx in
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
   

 
