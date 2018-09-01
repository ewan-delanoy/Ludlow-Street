(*

#use"Ocaml_analysis/read_needed_ocaml_files.ml";;

Given a module, first computes all the dependencies needed to
define this module, then reads the dependencies and 
finally the module itself.

*)

let read_needed_ocaml_files hm=
  let wmdata=German_wrapper.data() in
  let nm=Half_dressed_module.naked_module hm in
  let idx1=Coma_state.find_module_index wmdata nm in
  let pre_temp2=(Coma_state.ancestors_at_idx wmdata idx1)@[nm] in
  let temp2=Image.image (Coma_state.hm_from_nm wmdata) pre_temp2 in
  let all_files=Image.image  (fun hm2->
     Mlx_ended_absolute_path.to_path(Mlx_ended_absolute_path.join hm2 Ocaml_ending.Ml)
  ) temp2 in
   Read_ocaml_files.read_ocaml_files all_files;;
