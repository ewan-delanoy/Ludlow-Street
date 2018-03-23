
(* 

#use"Country/Alaska/alaskan_write_makefile.ml";;


*)

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

let write_makefile_element main_root mdata tgt=
 let ingrs=Alaskan_ingredients_for_ocaml_target.ingredients_for_ocaml_target mdata tgt in
 let sliced_ingrs=slice_shortened_targets ingrs in
 let cmds=Alaskan_command_for_ocaml_target.command_for_ocaml_target 
                       main_root mdata tgt in
 let s1=(Ocaml_target.to_shortened_string tgt)^" : " 
 and s2=String.concat " \\\n" sliced_ingrs
 and s3="\n\t"
 and s4=String.concat "\n\t" cmds in
 String.concat "" [s1;s2;s3;s4];;
 
 
let write_makefile (main_root,main_toplevel_name) mdata=
 let temp1=Alaskan_data.default_targets German_constant.main_toplevel_name mdata in
 let temp2=Image.image (write_makefile_element main_root mdata) temp1 in
 let temp5=slice_targets  temp1 in
 let temp6=String.concat " \\\n" temp5 in
 let temp7="clean :\n\trm -r -f "^temp6^"\n\n" in
 String.concat "\n\n" (temp2@[temp7]);;

 