(*

#use"Ocaml_analysis/gdecompose_ocaml_text.ml";;

*)


exception Unreadable of string;;

let accuse_final_excerpt s i=
  let j=min(String.length s)(i+100) in
  raise(Unreadable(Cull_string.interval s i j));;

let read1 s=
  let opt=Gparser.apply Gparser_for_ocaml_language.main_prsr s 1 in
  if opt=None then accuse_final_excerpt s 1 else
  let res=Option.unpack opt in 
  let p=Gparser_result.final_cursor_position res in
  if p<=(String.length s) 
  then accuse_final_excerpt s p
  else 
  let temp1=Gparser_result.important_ranges res in
  Image.image (fun (i,j)->
    let opt=Gparser.apply Gparser_for_ocaml_language.elt_prsr s i in
    let res=Option.unpack opt in
    ((i,j),Option.unpack(Gparser_result.disjunction_index res))
  ) temp1;;
  
(*  

let update_accumulator (preceding_values,module_chain,current_module) ((i,j),idx)=
  if idx=1 /* we have a value */  
  then 
  
let s1="let jiving=abc;;";;
let res1=Gparser.apply Gparser_for_ocaml_language.elt_prsr s1   
   
*)   
   
     
  


(*

let g1=German_wrapper.data();;
let g2=List.filter Modulesystem_data.ml_present g1;;
let g3=List.flatten (image Modulesystem_data.acolytes g2);;
let g4=List.filter (fun mlx->snd(Mlx_filename.decompose mlx)=Ocaml_ending.ml) g3;;
let g5=image Mlx_filename.to_absolute_path g4;;
let g6=image (fun ap->let s=Io.read_whole_file ap in
  (-(String.length s),(ap,s))
) g5 ;;
let g7=image snd (ofo(Tidel2.diforchan g6));;
let g8=image (fun (ap,s)->read1 s) g7;;

*)
