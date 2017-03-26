(*

#use"Ocaml_analysis/gdecompose_ocaml_text.ml";;

*)

exception Unreadable of string;;

let accuse_final_excerpt s i=
  let j=min(String.length s)(i+100) in
  raise(Unreadable(Cull_string.interval s i j));;

let read1 s=
  let opt=Gparser_apply.apply Gparser_for_ocaml_language.main_prsr s 1 in
  if opt=None then accuse_final_excerpt s 1 else
  let res=Option.unpack opt in 
  let p=Gparser_result.final_cursor_position res in
  if p<=(String.length s) 
  then accuse_final_excerpt s p
  else 
  let temp1=Gparser_result.important_ranges res in
  Image.image (fun (i,j)->
    let opt=Gparser_apply.apply Gparser_for_ocaml_language.elt_prsr s i in
    let res=Option.unpack opt in
    ((i,j),Option.unpack(Gparser_result.disjunction_index res))
  ) temp1;;
  
let describe_value_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_value_making s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 2
     and (i2,j2)=List.nth(Gparser_result.important_ranges res) 5 in
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Value
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (* the -2 of because of the 2 characters in the double semicolon *)
          (Cull_string.interval s i2 (j2-2))
          (i2,j2-2)
          false;;

let describe_type_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_type_making s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 3
     and (i2,j2)=List.nth(Gparser_result.important_ranges res) 6 in
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Type
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (* the -2 of because of the 2 characters in the double semicolon *)
          (Cull_string.interval s i2 (j2-2))
          (i2,j2-2)
          false;;

let describe_exception_item s (i,j)=
     let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_type_making s i in
     let res=Option.unpack opt in
     let (i1,j1)=List.nth(Gparser_result.important_ranges res) 2
     and (i2,j2)=List.nth(Gparser_result.important_ranges res) 3 in
       Ocaml_gsyntax_item.make
          Ocaml_gsyntax_category.Exception
          (Cull_string.interval s i1 j1)
          (i1,j1)
          (* the -2 of because of the 2 characters in the double semicolon *)
          (Cull_string.interval s i2 (j2-2))
          (i2,j2-2)
          false;;


(*


 prsr_for_value_making;
       prsr_for_type_making;
       prsr_for_exception_making;
       prsr_for_comment;
       prsr_for_sharp_comment;
       prsr_for_special_sharp;
       prsr_for_module_opener;
       prsr_for_module_ender;
       prsr_for_module_inclusion;
       prsr_for_specialities;
       prsr_for_white;
       
*)       
          
  
(*  

let s1="let jiving=234  ;;";;
describe_value_item s1 (1,String.length s1);;

let s2="type ('a,'b) sister=('a list)*'b*string;;";;
describe_type_item s2 (1,String.length s2);;

let s3="type sister=(int list)*float*string;;";;
describe_type_item s3 (1,String.length s3);;

let s4="exception Foobar of string*int;;";;
describe_exception_item s4 (1,String.length s4);;

let s5="exception Foobar;;";;
describe_exception_item s5 (1,String.length s5);;




let update_accumulator s (preceding_values,module_chain,current_module) ((i,j),idx)=
  if idx=1 /* we have a value */  
  then let opt=Gparser_for_ocaml_language.prsr_for_value_making s i in
       let res=Option.unpack opt in
       
       let new_item=
        Ocaml_gysntax_item.make
          Ocaml_gsyntax_category.Value
          
        {
  		category =cat;
        name =nm;
        interval_for_name =nm_itv;
        content =ctnt;
        interval_for_content =ctnt_itv;  
    };;
  
let s1="let jiving=234  ;;";;
let res1=Gparser.apply Gparser_for_ocaml_language.prsr_for_value_making s1 1;;  
let z1=Strung.show_indices s1;;   
   
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
