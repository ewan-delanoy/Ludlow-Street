(*

#use"Ocaml_analysis/pre_read_ocaml_files.ml";;

Originated as the code shared by modules
read_ocaml_files and read_ocaml_files_without_expanding_inclusions.

*)

exception Pre_read_exn of string;;

module Private=struct
  exception Unreadable of string;;
  
  let accuse_final_excerpt s i=
    let j=min(String.length s)(i+100) in
    raise(Unreadable(Cull_string.interval s i j));;
  
  let uncatched_read1 s=
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
  
  exception Read1_exn of string;;
  
  let read1 s= try uncatched_read1 s with Unreadable(t)->raise(Read1_exn(t));;
    
  let describe_value_item s (i,j)=
       let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_value_making s i in
       let res=Option.unpack opt in
       let (i1,j1)=List.nth(Gparser_result.important_ranges res) 
            (Gparser_for_ocaml_language.index_for_name_in_value_parser-1)
       and (i2,j2)=List.nth(Gparser_result.important_ranges res) 
            (Gparser_for_ocaml_language.index_for_content_in_value_parser-1) 
       and (i3,j3)=Gparser_result.whole_range res in
         Ocaml_gsyntax_item.make
            Ocaml_gsyntax_category.Value
            (Cull_string.interval s i1 j1)
            (i1,j1)
            (Cull_string.interval s i3 j3)
            (* the -2 of because of the 2 characters in the double semicolon *)
            (Cull_string.interval s i2 (j2-2))
            (i2,j2-2)
            false;;
  
  let describe_type_item s (i,j)=
       let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_type_making s i in
       let res=Option.unpack opt in
       let (i1,j1)=List.nth(Gparser_result.important_ranges res) 3
       and (i2,j2)=List.nth(Gparser_result.important_ranges res) 6 
       and (i3,j3)=Gparser_result.whole_range res in
         Ocaml_gsyntax_item.make
            Ocaml_gsyntax_category.Type
            (Cull_string.interval s i1 j1)
            (i1,j1)
            (Cull_string.interval s i3 j3)
            (* the -2 of because of the 2 characters in the double semicolon *)
            (Cull_string.interval s i2 (j2-2))
            (i2,j2-2)
            false;;
  
  let describe_exception_item s (i,j)=
       let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_exception_making s i in
       let res=Option.unpack opt in
       let (i1,j1)=List.nth(Gparser_result.important_ranges res) 2
       and (i2,j2)=List.nth(Gparser_result.important_ranges res) 3 
       and (i3,j3)=Gparser_result.whole_range res in
         Ocaml_gsyntax_item.make
            Ocaml_gsyntax_category.Exception
            (Cull_string.interval s i1 j1)
            (i1,j1)
            (Cull_string.interval s i3 j3)
            (* the -2 of because of the 2 characters in the double semicolon *)
            (Cull_string.interval s i2 (j2-2))
            (i2,j2-2)
            false;;
  
  let describe_module_opener_item s (i,j)=
       let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_module_opener s i in
       let res=Option.unpack opt in
       let (i1,j1)=List.nth(Gparser_result.important_ranges res) 2
       and (i3,j3)=Gparser_result.whole_range res in 
         Ocaml_gsyntax_item.make
            Ocaml_gsyntax_category.Module_opener
            (Cull_string.interval s i1 j1)
            (i1,j1)
            (Cull_string.interval s i3 j3)
            ""
            (0,0)
            false;;
  
  
  let describe_module_closer_item=
         Ocaml_gsyntax_item.make
            Ocaml_gsyntax_category.Module_closer
            ""
            (0,0)
            ""
            ""
            (0,0)
            false;;
  
  
  let describe_module_inclusion_item s (i,j)=
       let opt=Gparser_apply.apply Gparser_for_ocaml_language.prsr_for_module_inclusion s i in
       let res=Option.unpack opt in
       let (i1,j1)=List.nth(Gparser_result.important_ranges res) 2 
       and (i3,j3)=Gparser_result.whole_range res in 
         Ocaml_gsyntax_item.make
            Ocaml_gsyntax_category.Module_inclusion
            (Cull_string.interval s i1 j1)
            (i1,j1)
            (Cull_string.interval s i3 j3)
            ""
            (0,0)
            false;;
            
   let describe_item s ((i,j),idx)=
     if idx=Gparser_for_ocaml_language.index_for_value
     then Some(describe_value_item s (i,j))
     else
     if idx=Gparser_for_ocaml_language.index_for_type
     then Some(describe_type_item s (i,j))
     else
     if idx=Gparser_for_ocaml_language.index_for_exception
     then Some(describe_exception_item s (i,j))
     else
     if idx=Gparser_for_ocaml_language.index_for_module_opener
     then Some(describe_module_opener_item s (i,j))
     else
     if idx=Gparser_for_ocaml_language.index_for_module_closer
     then Some(describe_module_closer_item)
     else          
     if idx=Gparser_for_ocaml_language.index_for_module_inclusion
     then Some(describe_module_inclusion_item s (i,j))
     else None;;
     
  let uncatched_read2 s=
     Option.filter_and_unpack (describe_item s) (read1 s);;   
     
  
  
  let pre_read s= try uncatched_read2 s with Read1_exn(t)->raise(Pre_read_exn(t));;
  end;;

  let pre_read =Private.pre_read;;