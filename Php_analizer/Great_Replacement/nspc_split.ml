(*

#use"Php_analizer/Great_Replacement/nspc_split.ml";;

*)

module Private=struct

exception Missing_php_open_tag;;

let treat_non_namespaced_case whole_text lines=
    match Option.seek (fun (j,line)->
      Nspc_detect.test_for_declaration_line line) lines   with
     None->if(not(Substring.begins_with whole_text "<?php"))
          then raise(Missing_php_open_tag)
          else 
          let opened_text=Cull_string.cobeginning 5 whole_text in
         Nspc_decomposed_form.make 
             "<?php" (Some opened_text) []  
    |Some(j1,_)->
          let (before,after)=
              Prepared.partition_in_two_parts (fun (j,line)->j<=j1)
                 lines in
           let text_before=String.concat "\n" (Image.image snd before)
           and text_after=String.concat "\n" (Image.image snd after) in
           Nspc_decomposed_form.make  
             text_before (Some text_after) []     
       ;;

let rewrite_namespaced_item 
   all_lines ((k1,nspc_line1),(k2,_))=
     let lines_between=Option.filter_and_unpack (
      fun (line_idx,line)->
        if (k1<line_idx)&&(line_idx<k2)
        then Some(line)
        else None
   ) all_lines in
   let text_between=String.concat "\n" lines_between in
   let (_,is_standard)=Option.unpack(Nspc_detect.extract_namespace_name nspc_line1) in
   if is_standard
   then let idx=After.after_closing_character 
           ('{','}') text_between (1,1) in     
   (nspc_line1,
       Cull_string.beginning (idx-2) text_between,"}",
       Cull_string.cobeginning (idx-1) text_between )
   else (nspc_line1,text_between,"","");;

end;;   

let decompose s=
     let all_lines=Lines_in_string.core s in
     let nspc_lines=List.filter (
       fun (line_idx,line)->
          Nspc_detect.test_for_namespace_line line
     ) all_lines in
     if nspc_lines=[] 
     then Private.treat_non_namespaced_case s all_lines
     else
     let m=List.length(all_lines)+1 in
     let temp3=nspc_lines@[(m,"")] in
     let adjacent_lines=Listennou.universal_delta_list temp3 in
     let items=Image.image (Private.rewrite_namespaced_item all_lines) adjacent_lines in
     let i1=fst(List.hd nspc_lines) in
     let temp8=List.filter (fun (line_idx,line)->line_idx<i1) all_lines in
     let before_namespaces=String.concat "\n" (Image.image snd temp8) in
     Nspc_decomposed_form.make before_namespaces None items;;
     

let recompose dec_form=
      let before_namespaces=Nspc_decomposed_form.before_namespaces dec_form
      and items=Nspc_decomposed_form.namespaced_parts dec_form in
      let all_items=Image.image (fun (a,b,c,d)->a^"\n"^b^c^d) items in
      match Nspc_decomposed_form.namespacable dec_form with
      None->before_namespaces^"\n"^(String.concat "\n" all_items)
      |Some(after_decl)->before_namespaces^"\n"^after_decl;;

(*
let z1="<?php12\nnamespace A{\n34\n56}78\nnamespace B{\n78\n90}12\n"^
      "\nnamespace C{\n44\n55}78\nnamespace D{\n87\n09}12\n347";;      
let z2="<?php12\nnamespace A{\n34\n56}78\nnamespace B{\n78\n90}12\n"^
"\nnamespace C{\n44\n55}78\nnamespace D{\n87\n09}12\n347\n";;
let z3="<?php12\nnamespace A{\n34\n56}78\nnamespace B{\n78\n90}12\n"^
"\nnamespace C{\n44\n55}78\nnamespace D{\n87\n09}12\n347\n\n\n";;
let z4="<?php12\nnamespace A;\n34\n56789\nnamespace B;\n78\n9012\n"^
      "\nnamespace C{\n44\n55}78\nnamespace D;\n87\n0912\n347";;

let zl=Image.image(
      fun z->
            let zz=decompose z in
            let zzz=recompose zz in
            (z,zz,zzz)
) [z1;z2;z3;z4];;

let check=List.filter (fun (z,zz,zzz)->zzz<>z) zl;;
*)
