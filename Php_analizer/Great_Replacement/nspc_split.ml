(*

#use"Php_analizer/Great_Replacement/nspc_split.ml";;

*)




exception Decompose_exn;;

let decompose s=
     let temp1=Lines_in_string.core s in
     let temp2=List.filter (
       fun (line_idx,line)->
          Nspc_detect.test_for_namespace_line line
     ) temp1 in
     if temp2=[] 
     then raise Decompose_exn
     else
     let m=List.length(temp1)+1 in
     let temp3=temp2@[(m,"")] in
     let temp4=Listennou.universal_delta_list temp3 in
     let items=Image.image (
        fun ((k1,nspc_line1),(k2,_))->
          let ttemp6=Option.filter_and_unpack (
             fun (line_idx,line)->
               if (k1<line_idx)&&(line_idx<k2)
               then Some(line)
               else None
          ) temp1 in
          let ttemp7=String.concat "\n" ttemp6 in
          let k3=After.after_closing_character 
                  ('{','}') ttemp7 (1,1) in
          let ttemp8=Cull_string.cobeginning (k3-2) ttemp7 in
          let ttemp9=(if (k2=m)&&(Substring.ends_with s "\n") 
                      then ttemp8^"\n"
                      else ttemp8 ) in       
          (nspc_line1,Cull_string.beginning (k3-2) ttemp7 ,ttemp9)
     ) temp4 in
     let i1=fst(List.hd temp2) in
     let temp8=List.filter (fun (line_idx,line)->line_idx<i1) temp1 in
     let before_namespaces=String.concat "\n" (Image.image snd temp8) in
     (before_namespaces,items);; 

let recompose (before_namespaces,items)=
      let temp1=Image.image (fun (a,b,c)->a^"\n"^b^c) items in
      before_namespaces^"\n"^(String.concat "\n" temp1);;

(*
let z1="<?php12\nnamespace A{\n34\n56}78\nnamespace B{\n78\n90}12\n"^
      "\nnamespace C{\n44\n55}78\nnamespace D{\n87\n09}12\n347";;      
let z1="<?php12\nnamespace A{\n34\n56}78\nnamespace B{\n78\n90}12\n"^
"\nnamespace C{\n44\n55}78\nnamespace D{\n87\n09}12\n347\n";;
let z1="<?php12\nnamespace A{\n34\n56}78\nnamespace B{\n78\n90}12\n"^
"\nnamespace C{\n44\n55}78\nnamespace D{\n87\n09}12\n347\n\n\n";;
let z2=decompose z1;;
let z3=recompose z2;;
let check=(z3=z1);;
*)
