(*

#use"Php_analizer/Great_Replacement/classlike_decomposition.ml";;


*)

let reference_for_subtle_case=ref("",0);;
exception Subtle_case;;


let on_namespaced_text nspc_name s=
   let n=String.length s in
   let rec tempf=(
     fun (graet,idx)->
       if idx>n
       then List.rev graet
       else 
       match Unshadowed_appearance.next 
        s ["abstract";"final";"class";"interface"] idx with
       None->
           let last_item=
              Classlike_item.non_class nspc_name 
                (Cull_string.interval s idx n) in
              List.rev (last_item::graet)    
       |Some(jdx)->
            let opt=First_pass_parse.classlike_item nspc_name s jdx in
            if opt=None
            then (reference_for_subtle_case:=(s,jdx);raise(Subtle_case))
            else
            let item2=Option.unpack opt in
            let new_idx=jdx+(Classlike_item.length item2) in
            if jdx=idx
            then tempf(item2::graet, new_idx) 
            else let item1=
                  Classlike_item.non_class nspc_name 
                   (Cull_string.interval s idx (jdx-1)) in
                 tempf(item2::item1::graet, new_idx)
   ) in
   tempf([],1);; 
           
(*

on_namespaced_text "NKP" "123 class abc xyz {uvw}";;
on_namespaced_text "NKP" "class abc xyz {uvw}";;

*)

let on_namespaced_part
(nspc_line,nspc_content,nspc_offset,after_nspc)=
  let (nspc_name,_)=Option.unpack(Nspc_detect.extract_namespace_name nspc_line) in
  let elt1=Classlike_item.namespace_line nspc_line
  and temp2=on_namespaced_text nspc_name nspc_content
  and elt3=Classlike_item.after_namespace_comments (nspc_offset^after_nspc) in
  Merge_nonclasses.mn (elt1::temp2@[elt3]);;
  
(* The decomposed form is assumed to be standardized already. *)

let on_decomposed_form df=
    let elt1=Classlike_item.non_class "" 
      (Nspc_decomposed_form.before_namespaces df)  
    and temp2=Image.image on_namespaced_part 
    (Nspc_decomposed_form.namespaced_parts df) in
    Merge_nonclasses.mn (List.flatten([elt1]::temp2));;  
  
let on_string s=on_decomposed_form(
  Nspc_split.decompose s
);;



