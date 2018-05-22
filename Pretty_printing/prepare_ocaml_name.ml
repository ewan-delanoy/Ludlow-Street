(*

#use"Pretty_printing/prepare_ocaml_name.ml";;

*)

let for_list f l=
    match l with
    []->Disaggregated_ocaml_name.D["[]"]
    |a::peurrest->
      let uf=(fun x->Disaggregated_ocaml_name.unveil(f x)) in
      let temp1=(uf a)::(Image.image (fun x-> ";"::(uf x)) peurrest) in
      let temp2=List.flatten temp1 in
      let temp3="["::(temp2@["]"]) in
      Disaggregated_ocaml_name.D(temp3);;

let for_pair (fa,fb) (a,b)=
    let sa=Disaggregated_ocaml_name.unveil(fa a)
    and sb=Disaggregated_ocaml_name.unveil(fb b) in
    Disaggregated_ocaml_name.D("("::sa@(sb@[")"]));;

let for_labelled_elt f lab x=
  let sx=Disaggregated_ocaml_name.unveil(f x) in    
  Disaggregated_ocaml_name.D(lab::"("::sx@[")"]);;

let for_labelled_pair (fx,fy) lab (x,y)=
    let sx=Disaggregated_ocaml_name.unveil(fx x) 
    and sy=Disaggregated_ocaml_name.unveil(fy y) in    
    Disaggregated_ocaml_name.D(lab::"("::sx@(","::sy@[")"]));;  

let for_labelled_triple (fx,fy,fz) lab (x,y,z)=
      let sx=Disaggregated_ocaml_name.unveil(fx x) 
      and sy=Disaggregated_ocaml_name.unveil(fy y) 
      and sz=Disaggregated_ocaml_name.unveil(fy y) in    
      Disaggregated_ocaml_name.D(lab::"("::sx@(","::sy@(","::sz@[")"])));;  
  
    
let for_string s=Disaggregated_ocaml_name.D(["\""^s^"\""]);;
let for_string_list l=for_list (for_string) l;;

let for_str_times_strlist (a,b)=
  for_pair (for_string,for_string_list) (a,b);;

let for_p_str_times_str_list_p_list l=
  for_list for_str_times_strlist l;;    

let for_char c=Disaggregated_ocaml_name.D(["'"^(String.make 1 c)^"'"]);;
let for_char_list l=for_list (for_char) l;;
