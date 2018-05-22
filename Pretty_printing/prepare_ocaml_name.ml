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

let for_string s=Disaggregated_ocaml_name.D(["\""^s^"\""]);;
let for_string_list l=for_list (for_string) l;;

let for_str_times_strlist (a,b)=
  for_pair (for_string,for_string_list) (a,b);;

let for_p_str_times_str_list_p_list l=
  for_list for_str_times_strlist l;;    

