(*

#use"Php_analizer/HRecognizer/list_of_avoidables.ml";;

*)

type t=L of ( Avoider_label.t * ((string*(string list)) list)) list;;

let empty_one=L [];;

let name_already_used (L ll) nahme=
    List.exists (fun (_,l)->List.exists (fun (name,_)->name=nahme) l) ll;;

let add_new_element (L ll) avdbl nahme parts=
    if List.for_all (fun (lbl,l)->lbl<>avdbl) ll
    then  L(ll@[avdbl,[nahme,parts]])
    else 
    let new_ll=  
    Image.image (
      fun (lbl,l)->
        let new_l=(if lbl=avdbl then l@[nahme,parts] else l) in
        (lbl,new_l)
    ) ll  in
    L(new_ll);;

let avoided_words (L lll) avdbl=
    let ll=List.assoc avdbl lll in
    Image.image (fun (_,l)->String.concat "" l) ll;;    

module Private=struct


let prepare_elt_name  (a,b)=
  Prepare_ocaml_name.for_pair
    (Avoider_label.prepare_ocaml_name,Prepare_ocaml_name.for_p_str_times_str_list_p_list ) 
    (a,b);;

let prepare_elt_list_name l=
  Prepare_ocaml_name.for_list    prepare_elt_name l;;

end;;

let prepare_ocaml_name (L ll)=
  Prepare_ocaml_name.for_labelled_elt
  Private.prepare_elt_list_name
  ("L"^"ist_of_avoidables.L")  ll;;
