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

module Name_For=struct
let str_list l=
   "["^(String.concat ";" (Image.image (fun t->"\""^t^"\"") l))^"]";;  

let str_times_str_list (a,b)=
   "(\""^a^"\","^(str_list b)^")";;

let str_times_str_list_list l=
  "["^(String.concat ";" (Image.image str_times_str_list l))^"]";;    

let elt (a,b)=
  "(\""^(Avoider_label.ocaml_name a)^"\","^(str_times_str_list_list b)^")";;
end;;

end;;  
       
let ocaml_name (L ll)=
  "L"^"ist_of_avoidables."^
  "(["^(String.concat ";" (Image.image Private.Name_For.elt l))^"])";;  

