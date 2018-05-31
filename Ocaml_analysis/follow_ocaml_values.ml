(*

#use"Ocaml_analysis/follow_ocaml_values.ml";;

*)

module Private=struct

let order_for_string_pairs =
  ((Total_ordering.product 
  Total_ordering.lex_for_strings Total_ordering.standard): 
  (string*string) Total_ordering.t
  );;

end;;

let follow_values l=
  let temp1=Option.filter_and_unpack (
     fun x->
       match x.Ocaml_gsyntax_item.category with
       Ocaml_gsyntax_category.Value                                                                          
    | Ocaml_gsyntax_category.Type
    | Ocaml_gsyntax_category.Exception->
            Some(x.Ocaml_gsyntax_item.name,x.Ocaml_gsyntax_item.content)
    | _->None
) l in
let temp2=Ordered.diforchan_plaen Private.order_for_string_pairs temp1 in
Followed_ocaml_values.F(temp2);;

(*
let local_delchacre (Followed_ocaml_values.F l1) (Followed_ocaml_values.F l2)=
   let names_from_l1=Image.image fst l1
   and names_from_l2=Image.image fst l2 in
   let list_of_all_names=Ordered.teuzin_plaen  
   Total_ordering.lex_for_strings names_from_l1 names_from_l2 in
   let m=List.length list_of_all_names in
   let array_of_all_names=Array.of_list list_of_all_names in
*)







(*

Ordered.cooperation_for_two
Total_ordering.standard
(Ordered.S [1;2;3;4;5;6])
(Ordered.S [2;5;7;8]);;

*)
