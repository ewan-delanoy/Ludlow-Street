
(* 

#use"Country/Germany/german_recently_deleted.ml";;


*)


let delete_file ap recently_deleted=
  let s_ap=Absolute_path.to_string ap in
  let s=Directory_name.cut_beginning German_constant.root s_ap in
  Ordered.forget_order(Ordered_string.outsert s recently_deleted);; 