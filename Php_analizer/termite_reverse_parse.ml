(*

#use"Php_analizer/termite_reverse_parse.ml";;

Useful to debug the Termite.parse function.

*)


let rec iterator_for_reverse_parsing (graet,da_ober,l)=
  match da_ober with
  []->None
  |(ret,wh)::da_ober2->
     (
       match Php_constructible_recognizer.recognize wh l with
       None->Some(graet,wh,l,Php_constructible_recognizer.reverse_sleepy_parse wh l)
       |Some(cr,peurrest)->
          let d=Positioned_php_token_list.length(l)-Positioned_php_token_list.length(peurrest) in
          let part=Positioned_php_token_list.big_head d l in
          let graet2=(if ret=Glued_or_not.Not_retained_not_glued then graet else part::graet) in
          iterator_for_reverse_parsing (graet2,da_ober2,peurrest)
     );;

let rp (Termite.Trmt(trmt)) l=iterator_for_reverse_parsing ([],trmt,l) ;;

     
 
