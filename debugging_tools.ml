(*

#use"debugging_tools.ml";;

*)


let image f l=
  let modified_f=(fun (j,x)->
    try( (function _->false) (f x) ) with _->true
  ) in
  let modified_l=Ennig.index_everything l in
  Option.find_really modified_f modified_l;;

exception Star_exn;;

let star prsr=
  let rec tempf=(fun (graet,cr,z)->
     try(
      match Php_parser.parse prsr z with
       None->None
      |Some(res,cr2,z2)->tempf(res::graet,cr2,z2)
     ) with
     _->Some(z) 
  ) in
  let tempg=(fun l->
    let candidate1=tempf([],Php_char_range.dummy,l) in
    if candidate1<>None
    then candidate1
    else raise(Star_exn)
  ) in
  tempg;;

