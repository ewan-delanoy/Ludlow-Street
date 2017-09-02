(*

#use"debugging_tools.ml";;

*)


let image f l=
  let modified_f=(fun x->
    try( (function _->false) (f x) ) with _->true
  ) in
  Option.find_really modified_f l;;


