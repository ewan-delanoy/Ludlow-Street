(*

#use"debugging_tools.ml";;

*)


let image f l=
  let modified_f=(fun (j,x)->
    try( (function _->false) (f x) ) with _->true
  ) in
  let modified_l=Ennig.index_everything l in
  Option.find_really modified_f modified_l;;


