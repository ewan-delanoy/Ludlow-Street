(*

#use"Php_analizer/Great_Replacement/ivy_aware_kind.ml";;


*)

type t=
      Non_ivy
     |Ivy
     |Elsie
     |ElseIf;;

let non_ivy=Non_ivy;;
let ivy=Ivy;;
let elsie=Elsie;;
let elseif=ElseIf;;