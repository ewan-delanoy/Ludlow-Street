(*

#use"Prepare_html/html_bracable.ml";;

*)


type t=
   Simple of (Html_parameter.t*string) list
  |Constant of string 
  |Uncategorized of ((Html_parameter.t list)->string);;

exception Missing_parameter;;

let simple l=Simple(l);;
let constant s=Constant(s);;
let uncategorized f=Uncatgeorized(f);;

let eval l_param =function
  Simple(l_assoc)->
       ( match Option.seek(fun (key,vval)->List.mem key l_param) l_assoc  with
         None->raise(Missing_parameter)
         |Some(_,vval)->vval
       )
  |Constant(cst)->cst     
  |Uncategorized (f)->f l_param;;    