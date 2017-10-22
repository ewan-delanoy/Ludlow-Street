(*

#use"visualization_tools.ml";;

*)

let ptokset_of_readable s=
    let (_,pt1)=Option.find (fun (s1,_)->s1=s) 
      (Php_projected_token_set.readables_and_toksets) in
    let (Php_projected_token_set.N l1)=pt1 in
    l1;;









