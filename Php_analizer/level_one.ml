(*
#use"Php_analizer/level_one.ml";;
*)


let level_one=Php_parser_homomorphism.star Beaver_for_statement.parser;;

let one_more_time ll=
  let temp1=Explicit.image (fun l->(l,Php_parser_homomorphism.star
    Beaver_for_statement.parser l) )  ll in
  let (temp2,temp3)=List.partition(fun (l,res)->res=None) temp1 in
  let part1=Image.image fst temp2 in
  let part2=Option.filter_and_unpack(
    fun (l,res)->
       let (_,cr,l2)=Option.unpack(res) in
       if l2=[] then None else Some(l2)
  ) temp3 in
  let whole=part1@part2 in
  (whole (* ,Php_molecularize.molecularize(List.hd whole) *));;