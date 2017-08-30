(*
#use"code_namespace.ml";;
*)


let encode (bowl,l,s)=
    let s_bowl=(if bowl then "t" else "f") in
    s_bowl^(String.concat "#" l)^"*"^s;;
 
 let decode s=
   let j=Substring.leftmost_index_of_in "*" s in
   let s1=Cull_string.interval s 2 (j-1)
   and s2=Cull_string.cobeginning j s in
   let l=Str.split (Str.regexp_string "#") s1 in
   ((String.get s 0)='t',l,s2);;
 
(*   
 let z1=(false,["ani";"mal";"inst";"inct"],"sally");;
 let z2=encode z1;;
 let check=(decode z2);;
 
 let z1=(true,["uncle";"joe";"is";"sick"],"again");;
 let z2=encode z1;;
 let check=(decode z2);;
*)
