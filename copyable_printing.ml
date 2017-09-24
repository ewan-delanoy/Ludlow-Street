(*

#use"copyable_printing.ml";;

*)

let print_stringlist_naively l=
  let temp1=Image.image (fun s->(Strung.enclose s)) l in
  let temp2=String.concat ";" temp1 in
  "["^temp2^"]";;  

let print_stringlist_with_offset l w=
    let offset=String.make w ' ' in
    let temp1=Image.image (fun s->offset^"  "^(Strung.enclose s)) l in
    let temp2=String.concat ";\n" temp1 in
    ("[\n")^temp2^"\n"^offset^"]";;

let print_stringlist w l=
     if List.length(l)=1
     then print_stringlist_naively l
     else print_stringlist_with_offset l w;;

let sbf_to_string (s,fl)=(Strung.enclose s)^","^(string_of_float fl);;

let print_sbf_list_naively l=
    let temp1=Image.image sbf_to_string l in
    let temp2=String.concat ";" temp1 in
    "["^temp2^"]";;  
    
let print_sbf_list_with_offset l w=
    let offset=String.make w ' ' in
    let temp1=Image.image (fun t->offset^"  "^(sbf_to_string t)) l in
    let temp2=String.concat ";\n" temp1 in
    ("[\n")^temp2^"\n"^offset^"]";;
    
let print_sbf_list w l=
    if List.length(l)=1
    then print_sbf_list_naively l
    else print_sbf_list_with_offset l w;;     

(*

let s1=print_sbf_list 3
  [
    "abc",5.78;"de",9.15;"fghijklm",3.21
  ];;

*)



