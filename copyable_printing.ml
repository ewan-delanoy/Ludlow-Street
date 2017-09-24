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
