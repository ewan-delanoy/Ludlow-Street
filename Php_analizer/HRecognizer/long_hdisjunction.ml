(*
#use"Php_analizer/HRecognizer/long_hdisjunction.ml";;


A tool to manage and to pretty_print long disjunctions in parsers.
Names not explicitly given are by default made from the wole parser
name with an index number added at the end.

*)


type t=L of string*((string*(Nonatomic_hrecognizer.t list)) list);;

let to_usual (L(name_for_whole,ll))=
    let counter=ref(0) in
    let temp1=Image.image (
         fun (name,l)->
           if (List.length(l)=1)
           then List.hd(l)
           else 
           let actual_name=(
             if name<>"" then name else
             let j=(!counter)+1 in
             let _=(counter:=j) in
              name_for_whole^(string_of_int j)
           ) in
           Hregistrar.possibly_already_created_chain actual_name l
    ) ll in
    (name_for_whole,temp1);;

let of_usual (name_for_whole,ll)=
  let w=String.length name_for_whole in
   let temp1=Image.image (
      fun rcgzr->
        let name=Nonatomic_hrecognizer.name rcgzr 
        and l=Nonatomic_hrecognizer.chained_version(rcgzr) in
        if not(Substring.begins_with name name_for_whole)
        then (name,l)
        else let temp2=Cull_string.cobeginning w name in
             let is_special=(try (fun _->true)(int_of_string temp2) with _->false) in
             if is_special
             then ("",l)
             else (name,l)
   ) ll in
   L(name_for_whole,temp1);;
   



