(*

#use"Ocaml_analysis/find_value_descendants.ml";;

if the string argument has a dot inside it, we interpret it
as a value inside a module.
Otherwise we interpret it as a mere string.

*)

let fvd all_ocaml_items s1=
   let j1=String.index(s1)('.')+1 in
   let module_name=Cull_string.beginning (j1-1) s1 
   and native_name=Cull_string.cobeginning j1 s1 in
   let (native_ones,foreign_ones)=
   List.partition (
      fun itm->
        let s=Ocaml_gsyntax_item.name itm in
        let j=String.index(s)('.')+1 in
        (Cull_string.beginning (j-1) s)=module_name
   ) all_ocaml_items in
   let native_descendants=
   Option.filter_and_unpack(
     fun itm->
        if Substring.is_a_substring_of
           native_name (Ocaml_gsyntax_item.content itm)
        then Some(Ocaml_gsyntax_item.name itm)
        else None   
   ) native_ones 
   and foreign_descendants=
   Option.filter_and_unpack(
     fun itm->
        if Substring.is_a_substring_of
           native_name (Ocaml_gsyntax_item.content itm)
        then Some(Ocaml_gsyntax_item.name itm)
        else None   
   ) foreign_ones in
   (native_descendants,foreign_descendants);;
   
