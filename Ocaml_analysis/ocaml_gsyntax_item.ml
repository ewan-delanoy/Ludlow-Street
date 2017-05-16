(*

#use"Ocaml_analysis/ocaml_gsyntax_item.ml";;

*)

type t={
  category : Ocaml_gsyntax_category.t;
  name : string;
  interval_for_name : int*int;
  interval_for_whole : int*int;
  content : string;
  interval_for_content : int*int;  
  is_an_included_item : bool;
};;

let name x=x.name;;
let content x=x.content;;


let make cat nm nm_itv intr_itv ctnt ctnt_itv incldd_or_not=
    {
  		category =cat;
        name =nm;
        interval_for_name =nm_itv;
        interval_for_whole =nm_itv;
        content =ctnt;
        interval_for_content =ctnt_itv;  
        is_an_included_item =incldd_or_not;
    };;

let prepend_prefix prefix x=
    {
  		category =x.category;
        name =prefix^"."^x.name;
        interval_for_name =x.interval_for_name;
        interval_for_whole =x.interval_for_whole;
        content =x.content;
        interval_for_content =x.interval_for_content;  
        is_an_included_item =x.is_an_included_item;
    };;
    
let include_in_new_namespace new_nmspc x=
    {
  		category =x.category;
        name =new_nmspc^(Father_and_son.invasive_father x.name '.');
        interval_for_name =x.interval_for_name;
        interval_for_whole =x.interval_for_whole;
        content =x.content;
        interval_for_content =x.interval_for_content;  
        is_an_included_item =true;
    };;    
    
    
    
    
    
    
    
    