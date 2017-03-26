(*

#use"Ocaml_analysis/ocaml_gsyntax_item.ml";;

*)

type t={
  category : Ocaml_gsyntax_category.t;
  name : string;
  interval_for_name : int*int;
  content : string;
  interval_for_content : int*int;  
  is_an_included_item : bool;
};;



let make cat nm nm_itv ctnt ctnt_itv incldd_or_not=
    {
  		category =cat;
        name =nm;
        interval_for_name =nm_itv;
        content =ctnt;
        interval_for_content =ctnt_itv;  
        is_an_included_item =incldd_or_not;
    };;

