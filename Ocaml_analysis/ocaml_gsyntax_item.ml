(*

#use"Ocaml_analysis/ocaml_gsyntax_item.ml";;

*)

type t={
  category : Ocaml_gsyntax_category.t;
  name : string;
  interval_for_name : int*int;
  content : string;
  interval_for_content : int*int;  
};;



let make cat nm nm_itv ctnt ctnt_itv=
    {
  		category =cat;
        name =nm;
        interval_for_name =nm_itv;
        content =ctnt;
        interval_for_content =ctnt_itv;  
    };;

(*


 prsr_for_value_making;
       prsr_for_type_making;
       prsr_for_exception_making;
       prsr_for_comment;
       prsr_for_sharp_comment;
       prsr_for_special_sharp;
       prsr_for_module_opener;
       prsr_for_module_ender;
       prsr_for_module_inclusion;
       prsr_for_specialities;
       prsr_for_white;
       
*)       