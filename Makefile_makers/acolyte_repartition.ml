(*

#use"Makefile_makers/acolyte_repartition.ml";;

*)

let to_string=function
     Acolyte_repartition_t.Mll->"mll"
    |Mly->"mly"
    |Ml_without_mli->"ml_without_mli"
    |Ml_with_mli->"ml_with_mli"
    |Lonely_mli->"lonely_mli";;

let of_string s=List.assoc s
   [
    "mll",Acolyte_repartition_t.Mll;
    "mly",Acolyte_repartition_t.Mly;
    "ml_without_mli",Acolyte_repartition_t.Ml_without_mli;
    "ml_with_mli",Acolyte_repartition_t.Ml_with_mli;
    "lonely_mli",Acolyte_repartition_t.Lonely_mli
   ];;

exception Add_error of Acolyte_repartition_t.t * Ocaml_ending.t ;;

let add_ml=
    function
    Acolyte_repartition_t.Lonely_mli->Acolyte_repartition_t.Ml_with_mli
    |x->raise(Add_error(x,Ocaml_ending.ml));;

let add_mli=
    function
    Acolyte_repartition_t.Ml_with_mli->Acolyte_repartition_t.Ml_with_mli
    |x->raise(Add_error(x,Ocaml_ending.mli));;    

let add_ending x edg=match edg with
     Ocaml_ending.Ml->add_ml x
    |Ocaml_ending.Mli->add_mli x 
    |_->raise(Add_error(x,edg));;   

exception Remove_error of Acolyte_repartition_t.t * Ocaml_ending.t ;;

let remove_ml=
    function
    Acolyte_repartition_t.Ml_with_mli->Acolyte_repartition_t.Lonely_mli
    |x->raise(Remove_error(x,Ocaml_ending.ml));;

let remove_mli=
    function
    Acolyte_repartition_t.Ml_with_mli->Acolyte_repartition_t.Ml_without_mli
    |x->raise(Remove_error(x,Ocaml_ending.mli));;    

let remove_ending x edg=match edg with
     Ocaml_ending.Ml->add_ml x
    |Ocaml_ending.Mli->add_mli x 
    |_->raise(Remove_error(x,edg));;  

let principal_ending=function
    Acolyte_repartition_t.Mll->Ocaml_ending.mll
   |Mly->Ocaml_ending.mly
   |Ml_without_mli->Ocaml_ending.ml
   |Ml_with_mli->Ocaml_ending.ml
   |Lonely_mli->Ocaml_ending.mli;;

let test_ml_presence=function
Acolyte_repartition_t.Ml_without_mli
|Ml_with_mli->true
|_->false;;

let test_mli_presence=function
Acolyte_repartition_t.Ml_with_mli
|Lonely_mli->true
|_->false;;

let test_mll_presence=function
Acolyte_repartition_t.Mll->true
|_->false;;

let test_mly_presence=function
Acolyte_repartition_t.Mll->true
|_->false;;

let from_presences (mlp,mlip,mllp,mlyp)=
     if mllp     then Acolyte_repartition_t.Mll        else
     if mlyp     then Acolyte_repartition_t.Mly        else
     if not(mlp) then Acolyte_repartition_t.Lonely_mli else
     if mlip     then Acolyte_repartition_t.Ml_with_mli
                 else Acolyte_repartition_t.Ml_without_mli;;

let presences=function
    Acolyte_repartition_t.Mll->(false,true,false)
   |Mly->(false,false,true)
   |Ml_without_mli
   |Ml_with_mli->(true,false,false)
   |Lonely_mli->(false,false,false);;

                 







