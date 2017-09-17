(*

#use"Php_analizer/php_named_recognizer.ml";;

Adds names to PHP recognizers.

*)

type t=
  {
    name : string;
    definition : string;
    unnamed_content : Php_constructible_recognizer.t;
    divided : t list;
    is_a_chain : bool;
    is_a_disjunction : bool;
  };;
  
  
module Private=struct  

    exception Name_too_long of string;;
    exception Name_already_in_use of string;; 

    let max_name_length=100;;
    let data=ref(Image.image(
      fun (s,sel)->{
        name =s;
        definition=s;
        unnamed_content=Php_constructible_recognizer.leaf(sel);
        divided=[];
        is_a_chain=false;
        is_a_disjunction=false;
      }
    ) Php_short_selector.readables_and_selectors);; 
    let encode elt=
         (elt.name,(elt.definition,elt.unnamed_content,elt.divided));;
    let order =((
      fun elt1 elt2->
        Total_ordering.for_longest_match_pairs (encode elt1) (encode elt2)
    ) : t Total_ordering.t);;


    let automatic_name_counter=ref(0);;
    
    let new_automatic_name ()=
         let j=(!automatic_name_counter)+1 in
         let _=(automatic_name_counter:=j) in
         "recognizer_"^(string_of_int(j));;

    let compute_name (opt_name,definition)=
        match opt_name with
         Some(nahme)->(if String.length(nahme)>max_name_length
                    then  raise(Name_too_long(nahme))
                    else 
                    if List.exists(fun x->x.name=nahme)(!data)
                    then raise(Name_already_in_use(nahme))
                    else nahme)
        |None->if (String.length(definition)>max_name_length)     
                  ||
                  (List.exists(fun x->x.name=definition)(!data))
               then new_automatic_name ()
               else definition;;     

    let make (opt_name,defn,rcgzr,div)=
         match Option.seek(fun nr->nr.unnamed_content=rcgzr) (!data) with
         Some(nr1)->nr1
         |None->
         let x={
          name =compute_name (opt_name,defn);
          definition=defn;
          unnamed_content=rcgzr;
          divided=div;
          is_a_chain=(Php_constructible_recognizer.chain_content(rcgzr)<>None);
          is_a_disjunction=(Php_constructible_recognizer.disjunction_content(rcgzr)<>None);
         }  in
         let _=(data:=Ordered.insert_plaen order x (!data)) in
         x;; 
    
    let generalized opt_name grlzr nr=
          let (lpar,rpar)=Generalizer.pair grlzr in
          let definition=lpar^(nr.name)^rpar in
          let rcgzr=Php_constructible_recognizer.generalized
             grlzr nr.unnamed_content in  
          make (opt_name,definition,rcgzr,[]);;   
             
    let chain opt_name old_l_nr=
          let temp1=Image.image (
              fun nr->if nr.is_a_chain 
                      then nr.divided
                      else [nr]
          ) old_l_nr in
          let l_nr=List.flatten temp1 in
          if List.length(l_nr)=1
          then List.hd l_nr
          else 
          let definition=String.concat " " (Image.image (fun nr->nr.name) l_nr) in
          let rcgzr=Php_constructible_recognizer.chain
          (Image.image (fun nr->nr.unnamed_content) l_nr) in  
           make (opt_name,definition,rcgzr,l_nr);;   
        
    let disjunction opt_name old_l_nr=
      let temp1=Image.image (
        fun nr->if nr.is_a_disjunction 
                then nr.divided
                else [nr]
      ) old_l_nr in
      let l_nr=List.flatten temp1 in     
      let (lpar,rpar)=Php_symbols_for_recognizer_description.pair_for_disjunction in
      let definition=
        lpar^
        (String.concat Php_symbols_for_recognizer_description.associator_for_disjunction 
          (Image.image (fun nr->nr.name) l_nr))
        ^rpar
      in
      let rcgzr=Php_constructible_recognizer.disjunction
      (Image.image (fun nr->nr.unnamed_content) l_nr) in  
       make (opt_name,definition,rcgzr,l_nr);;   

    exception Unknown_name of string;;

    let of_name nahme=
      try Option.find(fun nr->nr.name=nahme)(!data) with
      Option.Unpackable(_)->raise(Unknown_name(nahme));;

    let of_elementary_definition opt_name defn=
      let names=Image.image (fun nr->nr.name) (!data) in
      let cleaned_defn=Str.global_replace (Str.regexp_string " ") "" defn in
      let temp1=Strung.longest_match_parsing names cleaned_defn in
      let temp2=Image.image of_name temp1 in
      chain opt_name temp2;;
    

    exception Helper_for_definition_reading_exn of ((string*string) option)*string;;
    
    
    let helper_for_definition_reading opt_name (opt,t)=
                let oed=of_elementary_definition in
                if opt=None then oed opt_name t else
                let pair=Option.unpack opt in
                let opt2=Option.seek
                  (fun x->(Generalizer.pair x)=pair)
                  Generalizer.all in
                if opt2<>None 
                then generalized opt_name (Option.unpack opt2) (oed None t) 
                else
                if pair=Php_symbols_for_recognizer_description.pair_for_disjunction
                then 
                     let temp1=Parenthesed_block.decompose_with_associator
                                Php_symbols_for_recognizer_description.associator_for_disjunction 
                                Php_symbols_for_recognizer_description.all_pairs t in
                     disjunction opt_name (Image.image 
                          (oed None) temp1)
                else
                raise(Helper_for_definition_reading_exn(opt,t));; 
    
    exception Empty_output;;
    exception Complicated of string*((((string*string) option)*string) list);; 
        
                
    let of_definition opt_name rough_s=
        let s=Cull_string.trim_spaces rough_s in
        if s="" then raise(Empty_output) else
        let temp1=Parenthesed_block.decompose_without_taking_blanks_into_account 
           Php_symbols_for_recognizer_description.all_pairs s in
        let temp2=Image.image (fun (opt,t)->(opt,Cull_string.trim_spaces t) ) temp1 in
        let temp3=List.filter (fun (opt,t)->t<>"") temp2 in 
        if temp3=[]
        then raise(Empty_output)
        else
        if List.length(temp3)=1
        then helper_for_definition_reading opt_name (List.hd temp3)
        else  
        let temp4=Image.image (helper_for_definition_reading None) temp3 in
        chain opt_name temp4;;            

    let official_defs=ref([]:(string*string) list);;
    
    let make_official_def x y=
        (official_defs:=(x,y)::(!official_defs);
         of_definition (Some x) y);;

    let rec iterator_for_apparition_order (graet,names,da_ober)=
        if da_ober=[]
        then List.flatten(List.rev graet)
        else 
        let tester=(fun nr->
           List.for_all (fun t->
             Ordered_string.elfenn t.name names
           ) nr.divided
        ) in
        let (dead_ones,still_alive)=List.partition tester da_ober in
        let dead_names=Image.image (fun nr->nr.name) dead_ones in
        let updated_names=Ordered_string.teuzin names (Ordered_string.diforchan dead_names) in
        iterator_for_apparition_order (dead_ones::graet,updated_names,still_alive);;

    let _=make_official_def "optional_pblock" "_l_ () _r?_";;
    let _=make_official_def "namespace_name" "_l_ id _u_ nmspc _rd_";;

    let list_for_assignables=
      Image.image (fun (j,s)->("assignable"^(string_of_int j),s)) 
      (Ennig.index_everything(
      [
        "coerce           id ()";
        "nmspc            _l_ :: id _r?_ optional_pblock";
        "id ::            id ()";
        "id () ?: no_semicolon";
        "id () .          sqs";
        "id ()            ";
        "hdoc ";
        "include_like     _l_ loose= _r*_ ";
        "int          ";
        "new id           ()";
        "new nmspc        ()";
        "sqs . vvar . dqs . vvar -> id () . dqs . vvar -> id () . dqs";
        "sqs . vvar . sqs";
        "sqs";
        "vvar [ sqs ]";
        "vvar . sqs";
        "vvar = sqs";
        "vvar -> id optional_pblock _l_ -> id optional_pblock _r*_";
        "vvar + _l_ loose= _r*_ ";
        "vvar";
        "@                id ()";
        "() ?  string_or_var  :  string_or_var  "
      ]));;
    
    let assignables=Image.image (
      fun (nahme,defn)->of_definition (Some(nahme)) defn
    ) list_for_assignables;;
    

    let _ =disjunction (Some"assignable") assignables;;

    let list_for_beheaded_ivies=
      Image.image (fun (j,s)->("beheaded_ivy"^(string_of_int j),s)) 
      (Ennig.index_everything(
      [
        "exit ;";
        "{}   _l_ else if () {} _r*_      else {} ";
      ]));;
    
    let beheaded_ivies=Image.image (
      fun (nahme,defn)->of_definition (Some(nahme)) defn
    ) list_for_beheaded_ivies;;
    

    let _ =disjunction (Some"beheaded_ivy") beheaded_ivies;;
    
    let list_for_beheaded_iwies=
      Image.image (fun (j,s)->("beheaded_iwy"^(string_of_int j),s)) 
      (Ennig.index_everything(
      [
        
        "_l_ no_ivies _r*_ if () : _l_no_ivies _r*_ else : _l_no_ivies _r*_ endif ; _l_no_ivies _r*_";
        "_l_ no_ivies _r*_ if () : _l_no_ivies _r*_ endif ; _l_ no_ivies _r*_";
        "_l_ no_ivies _r*_";
      ]));;
    
    let beheaded_iwies=Image.image (
      fun (nahme,defn)->of_definition (Some(nahme)) defn
    ) list_for_beheaded_iwies;;
    

    let _ =disjunction (Some"beheaded_iwy") beheaded_iwies;;

    let pairs_for_statements=
    [("append_byref", "vvar [ ] = assignable ;");                                                            ("assign_byref", "vvar assign & assignable ;");                                                      
    ("assign_on_servant", "vvar -> id_or_var  =  assignable ;");
    ("assign_on_static", "id :: id_or_var =  assignable ;");
    ("assign_usual", "vvar assign  assignable ;");
    ("cell_assign", "vvar [  int_or_string_or_var  ] = assignable ;");
    ("cell_assign_byref",
     "vvar [  int_or_string_or_var   ]  =   & assignable  ;");
    ("class_abstract", "abstract class _l_ no_left_brace _r*_ {}");
    ("class_final", "final class _l_ no_left_brace _r*_ {}");
    ("class_usual", "class _l_ no_left_brace _r*_ {}");
    ("decl", "declare () ;"); ("echo1", "echo vvar ext");
    ("echo2", "echo _l_ no_semicolon _r*_ ;"); ("exit", "exit ;");
    ("foreach1", "foreach () {}");
    ("foreach2", "foreach () :  _l_ no_breach _r*_  endforeach ;");
    ("fun_call2", "@ id () ;"); ("fun_def", "function id () {}");
    ("fun_returning", "return  function () {} ;");
    ("include_like", "include_like _l_stringy _r*_ ;");
    ("interface_decl", "interface _l_ no_left_brace _r*_ {}");
    ("ivy", "if () beheaded_ivy"); ("iwy", "if () : beheaded_iwy endif ;");
    ("nmspc_long_definition", "namespace nmspc ;");
    ("nmspc_short_definition", "namespace id ;");
    ("nonroot_namespace_use", "namespace  namespace_name {}");
    ("returning", "return _l_ no_semicolon _r*_ ;");
    ("root_namespace_use", "namespace  {}"); ("singleton", "ext");
    ("snake_on_meth_call", " id :: id () _l_ -> id () _r+_  ;");
    ("snake_on_var", "vvar _l_ -> id_or_var optional_pblock  _r+_ ;");
    ("static_assignment", "static vvar assign assignable ;");
    ("static_meth", "id :: id () ;");
    ("static_meth_on_nmspc", "nmspc :: id () ;"); ("switch", "switch () {}");
    ("trait_decl", "trait id {}"); ("trycatch", "try {} catch () {}");
    ("while_loop", "while () {}"); ("yuze_decl", "use _l_ no_semicolon _r*_ ;")];;

    let list_for_statements=
      Image.image (fun (j,s)->("statement"^(string_of_int j),s)) 
      (Ennig.index_everything(
      
        Image.image snd pairs_for_statements
      ));;
    
    let statements=Image.image (
      fun (nahme,defn)->of_definition (Some(nahme)) defn
    ) list_for_statements;;
    

    let _ =disjunction (Some"statement") statements;;

    

end;;


let generalized=Private.generalized;;
let chain=Private.chain;;
let disjunction=Private.disjunction;;

let of_name=Private.of_name;;
let of_definition=Private.of_definition;;


let chain_content nr=
   if Php_constructible_recognizer.chain_content(nr.unnamed_content)=None
   then None  
   else Some(nr.divided);;

let is_constant nr=Php_constructible_recognizer.is_constant 
                    nr.unnamed_content;;

let recognize nr=Php_constructible_recognizer.recognize 
                    nr.unnamed_content;;

let eat_prechewed x l= 
  let temp1=of_definition None x in
  let temp2=temp1.unnamed_content in
  Php_constructible_recognizer.recognize temp2 l;;

let eat x y=
    let temp3=Php_lexer.parse_string ("<?php "^y) in
    eat_prechewed x temp3;;

let data_in_apparition_order ()=
    Private.iterator_for_apparition_order (
      [],Ordered.S[],(!(Private.data))
    );;

let print (x:t)=
    "\xc9\xbe  "^(x.name)^"  \xc9\xbf";;
  
let print_out (fmt:Format.formatter) (x:t)=
     Format.fprintf fmt "@[%s@]" (print x);;


(*

*)
