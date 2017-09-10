(*

#use"Php_analizer/php_named_recognizer.ml";;

Adds names to PHP recognizers.

*)

type t=
  {
    name : string;
    definition : string;
    unnamed_content : Php_constructible_recognizer.t;
  };;
  
  
module Private=struct  

    exception Name_too_long of string;;
    exception Name_already_in_use of string;; 

    let max_name_length=100;;
    let data=ref(Image.image(
      fun (s,sel)->{
        name =s;
        definition=s;
        unnamed_content=Php_constructible_recognizer.Leaf(sel);
      }
    ) Php_short_selector.readables_and_selectors);; 
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

    let make (opt_name,defn,rcgzr)=
         let x={
          name =compute_name (opt_name,defn);
          definition=defn;
          unnamed_content=rcgzr;
         }  in
         let _=(data:=(x::(!data))) in
         x;; 
    
    let generalized opt_name grlzr nr=
          let (lpar,rpar)=Generalizer.pair grlzr in
          let definition=lpar^(nr.name)^rpar in
          let rcgzr=Php_constructible_recognizer.Generalized
             (grlzr,nr.unnamed_content) in  
          make (opt_name,definition,rcgzr);;   
             
    let chain opt_name l_nr=
          let definition=String.concat " " (Image.image (fun nr->nr.name) l_nr) in
          let rcgzr=Php_constructible_recognizer.Chain
          (Image.image (fun nr->nr.unnamed_content) l_nr) in  
           make (opt_name,definition,rcgzr);;   
        
    let disjunction opt_name l_nr=     
      let (lpar,rpar)=Php_constructible_recognizer.pair_for_disjunction in
      let definition=
        lpar^
        (String.concat Php_constructible_recognizer.associator_for_disjunction 
          (Image.image (fun nr->nr.name) l_nr))
        ^rpar
      in
      let rcgzr=Php_constructible_recognizer.Chain
      (Image.image (fun nr->nr.unnamed_content) l_nr) in  
       make (opt_name,definition,rcgzr);;   

    exception Unknown_name of string;;

    let of_name nahme=
      try Option.find(fun nr->nr.name=nahme)(!data) with
      Option.Unpackable(_)->raise(Unknown_name(nahme));;

    let of_elementary_definition opt_name defn=
      let names=Image.image (fun nr->nr.name) (!data) in
      let temp1=Strung.longest_match_parsing names defn in
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
                if pair=Php_constructible_recognizer.pair_for_disjunction
                then 
                     let temp1=Parenthesed_block.decompose_with_associator
                                Php_constructible_recognizer.associator_for_disjunction 
                                Php_constructible_recognizer.all_pairs t in
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
           Php_constructible_recognizer.all_pairs s in
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

end;;


let generalized=Private.generalized;;
let chain=Private.chain;;
let disjunction=Private.disjunction;;

let of_name=Private.of_name;;
let of_definition=Private.of_definition;;

(*
let _=of_definition (Some("statmeth")) " :: id ";; 
let _=of_definition (Some("optional_statmeth")) "_l_ statmeth _r?_";; 
*)

let _=of_definition (Some("optional_pblock")) "_l_ () _r?_";;

let list_for_assignables=
  Image.image (fun (j,s)->("assignable"^(string_of_int j),s)) 
  (Ennig.index_everything(
  [
    "coerce           id ()";
    "nmspc            _l_ :: id _r?_ optional_pblock";
    "id ::            id ()";
    "id () ?          _l_ no_ternary _r+_ : no_semicolon";
    "id () .          sqs";
    "id ()            ";
    "hdoc ";
    "include_like     _l_ loose= _r*_ ";
    "int          ";
    "new id           ()";
    "new nmspc        ()";
    "sqs .            vvar . sqs";
    "sqs";
    "vvar .       sqs";
    "vvar =       sqs";
    "vvar ->      id optional_pblock _l_ -> id optional_pblock _r*_";
    "vvar +       _l_ loose= _r*_ ";
    "vvar";
    "@                id ()";
  ]));;



let assignables=Image.image (
  fun (nahme,defn)->Private.of_definition (Some(nahme)) defn
) list_for_assignables;;

let assignable =disjunction (Some"assignable") assignables;;


let print (x:t)=
    "\xc9\xbe  "^(x.name)^"  \xc9\xbf";;
  
let print_out (fmt:Format.formatter) (x:t)=
     Format.fprintf fmt "@[%s@]" (print x);;


(*

*)
