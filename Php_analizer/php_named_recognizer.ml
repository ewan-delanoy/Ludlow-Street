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
    let original_data=(Image.image(
      fun (s,sel)->{
        name =s;
        definition=s;
        unnamed_content=Php_constructible_recognizer.leaf(sel);
        divided=[];
        is_a_chain=false;
        is_a_disjunction=false;
      }
    ) Php_short_selector.readables_and_selectors);; 
    let data=ref(original_data);;
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
          let definition=lpar^" "^(nr.name)^" "^rpar in
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
    
    let absorb_spider_item (item_name,l)=
         if List.length l=1
         then make_official_def item_name (List.hd l)
         else
         let temp1=Ennig.index_everything(l) in
         let temp2=Image.image(fun (j,s)->
              let tj=item_name^"_"^(string_of_int j) in
              make_official_def tj s
         ) temp1 in
         disjunction (Some item_name) temp2;;
    


    let absorb_spider  l=Image.image  absorb_spider_item l;;   

    let reset_with_spider l=(data:=original_data;absorb_spider l);;
    
    let reset_with_usual ()=reset_with_spider (Php_spider.php());;

    let add_dependencies x=(Php_spider.add_dependencies x;reset_with_usual ());;
    let remove_dependencies x=(Php_spider.remove_dependencies x;reset_with_usual ());;

    absorb_spider (Php_spider.php());;
    
    let of_official_name old_name=
       let name=(
           match Option.seek (fun (x,y)->x=old_name) official_defs with
           Some(_,y)->y
           |None->old_name
       ) in
       of_name name;;

    let analize_item s=
       let temp1=List.assoc s (Php_spider.php()) in
       if List.length (temp1)<2
       then []
       else 
       let temp2=Ennig.index_everything temp1 in
       let temp3=Image.image(
           fun (j,t)->
              let named_rcgzr=of_official_name(s^"_"^(string_of_int j)) in
              let rcgzr=named_rcgzr.unnamed_content in
              (t,fst(Php_constructible_recognizer.big_head_tail_decomposition rcgzr))
       ) temp2 in
       let temp4=Uple.list_of_pairs temp3 in
       let temp5=List.filter (
          fun ((_,l1),(_,l2))->Listennou.comparable_for_prefix_order l1 l2
       ) temp4 in
       temp5;;
    

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

let basic_parser nr=((function l->
   match recognize nr l with
    None->None
   |Some(cr,peurrest)->Some((),cr,peurrest)
): unit Php_parser.t);;

let star_parser nr=Php_parser_homomorphism.star (basic_parser nr);;

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

let add_dependencies=Private.add_dependencies;;
let remove_dependencies=Private.remove_dependencies;;

let analize_item=Private.analize_item;;

let print (x:t)=
    "\xc9\xbe  "^(x.name)^"  \xc9\xbf";;
  
let print_out (fmt:Format.formatter) (x:t)=
     Format.fprintf fmt "@[%s@]" (print x);;


(*

*)
