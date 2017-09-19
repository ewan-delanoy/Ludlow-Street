(*

#use"Php_analizer/php_named_recognizer.ml";;

Adds names to PHP recognizers.

*)



type t=
  {
    names : string list;
    definition : string;
    shortened_definition : string option;
    unnamed_content : Php_constructible_recognizer.t;
    elements : t list;
    is_a_chain : bool;
    is_a_disjunction : bool;
  };;
  
  
module Private=struct  

    exception Name_too_long of string;;
    exception Name_already_in_use of string;; 

    let max_name_length=100;;
    let original_data=(Image.image(
      fun (s,sel)->{
        names =[s];
        definition=s;
        shortened_definition=None;
        unnamed_content=Php_constructible_recognizer.leaf(sel);
        elements=[];
        is_a_chain=false;
        is_a_disjunction=false;
      }
    ) Php_short_selector.readables_and_selectors);; 
    let data=ref(original_data);;

    let principal_name nr=List.hd(nr.names);;

    let encode nr=
         (principal_name nr,nr.unnamed_content);;
    let order =((
      fun elt1 elt2->
        Total_ordering.for_longest_match_pairs (encode elt1) (encode elt2)
    ) : t Total_ordering.t);;


    let automatic_name_counter=ref(0);;
    
    let new_automatic_name ()=
         let j=(!automatic_name_counter)+1 in
         let _=(automatic_name_counter:=j) in
         "recognizer_"^(string_of_int(j));;

    let compute_name opt_name=
        match opt_name with
         Some(nahme)->(if String.length(nahme)>max_name_length
                    then  raise(Name_too_long(nahme))
                    else 
                    if List.exists(fun x->List.mem nahme x.names)(!data)
                    then raise(Name_already_in_use(nahme))
                    else nahme)
        |None->new_automatic_name ();;     
      
      let force_add_name_to_element nahme nr =
        let new_nr= 
        {
          names =nr.names@[nahme];
          definition=nr.definition;
          shortened_definition=nr.shortened_definition;
          unnamed_content=nr.unnamed_content;
          elements=nr.elements;
          is_a_chain=nr.is_a_chain;
          is_a_disjunction=nr.is_a_disjunction;
        } in
        let new_data=Image.image (fun y->if y.names=nr.names then new_nr else y) (!data) in
        let _=(data:=new_data) in
        new_nr;;

      let add_name_to_element_if_necessary opt_name nr=
        match opt_name with
        None->nr
       |Some(nahme)->
        if List.mem nahme nr.names 
        then nr
        else force_add_name_to_element nahme nr;;

    let make (opt_name,defn,short_defn,rcgzr,elts)=
         match Option.seek(fun nr->nr.unnamed_content=rcgzr) (!data) with
         Some(nr1)->add_name_to_element_if_necessary opt_name nr1
         |None->
         let x={
          names =[compute_name (opt_name)];
          definition=defn;
          shortened_definition=short_defn;
          unnamed_content=rcgzr;
          elements=elts;
          is_a_chain=(Php_constructible_recognizer.chain_content(rcgzr)<>None);
          is_a_disjunction=(Php_constructible_recognizer.disjunction_content(rcgzr)<>None);
         }  in
         let _=(data:=Ordered.insert_plaen order x (!data)) in
         x;; 
    
    let generalized (opt_name,short_defn) grlzr nr=
          let (lpar,rpar)=Generalizer.pair grlzr in
          let definition=lpar^" "^(principal_name nr)^" "^rpar in
          let rcgzr=Php_constructible_recognizer.generalized
             grlzr nr.unnamed_content in  
          make (opt_name,definition,short_defn,rcgzr,[]);;   
             
    let chain (opt_name,short_defn) old_l_nr=
          let temp1=Image.image (
              fun nr->if nr.is_a_chain 
                      then nr.elements
                      else [nr]
          ) old_l_nr in
          let l_nr=List.flatten temp1 in
          if List.length(l_nr)=1
          then add_name_to_element_if_necessary opt_name (List.hd l_nr)
          else 
          let definition=String.concat " " (Image.image principal_name l_nr) in
          let rcgzr=Php_constructible_recognizer.chain
          (Image.image (fun nr->nr.unnamed_content) l_nr) in  
           make (opt_name,definition,short_defn,rcgzr,l_nr);;   
        
    let disjunction (opt_name,short_defn) old_l_nr=
      let temp1=Image.image (
        fun nr->if nr.is_a_disjunction 
                then nr.elements
                else [nr]
      ) old_l_nr in
      let l_nr=List.flatten temp1 in     
      if List.length(l_nr)=1
      then add_name_to_element_if_necessary opt_name (List.hd l_nr)
      else 
      let (lpar,rpar)=Php_symbols_for_recognizer_description.pair_for_disjunction in
      let definition=
        lpar^
        (String.concat Php_symbols_for_recognizer_description.associator_for_disjunction 
          (Image.image principal_name l_nr))
        ^rpar
      in
      let rcgzr=Php_constructible_recognizer.disjunction
      (Image.image (fun nr->nr.unnamed_content) l_nr) in  
       make (opt_name,definition,short_defn,rcgzr,l_nr);;   

    exception Unknown_name of string;;

    let of_name nahme=
      try Option.find(fun nr->List.mem nahme nr.names)(!data) with
      Option.Unpackable(_)->raise(Unknown_name(nahme));;

    let of_elementary_definition (opt_name,short_defn) defn=
      let names=List.flatten(Image.image (fun nr->nr.names) (!data)) in
      let temp1=Strung.longest_match_parsing names defn in
      let temp2=Image.image of_name temp1 in
      chain (opt_name,short_defn) temp2;;
  
    exception Helper_for_definition_reading_exn of ((string*string) option)*string;;
    
    let helper_for_definition_reading name_options (opt,t)=
                let oed=of_elementary_definition  in
                if opt=None then oed name_options t else
                let pair=Option.unpack opt in
                let opt2=Option.seek
                  (fun x->(Generalizer.pair x)=pair)
                  Generalizer.all in
                if opt2<>None 
                then generalized name_options (Option.unpack opt2) (oed (None,None) t) 
                else
                if pair=Php_symbols_for_recognizer_description.pair_for_disjunction
                then 
                     let temp1=Parenthesed_block.decompose_with_associator
                                Php_symbols_for_recognizer_description.associator_for_disjunction 
                                Php_symbols_for_recognizer_description.all_pairs t in
                     disjunction name_options (Image.image 
                          (oed (None,None))  temp1)
                else
                raise(Helper_for_definition_reading_exn(opt,t));; 
    
    exception Empty_output;;
    exception Complicated of string*((((string*string) option)*string) list);; 
        
                
    let of_definition name_options rough_s=
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
        then helper_for_definition_reading name_options (List.hd temp3)
        else  
        let temp4=Image.image (helper_for_definition_reading (None,None)) temp3 in
        chain name_options temp4;;            

    let rec iterator_for_apparition_order (graet,names,da_ober)=
        if da_ober=[]
        then List.flatten(List.rev graet)
        else 
        let tester=(fun nr->
           List.for_all (fun t->
             Ordered_string.elfenn (principal_name t) names
           ) nr.elements
        ) in
        let (dead_ones,still_alive)=List.partition tester da_ober in
        let dead_names=List.flatten(Image.image (fun nr->nr.names) dead_ones) in
        let updated_names=Ordered_string.teuzin names (Ordered_string.diforchan dead_names) in
        iterator_for_apparition_order (dead_ones::graet,updated_names,still_alive);;
    
    let absorb_spider_item (item_name,l)=
         let n=List.length l in
         if n=1
         then of_definition (Some(item_name),None) (List.hd l)
         else
         let temp1=Ennig.index_everything(l) in
         let temp2=Image.image(fun (j,s)->
              let tj=item_name^"_"^(string_of_int j) in
              of_definition (Some(tj),None) s
         ) temp1 in
         let shortened_def=item_name^"_i (1<=i<="^(string_of_int n)^")" in
         disjunction (Some item_name,Some shortened_def) temp2;;
    


    let absorb_spider  l=Image.image  absorb_spider_item l;;   

    let reset_with_spider l=(data:=original_data;absorb_spider l);;
    
    let reset_with_usual ()=reset_with_spider (Php_spider.php());;

    let add_dependencies x=(Php_spider.add_dependencies x;reset_with_usual ());;
    let remove_dependencies x=(Php_spider.remove_dependencies x;reset_with_usual ());;
    
    let remove_idependencies (x,l)=
      let nr1=of_name x in
      let l1=nr1.elements in
      let l2=Image.image (fun j->
        let nr2=List.nth l1 (j-1) in nr2.definition) l in
      remove_dependencies (x,l2);;   

    let replace_dependencies x l_idx l_name=
      (
        remove_idependencies (x,l_idx);
        add_dependencies (x,l_name)
      );;  


    let analize_item s=
       let temp1=List.assoc s (Php_spider.php()) in
       if List.length (temp1)<2
       then []
       else 
       let temp2=Ennig.index_everything temp1 in
       let temp3=Image.image(
           fun (j,t)->
              let sj=s^"_"^(string_of_int j) in
              let named_rcgzr=of_name sj in
              let rcgzr=named_rcgzr.unnamed_content in
              (t,fst(Php_constructible_recognizer.big_head_tail_decomposition rcgzr))
       ) temp2 in
       let temp4=Uple.list_of_pairs temp3 in
       let temp5=List.filter (
          fun ((_,l1),(_,l2))->Listennou.comparable_for_prefix_order l1 l2
       ) temp4 in
       temp5;;
    
    absorb_spider (Php_spider.php());;
    
end;;



let name=Private.principal_name;;
let of_name=Private.of_name;;
let definition x= match x.shortened_definition with
                  Some(sdef)->sdef
                  |None->x.definition;;
let of_definition=Private.of_definition (None,None);;


let chain_content nr=
   if Php_constructible_recognizer.chain_content(nr.unnamed_content)=None
   then None  
   else Some(nr.elements);;

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
  let temp1=of_definition x in
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
let replace_dependencies=Private.replace_dependencies;;

let analize_item=Private.analize_item;;

let print (x:t)=
    let (nahme,defn)=(name x,definition x) in
    let descr=(if nahme=defn then nahme else nahme^" : "^defn) in
     "\xc9\xbe  "^descr^"  \xc9\xbf";;
  
let print_out (fmt:Format.formatter) (x:t)=
     Format.fprintf fmt "@[%s@]" (print x);;


(*

*)
