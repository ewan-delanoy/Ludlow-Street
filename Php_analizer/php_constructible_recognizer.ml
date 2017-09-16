(*

#use"Php_analizer/php_constructible_recognizer.ml";;

Generic parser for php code.


*)

type t=
   Leaf of Php_short_selector.t
  |Generalized of Generalizer.t*t
  |Chain of t list
  |Disjunction of t list;;

let empty_word_acceptor=Chain[];;  
let leaf sel=Leaf(sel);;
let generalized grlzr x=Generalized(grlzr,x);;  



let chain_content wh=
    match wh  with
     Chain(ch)->Some(ch)
    |_->None;;

let chain l=
    let temp1=Image.image (
        fun x->match chain_content x
        with 
        None->[x]
        |Some(ch)->ch
    ) l in
    let temp2=List.flatten temp1 in
    if List.length(temp2)=1
    then List.hd temp2
    else Chain(temp2);;

    
let disjunction_content wh=
      match wh  with
       Disjunction(ds)->Some(ds)
      |_->None;;
  
  let disjunction l=
      let temp1=Image.image (
          fun x->match disjunction_content x
          with 
          None->[x]
          |Some(ds)->ds
      ) l in
      let temp2=List.flatten temp1 in
      if List.length(temp2)=1
      then List.hd temp2
      else Disjunction(temp2);;

exception Helper_for_string_reading_exn of ((string*string) option)*string;;

let helper_for_string_reading old_f (opt,t)=
       if opt=None then old_f t else
       let pair=Option.unpack opt in
       let opt2=Option.seek
         (fun x->(Generalizer.pair x)=pair)
         Generalizer.all in
       if opt2<>None then Generalized(Option.unpack opt2,old_f t) else
       if pair=Php_symbols_for_recognizer_description.pair_for_disjunction
       then 
            let temp1=Parenthesed_block.decompose_with_associator
            Php_symbols_for_recognizer_description.associator_for_disjunction 
            Php_symbols_for_recognizer_description.all_pairs t in
            disjunction(Image.image old_f temp1)
       else
       raise(Helper_for_string_reading_exn(opt,t));; 



exception Empty_output;;

let rec of_string rough_s=
  let s=Cull_string.trim_spaces rough_s in
  if s="" then raise(Empty_output) else
  let temp1=Parenthesed_block.decompose_without_taking_blanks_into_account 
             Php_symbols_for_recognizer_description.all_pairs s in
  let temp2=Image.image (fun (opt,t)->(opt,Cull_string.trim_spaces t) ) temp1 in
  let temp3=List.filter (fun (opt,t)->t<>"") temp2 in
  if List.length(temp3)>1
  then chain(Image.image (helper_for_string_reading of_string) temp3)
  else 
  let (opt,t)=List.hd temp3 in
  if opt<>None
  then helper_for_string_reading of_string (opt,t)
  else
  let temp5=Php_short_selector.list_from_string t in
  let temp4=Image.image (fun sel->Leaf(sel)) temp5 in
  if List.length(temp4)=1
  then List.hd(temp4)
  else chain(temp4);;



let is_constant wh=
  match wh  with
   Leaf(sel)->Php_short_selector.acts_only_once sel
  |_->false;;



let recognize_selector=Php_short_selector.recognize;;

let recognize_generalized old_f grlz x=Php_recognizer_homomorphism.generalize grlz (old_f x);;

let recognize_chain old_f ch=Php_recognizer_homomorphism.chain (Image.image old_f ch);;

let recognize_disjunction old_f l=Php_recognizer_homomorphism.ordered_disjunction (Image.image old_f l);;

let rec recognize wh l=
  match wh  with
   Leaf(sel)->recognize_selector sel l
  |Generalized(grlz,x)->recognize_generalized recognize grlz x l
  |Chain(ch)->recognize_chain recognize ch l
  |Disjunction(dis)->recognize_disjunction recognize dis l;;

(*  
let nonempty_accepted_word=function
 Leaf(sel)->Some()
|Generalized(grlz,x)->if List.mem grlz [Generalizer.Zero_or_one;Generalizer.Zero_or_more]
                      then true
                      else accepts_empty_word x
|Chain(ch)->List.for_all accepts_empty_word ch
|Disjunction(dis)->List.exists accepts_empty_word dis;; 
*)

let rec accepts_empty_word=function
     Leaf(_)->false
    |Generalized(grlz,x)->if List.mem grlz [Generalizer.Zero_or_one;Generalizer.Zero_or_more]
                          then true
                          else accepts_empty_word x
    |Chain(ch)->List.for_all accepts_empty_word ch
    |Disjunction(dis)->List.exists accepts_empty_word dis;;  



let htd_for_generalized old_f grlz x=
    let (accepts_ew,l)=old_f x in
    match grlz with
    Generalizer.Zero_or_one->(true,l)
    |Generalizer.Zero_or_more->(true,Image.image (fun (a,peurrest)->(a,chain[peurrest;x])) l)
    |Generalizer.One_or_more->(accepts_ew,Image.image (fun (a,peurrest)->(a,chain[peurrest;x])) l);;

let normalize_head_tail_decomposition l=
    let temp1=Image.image fst l in
    let temp2=Php_projected_token_set.generated_algebra temp1 in
    let temp3=Image.image(
      fun (generated,generators)->
      (generated,disjunction(Option.filter_and_unpack (fun (a,peurrest)->
         if Tidel.elfenn a generators
         then Some(peurrest)
         else None
      ) l))
    ) temp2 in
    temp3;;



let rec head_tail_decomposition=function
 Leaf(sel)->let temp1=Php_short_selector.head_tail_decomposition sel in
            (false,Image.image (fun (a,opt)->match opt with None->(a,Chain[]) |Some(sel2)->(a,Leaf(sel2))) temp1) 
|Generalized(grlz,x)->htd_for_generalized head_tail_decomposition grlz x
|Chain(ch)->if ch=[] then (true,[]) else
            let (elt1,peurrest1)=Listennou.ht ch in
            let (aew1,l1)=head_tail_decomposition elt1 in
            let first_part=Image.image (fun (b,peurrest2)->(b,chain (peurrest2::peurrest1))) l1 in
            if not(aew1)
            then (false,first_part)
            else let (aew2,l2)=head_tail_decomposition (Chain(peurrest1)) in
                 (aew2,normalize_head_tail_decomposition (l1@l2))
|Disjunction(dis)->let temp2=Image.image head_tail_decomposition dis in
                   let aew=List.exists fst temp2 in
                   let temp3=List.flatten(Image.image snd temp2) in
                   (aew,normalize_head_tail_decomposition temp3);;  









