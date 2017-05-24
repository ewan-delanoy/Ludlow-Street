(*

#use"Php_analizer/termite.ml";;

A termite object is the first decomposing a list of tokens
during the parsing process.
Each termite scans according to just one predefined pattern.
This beginning is decomposed into several intervals, some of
which are retained (for further analysis later) and other which
are discarded (because they correspond to expected keywords in the
pattern).
A termite scanning according to pattern if()then{} for example,
will retain the () and {}'s contents and discard the "if" and the "then".
 

*)

type t=Trmt of (Glued_or_not.t*Old_php_constructible_recognizer.t) list;;

let left_paren_for_retaining="##(";;
let right_paren_for_retaining=")##";;

let parens_for_retaining=(left_paren_for_retaining,right_paren_for_retaining);;

let default_embedding wh=
   if Old_php_constructible_recognizer.is_constant wh
   then (Glued_or_not.Not_retained_not_glued,wh)
   else (Glued_or_not.Retained_not_glued,wh);;

let rewriter (opt,t)=
      let better_t=Cull_string.trim_spaces t in
      if better_t="" then [] else
      let wh=Old_php_constructible_recognizer.of_string better_t in
      if opt<>None 
      then [Glued_or_not.Glued,wh]
      else (
                match Old_php_constructible_recognizer.chain_content wh with
                 None->[default_embedding wh]
                |Some(l)->
                  Image.image default_embedding l                
           
            );;
            
let of_string s=
   let temp1=Parenthesed_block.decompose_without_taking_blanks_into_account 
     [parens_for_retaining] 
     (Cull_string.trim_spaces s) in
   let temp2=Image.image rewriter temp1 in
   let temp3=List.flatten temp2 in
   Trmt(temp3);;
   
exception Stepper_for_parsing_exn;;

 
let rec iterator_for_parsing (graet,da_ober,lexings,l)=
  match da_ober with
  []->let temp1=List.rev_map Php_char_range.fst lexings 
      and temp2=Image.image Php_char_range.snd lexings in
      let temp3=List.filter (fun x->x<>Php_char_range.dummy_lexing) temp1
      and temp4=List.filter (fun x->x<>Php_char_range.dummy_lexing) temp2 in
      let u=Php_char_range.select_head temp3
      and v=Php_char_range.select_head temp4 in
      let cr=Php_char_range.make u v in
       Some(List.rev(graet),cr,l)
  |(ret,wh)::da_ober2->
     (
       match Old_php_constructible_recognizer.recognize wh l with
       None->None
       |Some(cr,peurrest)->
          let d=List.length(l)-List.length(peurrest) in
          let part=Listennou.big_head d l in
          let graet2=(if ret=Glued_or_not.Not_retained_not_glued then graet else part::graet) in
          iterator_for_parsing (graet2,da_ober2,cr::lexings,peurrest)
     );;

let parse (Trmt(trmt))=
  let f=(fun l->iterator_for_parsing ([],trmt,[],l) ) in
  (f: Positioned_php_token.t list list Old_php_parser.t);;


let rec iterator_for_reverse_parsing (graet,da_ober,l)=
  match da_ober with
  []->None
  |(ret,wh)::da_ober2->
     (
       match Old_php_constructible_recognizer.recognize wh l with
       None->Some(graet,wh,l,Old_php_constructible_recognizer.reverse_sleepy_parse wh l)
       |Some(cr,peurrest)->
          let d=List.length(l)-List.length(peurrest) in
          let part=Listennou.big_head d l in
          let graet2=(if ret=Glued_or_not.Not_retained_not_glued then graet else part::graet) in
          iterator_for_reverse_parsing (graet2,da_ober2,peurrest)
     );;

let reverse_parse (Trmt(trmt)) l=iterator_for_reverse_parsing ([],trmt,l) ;;

     
   
 
