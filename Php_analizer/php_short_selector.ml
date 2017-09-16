(*

#use"Php_analizer/php_short_selector.ml";;

*)

type t =                                                                    
         Atomic of Php_projected_token_set.t 
        |Block of Php_blocker.t
        |Unusual_block of Php_blocker.t*int;;
      
let new_pairs=
   [
     "()",Block(Php_blocker.parenthesis);
     "{}",Block(Php_blocker.brace);
     "[]",Block(Php_blocker.bracket);
     "?:",Block(Php_blocker.ternop);
   ];;

let acts_only_once=function
   Atomic(atom_sel)->Php_projected_token_set.acts_only_once atom_sel
  |Block(_)->false
  |Unusual_block(_,_)->false;;

let readables_and_selectors=
   let temp1=
   (
     Image.image (fun (s,ato)->
        (s,Atomic(ato))
     ) Php_projected_token_set.readables_and_toksets
   )
   @
   new_pairs in
   Ordered.diforchan_plaen Total_ordering.for_longest_match_pairs temp1;;

let all_string_constants=Image.image fst readables_and_selectors;;

exception List_from_string_exn of string;;

let list_from_string s=
  try( 
  let temp1=Strung.longest_match_parsing all_string_constants s in
  Image.image (fun a->List.assoc a readables_and_selectors) temp1
  ) with
  _->raise( List_from_string_exn(s));;

exception Unregistered of t;; 
 
let to_string x=try (fst(Option.find (fun p->snd(p)=x) readables_and_selectors)) 
      with 
      _->raise(Unregistered(x));;

exception Unknown of string;;

let optional_of_string s0=match 
   Option.seek (fun (s,sel)->s=s0) readables_and_selectors with
   None->None
   |Some(_,sel)->Some(sel);;
   
let of_string s=match optional_of_string s with
   None->raise(Unknown(s))
  |Some(s)->s;;

let recognize_atomic atomic_sel=
    let f=(function x->
       if x=[] then None else
       let (a,peurrest)=Listennou.ht x in
         if Php_projected_token_set.test atomic_sel (Php_token.form(fst a)) 
         then let (u,v)=snd a in
              Some(Php_char_range.make u v,peurrest)
         else None
    ) in
    (f : Php_recognizer.t);;   

let recognize sel=
   let f=(function
      l->(
        match sel with
         Atomic(atomic_sel)->recognize_atomic atomic_sel l 
        |Block(blckr)->Php_recognize_starting_block.rsb blckr l
        |Unusual_block(blckr,d)->(match (Php_recognize_block.main (fun _->true) 
                                 (Php_token.pair_for_blocker blckr) 
                                 d l) 
                                 with
                                 None->None
                                 |Some(((u,last_lxng,others),last_tok))->
                                    let fst_lxng=fst(snd(List.hd l)) in
                                    Some(Php_char_range.make fst_lxng last_lxng,
                                         others) 
        )                       
      )
   ) in
   (f : Php_recognizer.t);; 
   

let head_tail_decomposition=function   
   Atomic(sel)->[sel,None]
   |Block(blckr)->[Php_projected_token_set.left_blocker blckr,
                    Some(Unusual_block(blckr,1))]
   |Unusual_block(blckr,d)->
           let temp1=(if d=1 
                      then None 
                      else Some(Unusual_block(blckr,d-1))
           ) in
           [
            Php_projected_token_set.left_blocker blckr,
            Some(Unusual_block(blckr,d+1)) ;
            Php_projected_token_set.right_blocker blckr,
            temp1;
            Php_projected_token_set.noneof blckr,
            Some(Unusual_block(blckr,d-1)) ;
            
           ]
     ;;

let nonempty_accepted_word=function   
     Atomic(sel)->[sel]
     |Block(blckr)->[Php_projected_token_set.left_blocker blckr;
                     Php_projected_token_set.right_blocker blckr]
     |Unusual_block(blckr,d)->
             Ennig.doyle (fun _->Php_projected_token_set.right_blocker blckr) 1 d
       ;;
  

(*

let gg x=
   let y=of_string x in
   let z=to_string y in
   let u=of_string z in
   (y,y=u);;

gg "if new instanceof kwd variable";;
gg "  if new instanceof kwd variable";;
gg "deny  if new instanceof kwd variable";;


*)


