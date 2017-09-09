(*

#use"Php_analizer/php_short_selector.ml";;

*)

type t =                                                                    
         Atomic of Php_projected_token_set.t 
        |Block of Php_blocker_name.t
        |Unusual_block of Php_blocker.t;;
      
let new_pairs=
   [
     "()",Block(Php_blocker_name.parenthesis);
     "{}",Block(Php_blocker_name.brace);
     "[]",Block(Php_blocker_name.bracket);
   ];;

let acts_only_once=function
   Atomic(atom_sel)->Php_projected_token_set.acts_only_once atom_sel
  |Block(_)->false
  |Unusual_block(_)->false;;

let all_pairs=
   let temp1=
   (
     Image.image (fun (s,ato)->
        (s,Atomic(ato))
     ) Php_projected_token_set.all_pairs
   )
   @
   new_pairs in
   let temp2=Image.image (fun (s,ato)->(-(String.length s),(s,ato)) ) temp1 in
   let temp3=Tidel2.diforchan temp2 in
   Tidel2.image snd temp3;;

let all_string_constants=Image.image fst all_pairs;;

exception List_from_string_exn of string;;

let list_from_string s=
  try( 
  let temp1=Strung.longest_match_parsing all_string_constants s in
  Image.image (fun a->List.assoc a all_pairs) temp1
  ) with
  _->raise( List_from_string_exn(s));;

exception Unregistered of t;; 
 
let to_string x=try (fst(Option.find_really (fun p->snd(p)=x) all_pairs)) 
      with 
      _->raise(Unregistered(x));;

exception Unknown of string;;

let optional_of_string s0=match 
   Option.find_it (fun (s,sel)->s=s0) all_pairs with
   None->None
   |Some(_,sel)->Some(sel);;
   
let of_string s=match optional_of_string s with
   None->raise(Unknown(s))
  |Some(s)->s;;

   let recognize_atomic atomic_sel=
    let f=(function x->
       if x=Php_positioned_token_list.empty then None else
       let (a,peurrest)=Php_positioned_token_list.ht x in
         if Php_projected_token_set.test atomic_sel (Php_token.form(Php_positioned_token.fst a)) 
         then let (u,v)=Php_positioned_token.snd a in
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
        |Unusual_block(blckr)->(match (Php_recognize_block.main (fun _->true) 
                                 (Php_blocker.token_pair blckr) 
                                 (Php_blocker.depth blckr) l) 
                                 with
                                 None->None
                                 |Some(((u,last_lxng,others),last_tok))->
                                    let fst_lxng=fst(Php_positioned_token.snd(Php_positioned_token_list.hd l)) in
                                    Some(Php_char_range.make fst_lxng last_lxng,others) 
        )                       
      )
   ) in
   (f : Php_recognizer.t);; 
   


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


