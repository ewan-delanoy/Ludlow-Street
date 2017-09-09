(*

#use"Php_analizer/php_recognize_starting_block.ml";;

*)


let  rsb blckr_name=
   let (left_blocker,right_blocker)=Php_blocker_name.token_pair blckr_name in
  ((function l->
     if l=Php_positioned_token_list.empty then None else
     let (a,peurrest)=Php_positioned_token_list.ht l in
     if  Php_positioned_token.fst(a)<>left_blocker
     then None
     else 
     match Php_recognize_block.main (fun x->true) (left_blocker,right_blocker) 1 peurrest 
     with
     None->None
     |Some(((u,last_lxng,others),last_tok))->
        let fst_lxng=fst(Php_positioned_token.snd(a)) in
        Some(Php_char_range.make fst_lxng last_lxng,others)
   ) : Php_recognizer.t);;

