(*

#use"Php_analizer/php_recognize_starting_block.ml";;

*)


let  rsb blckr_name=
   let (left_blocker,right_blocker)=Php_token.pair_for_blocker blckr_name in
  ((function l->
     if l=[] then None else
     let (a,peurrest)=Listennou.ht l in
     if  fst(a)<>left_blocker
     then None
     else 
     match Php_recognize_block.main (fun x->true) (left_blocker,right_blocker) 1 peurrest 
     with
     None->None
     |Some(((u,last_lxng,others),last_tok))->
        let fst_lxng=fst(snd(a)) in
        Some(Php_char_range.make fst_lxng last_lxng,others)
   ) : Php_recognizer.t);;

