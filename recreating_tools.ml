(*

#use"recreating_tools.ml";;

*)



let encode_projtok x=Listennou.find_index x Php_projected_token.all_tokens;;
let decode_projtok i=List.nth Php_projected_token.all_tokens (i-1);;

let encode_tok tok=(encode_projtok (Php_token.form tok),Php_token.content tok);;
let decode_tok (i,s)=Php_token.make (decode_projtok i) s;;

let encode_lexing lx=(lx.Lexing.pos_fname,lx.Lexing.pos_lnum,
                      lx.Lexing.pos_bol,lx.Lexing.pos_cnum);;
let decode_lexing (t,j1,j2,j3)=
     {
       Lexing.pos_fname=t;
       Lexing.pos_lnum=j1;
       Lexing.pos_bol=j2;
       Lexing.pos_cnum=j3;
     };;                      

let encode_postok (tok,(lx1,lx2))=
     let (i,s)=encode_tok tok 
     and (t,j1,j2,j3)=encode_lexing lx1
     and (_,k1,k2,k3)=encode_lexing lx2 in
     (i,s,t,j1,j2,j3,k1,k2,k3);;
     
let decode_postok  (i,s,t,j1,j2,j3,k1,k2,k3)=
     let tok=decode_tok (i,s)
     and lx1=decode_lexing  (t,j1,j2,j3)
     and lx2=decode_lexing  (t,k1,k2,k3) in
     (tok,(lx1,lx2));;

let encode_postok_list=Image.image encode_postok;;
let decode_postok_list=Image.image decode_postok;;

let encode_projtok_set (Php_projected_token_set.N l)=Image.image encode_projtok l;;
let decode_projtok_set l=Php_projected_token_set.N(Image.image decode_projtok l);;

let encode_generalizer blckr=Listennou.find_index blckr Generalizer.all;;
let decode_generalizer k=List.nth Generalizer.all (k-1);;

let encode_blocker blckr=Listennou.find_index blckr Php_blocker.all;;
let decode_blocker k=List.nth Php_blocker.all (k-1);;

let encode_short_sel=function
  Php_short_selector.Atomic(pts)->(1,encode_projtok_set pts,0,0)
| Php_short_selector.Block(blckr)->(2,[],encode_blocker blckr,0)
| Php_short_selector.Unusual_block(blckr,d)->(3,[],encode_blocker blckr,d);;

let decode_short_sel (i_pattern,i_projtok,i_blocker,depth)=
   match i_pattern with
    1->Php_short_selector.Atomic(decode_projtok_set i_projtok)
   |2->Php_short_selector.Block(decode_blocker i_blocker)
   |_->Php_short_selector.Unusual_block(decode_blocker i_blocker,depth);;

let dummy_short_sel=(4,[],0,0);;   

let leveller (old_encode,old_decode)=
  let new_encode=(function
    Php_constructible_recognizer.Leaf (ssel)->(1,encode_short_sel ssel,0,[])  
  | Php_constructible_recognizer.Generalized (grzr,x)->(2,dummy_short_sel,encode_generalizer grzr,[old_encode x])                                      
  | Php_constructible_recognizer.Chain(l)->(3,dummy_short_sel,0,Image.image old_encode l)
  | Php_constructible_recognizer.Disjunction(l)->(4,dummy_short_sel,0,Image.image old_encode l)
  ) and new_decode=(function
  (i_pattern,e_sel,i_grzr,e_older)->
  match i_pattern with
  1->Php_constructible_recognizer.Leaf (decode_short_sel e_sel)
  |2->Php_constructible_recognizer.Generalized (decode_generalizer i_grzr,old_decode (List.hd e_older))
  |3->Php_constructible_recognizer.Chain(Image.image old_decode e_older)                                      
  |_->Php_constructible_recognizer.Disjunction(Image.image old_decode e_older)
  ) in
  (new_encode,new_decode);;  

exception Enc_exn of Php_constructible_recognizer.t;;
exception Dec_exn of Php_constructible_recognizer.t;;

let hank_infty=function
x->raise(Enc_exn(x));;   
let dane_infty=function
x->raise(Dec_exn(x));;   

let (hank1,dane1)=leveller (hank_infty,dane_infty);;
let (encode_cons_rcgzr,decode_cons_rcgzr)=leveller (hank1,dane1);;



   









