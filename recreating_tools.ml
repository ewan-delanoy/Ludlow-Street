(*

#use"recreating_tools.ml";;

*)

let encode_projtok x=find_index x Php_projected_token.all_tokens;;
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






     




   









