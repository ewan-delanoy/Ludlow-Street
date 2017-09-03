(*

#use"recreating_tools.ml";;

*)


let encode_ctoken=function
   Php_constant_token.Kwd(kwd)->(Some(kwd),None,None)
  |Php_constant_token.Punct(pct)->(None,Some(pct),None)
  |Php_constant_token.Op(op)->(None,None,Some(op));;

exception Decode_ctoken_exn;;

let decode_ctoken (opt1,opt2,opt3)=
   if opt1<>None
   then Php_constant_token.Kwd(Option.unpack opt1)
   else 
   if opt2<>None
   then Php_constant_token.Punct(Option.unpack opt2)
   else 
   if opt3<>None
   then Php_constant_token.Op(Option.unpack opt3)
   else raise(Decode_ctoken_exn);;  

let encode_ptoken ptok=match ptok with
  Php_projected_token.Constant(ctok)->(encode_ctoken ctok,Php_projected_token.Int)
  |_->((None,None,None),ptok);; 

let decode_ptoken (w,ptok1)=
    if w=(None,None,None)
    then ptok1
    else Php_projected_token.Constant(decode_ctoken w);;  

let encode_token tok=
    (encode_ptoken (tok.Php_token.form),tok.Php_token.content);;

let decode_token (frm,ctnt)=
   {
    Php_token.form=decode_ptoken(frm);
    Php_token.content=ctnt;
   };;
   
let encode_postoken (Positioned_php_token.PPL(x,y))=
    (encode_token x,y);;

let decode_postoken (x1,y)=
  (Positioned_php_token.PPL(decode_token x1,y));;
      
let encode_postokenlist x=
    Image.image encode_postoken
    (x.Positioned_php_token_list.contained);;

let decode_postokenlist l=  
  { Positioned_php_token_list.contained=
     Image.image decode_postoken l };;














