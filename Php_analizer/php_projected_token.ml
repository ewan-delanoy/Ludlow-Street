(*

#use"Php_analizer/php_projected_token.ml";;

*)
type t=
    [
     Php_constant_token.t
    |`Variable 
    |`Ident 
    |`Comment 
    |`Single_quoted 
    |`Double_quoted 
    |`Heredoc 
    |`Nowdoc 
    |`Namespacer 
    |`External_echo 
    |`Int 
    |`Float 
    |`Char 
    |`End_of_text
    ];;




let cchar=(`Char:t);;
let comment=(`Comment:t);;
let constant (x:Php_constant_token.t)=(x:>t);;
let double_quoted=(`Double_quoted:t);;
let external_echo=(`External_echo:t);;
let end_of_text=(`End_of_text:t);;
let ffloat=(`Float:t);;
let heredoc=(`Heredoc:t);;    
let ident=(`Ident:t);;    
let iint=(`Int:t);;
let namespacer=(`Namespacer:t);;
let nowdoc=(`Nowdoc:t);;
let single_quoted=(`Single_quoted:t);;
let variable=(`Variable:t);;


let is_a_comment=function
   `Comment->true
  |_->false;;

let acts_only_once=function
  #Php_constant_token.t | `Comment |`End_of_text ->true
 |_->false;;

let fixture_of_nonconstants=
    [
       variable; 
       ident;
       comment;
       single_quoted;
       double_quoted;
       heredoc;
       nowdoc;
       namespacer;
       external_echo;
       iint;
       ffloat;
       cchar;
    ];;


  
let precedence (ptok:t)=match ptok with
  #Php_constant_token.t as ctok->Php_constant_token.precedence(ctok)
  |_->None;;


let op s=constant(Php_constant_token.c_op(Php_operator.from_visible s));;
let punct s=constant(Php_constant_token.c_punct(Php_punctuator.from_visible s));;
let kwd s=constant(Php_constant_token.c_kwd (Php_keyword.from_visible s));;

let test ctok tok=(tok=constant(ctok));;

let short_name (ptok:t)=match ptok with
#Php_constant_token.t as ctok->Php_constant_token.short_name ctok
|`Variable->"variable"
|`Ident->"id"
|`Comment->"cmt"
|`Single_quoted->"sqs"
|`Double_quoted->"dqs"
|`Heredoc->"heredoc"
|`Nowdoc->"nowdoc"
|`Namespacer->"nmspc"
|`External_echo->"ext"
|`Int->"integer"
|`Float->"float"
|`Char->"chr"
|`End_of_text->"eot";;

let readable (ptok:t)=match ptok with
#Php_constant_token.t as ctok->Php_constant_token.readable ctok
|`Variable->"vvar"
|`Ident->"id"
|`Comment->"cmt"
|`Single_quoted->"sqs"
|`Double_quoted->"dqs"
|`Heredoc->"hdoc"
|`Nowdoc->"ndoc"
|`Namespacer->"nmspc"
|`External_echo->"ext"
|`Int->"int"
|`Float->"float"
|`Char->"char"
|`End_of_text->"eot";;



let order=((
  fun x y->Dictionary_order.dictionary_order 
     (short_name x) (short_name y)
): t Total_ordering.t);;

let temp_pair=
  let temp1=(
    Image.image (fun ctok->constant ctok) Php_constant_token.all
   )
   @
   fixture_of_nonconstants in
   let temp2=Ordered.forget_order(Ordered.diforchan order temp1) in
   (temp2,Image.image (fun ptok->(readable ptok,ptok)) temp2);; 

let all_tokens=fst temp_pair;;
let readables_and_tokens=snd temp_pair;;

  let string_tokens=
    [
      
      variable;
      ident;
      comment;
      single_quoted;
      double_quoted;
      heredoc;
      nowdoc
      
    ];;     
      
 let harmless_tokens=string_tokens@[iint;ffloat];;  
 
 let precedence_neutral_tokens=harmless_tokens@
  (Image.image (fun x->constant(Php_constant_token.c_punct(x))) 
   [Php_punctuator.t_lparenthesis;Php_punctuator.t_rparenthesis]);;
   