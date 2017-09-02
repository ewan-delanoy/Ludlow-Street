(*

#use"Php_analizer/php_projected_token.ml";;

*)
type t=
     Constant of Php_constant_token.t
    |Variable 
    |Ident 
    |Comment 
    |Single_quoted 
    |Double_quoted 
    |Heredoc 
    |Nowdoc 
    |Namespacer 
    |External_echo 
    |Int 
    |Float 
    |Char 
    |End_of_text;;

let is_a_comment=function
   Comment->true
  |_->false;;

let fixture_of_nonconstants=
    [
       Variable; 
       Ident;
       Comment;
       Single_quoted;
       Double_quoted;
       Heredoc;
       Nowdoc;
       Namespacer;
       External_echo;
       Int;
       Float;
       Char;
    ];;


  
let precedence=function 
  Constant ctok->(match ctok with
                    (Php_constant_token.Op op)->Some(Php_operator.precedence(op))
                   |_->None
                 )
  |_->None;;


let constant_part=function 
 Constant ctok->Some(ctok)
|_->None;;



let op s=Constant(Php_constant_token.Op(Php_operator.of_string s));;
let punct s=Constant(Php_constant_token.Punct(Php_punctuator.of_string s));;
let kwd s=Constant(Php_constant_token.Kwd (Php_keyword.of_string s));;

let test ctok tok=(tok=Constant(ctok));;

let to_string=function
 Constant(ctok)->Php_constant_token.to_string ctok
|Variable->"variable"
|Ident->"id"
|Comment->"cmt"
|Single_quoted->"sqs"
|Double_quoted->"dqs"
|Heredoc->"heredoc"
|Nowdoc->"nowdoc"
|Namespacer->"nmspc"
|External_echo->"ext"
|Int->"integer"
|Float->"float"
|Char->"chr"
|End_of_text->"eot";;

let all_tokens=
  (
   Image.image (fun ctok->Constant ctok) Php_constant_token.all
  )
  @
  fixture_of_nonconstants;;

let order=((
  fun x y->Dictionary_order.dictionary_order 
     (to_string x) (to_string y)
): t Total_ordering.t);;

  let string_tokens=
    [
      
      Variable;
      Ident;
      Comment;
      Single_quoted;
      Double_quoted;
      Heredoc;
      Nowdoc
      
    ];;     
      
 let harmless_tokens=string_tokens@[Int;Float];;  