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
(*
let content=function
     
      (Constant ctok)->Php_constant_token.to_string ctok
     |(Variable s)->s 
     |(Ident s)->s
     |(Comment s)->s
     |(Single_quoted s)->"'"^s^"'"
     |(Double_quoted s)->"\""^s^"\""
     |(Heredoc s)->s
     |(Nowdoc s)->s
     |(Namespacer (b,l,s))->s
     |(External_echo s)->s
     |(Int s)->s
     |(Float s)->s
     |(Char c)->String.make 1 c
     |(End_of_text)->"EOF";;

let short_content x=
   let s=content x in
   if String.length(s)>50
   then "..."
   else s;;
*)

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


let token_category =function
      Constant(ctok)   ->Php_constant_token.token_category ctok
     |Variable      ->Token_category.Variable
     |Ident        ->Token_category.Identifier
     |Comment       ->Token_category.Comment
     |Single_quoted ->Token_category.Single_quoted_string
     |Double_quoted ->Token_category.Double_quoted_string
     |Heredoc       ->Token_category.Heredoc_string
     |Nowdoc        ->Token_category.Nowdoc_string
     |Namespacer    ->Token_category.Namespacer
     |External_echo ->Token_category.External_item
     |Int           ->Token_category.Integer
     |Float         ->Token_category.Floating_number
     |Char          ->Token_category.Character
     |End_of_text      ->Token_category.End_of_text;;
     
  
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



