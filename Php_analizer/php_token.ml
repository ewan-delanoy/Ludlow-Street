(*

#use"Php_analizer/php_token.ml";;

*)



type t=
    {
      form : Php_projected_token.t;
      content : string;
    }

let form tok=tok.form;;
let content tok=tok.content;;

let make proj s=
    {
      form = proj;
      content =s ;
    }
    
(* Constructors *)

let comment s = make Php_projected_token.Comment s;;
let constant ctok = make (Php_projected_token.Constant(ctok)) "";;
let double_quoted s = make Php_projected_token.Double_quoted s;;
let end_of_text = make Php_projected_token.End_of_text "";;
let external_echo s = make Php_projected_token.External_echo s;;
let ident s = make Php_projected_token.Ident s;;
let heredoc s = make Php_projected_token.Heredoc s;;
let namespacer triple = let s=Code_namespace.encode triple in
                        make Php_projected_token.Namespacer s;;
let nowdoc s = make Php_projected_token.Nowdoc s;;
let of_char s = make Php_projected_token.Char s;;
let of_float s = make Php_projected_token.Float s;;
let of_int s = make Php_projected_token.Int s;;
let single_quoted s = make Php_projected_token.Single_quoted s;;
let variable s = make Php_projected_token.Variable s;;

let op s=constant(Php_constant_token.Op(Php_operator.of_string s));;
let punct s=constant(Php_constant_token.Punct(Php_punctuator.of_string s));;
let kwd s=constant(Php_constant_token.Kwd (Php_keyword.of_string s));;


(* end of constructors ¨*)

    let short_content x=
      let s=content x in
      if String.length(s)>50
      then "..."
      else s;;
   
   let is_a_comment x=(form x)=Php_projected_token.Comment;;
   
   let fixture_of_nonconstants=
       [
          variable""; 
          ident"";
          comment"";
          single_quoted"";
          double_quoted"";
          heredoc"";
          nowdoc"";
          namespacer (false,[],"");
          external_echo"";
          of_int "0";
          of_float "0.";
          of_char "0";
       ];;
   

let of_string=Memoized.make(fun s->
  match Option.catch_exception(Php_operator.of_string s) with
   Some(_)->op s
  |None->
  (
   match Option.catch_exception(Php_punctuator.of_string s) with
   Some(_)->punct s
  |None->
   (
    match Option.catch_exception(Php_keyword.of_string s) with
     Some(_)->kwd s
    |None->ident s
   ) 
  ));;

