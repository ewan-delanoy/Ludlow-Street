(*

#use"Php_analizer/php_token.ml";;

*)


(*
In the type below, `Token(proj,s) we must have
Php_projected_token.make_visible(proj)=Some(s)
whenever the lhs is not None
*)

type t= [`Token of Php_projected_token.t * string];;
   

let form (tok:t)= match tok with `Token(frm,ctnt)->frm;;
let content (tok:t)= match tok with `Token(frm,ctnt)->ctnt;;

let make proj s=(`Token(proj,s):t);;
    
    
(* Constructors *)

let comment s = make Php_projected_token.comment s;;
let constant ctok = 
       make (Php_projected_token.constant(ctok)) 
            (Php_constant_token.make_visible ctok);;
let double_quoted s = make Php_projected_token.double_quoted s;;
let end_of_text = make Php_projected_token.end_of_text "";;
let external_echo s = make Php_projected_token.external_echo s;;
let ident s = make Php_projected_token.ident s;;
let heredoc s = make Php_projected_token.heredoc s;;
let namespacer triple = let s=Code_namespace.encode triple in
                        make Php_projected_token.namespacer s;;
let nowdoc s = make Php_projected_token.nowdoc s;;
let of_char s = make Php_projected_token.cchar s;;
let of_float s = make Php_projected_token.ffloat s;;
let of_int s = make Php_projected_token.iint s;;
let single_quoted s = make Php_projected_token.single_quoted s;;
let variable s = make Php_projected_token.variable s;;

let op s=constant(Php_constant_token.c_op(Php_operator.from_visible s));;
let punct s=constant(Php_constant_token.c_punct(Php_punctuator.from_visible s));;
let kwd s=constant(Php_constant_token.c_kwd (Php_keyword.from_visible s));;


(* end of constructors ¨*)

    let short_content x=
      let s=content x in
      if String.length(s)>50
      then "..."
      else s;;
   
   let is_a_comment x=((form x)=Php_projected_token.comment);;
   
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
   

let from_visible=Memoized.make(fun s->
  try constant(Php_constant_token.from_visible s) with
  Php_constant_token.Unknown_visible(s)->ident(s)
  ));;

