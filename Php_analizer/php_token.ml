(*

#use"Php_analizer/php_token.ml";;

*)
type t=
     Constant of Php_constant_token.t
    |Variable of string 
    |Ident of string
    |Comment of string
    |Single_quoted of string
    |Double_quoted of string
    |Heredoc of string
    |Nowdoc of string
    |Namespacer of bool*(string list)*string
    |External_echo of string
    |Int of string
    |Float of string
    |Char of char
    |End_of_text;;

    
let form =function
     (Constant ctok)->Php_projected_token.Constant ctok
    |(Variable s)->Php_projected_token.Variable
    |(Ident s)->Php_projected_token.Ident
    |(Comment s)->Php_projected_token.Comment
    |(Single_quoted s)->Php_projected_token.Single_quoted
    |(Double_quoted s)->Php_projected_token.Double_quoted
    |(Heredoc s)->Php_projected_token.Heredoc
    |(Nowdoc s)->Php_projected_token.Nowdoc
    |(Namespacer (b,l,s))->Php_projected_token.Namespacer
    |(External_echo s)->Php_projected_token.External_echo
    |(Int s)->Php_projected_token.Int
    |(Float s)->Php_projected_token.Float
    |(Char c)->Php_projected_token.Char
    |(End_of_text)->Php_projected_token.End_of_text;;


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



let make proj s=
    match proj with
    |(Php_projected_token.Constant ctok)->Constant ctok
    |Php_projected_token.Variable->Variable s
    |Php_projected_token.Ident->Ident s
    |Php_projected_token.Comment->Comment s
    |Php_projected_token.Single_quoted->Single_quoted s
    |Php_projected_token.Double_quoted->Double_quoted s
    |Php_projected_token.Heredoc->Heredoc s
    |Php_projected_token.Nowdoc->Nowdoc s
    |Php_projected_token.Namespacer->
                        let (b,l,n)=Code_namespace.decode s in
                        Namespacer(b,l,n)
    |Php_projected_token.External_echo->External_echo s
    |Php_projected_token.Int->Int s
    |Php_projected_token.Float->Float s
    |Php_projected_token.Char->Char (String.get s 0)
    |Php_projected_token.End_of_text->End_of_text;;


(* Constructors ¨*)

    let comment s = make Php_projected_token.Comment s;;
    let constant ctok = make (Php_projected_token.Constant(ctok)) "";;
    let double_quoted s = make Php_projected_token.Double_quoted s;;
    let end_of_text s = make Php_projected_token.End_of_text s;;
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
   

let put_lexeme_in_category=Memoized.make(fun s->
  match Php_operator.of_prudent_string s with
   Some(_)->op s
  |None->
  (
   match Php_punctuator.of_prudent_string s with
   Some(_)->punct s
  |None->
   (
    match Php_keyword.of_prudent_string s with
     Some(_)->kwd s
    |None->ident s
   ) 
  ));;
  
let of_string=put_lexeme_in_category;;  
  
let all_constant_strings=
   ( Php_operator.all_strings)
  @( Php_punctuator.all_strings)
  @( Php_keyword.all_strings);;  
  
let nonalphanumeric_lexemes=
  let temp4=List.filter (fun s->
     not(Charset.string_is_alphanumeric s)
    ) all_constant_strings in
  let temp5=Ordered_string.diforchan temp4 in
  let temp6=Ordered.forget_order temp5 in
  List.rev_map (fun s->(s,put_lexeme_in_category s) ) temp6;;
   

let instructions_for_nonalphanumeric_lexemes=
  let temp1=Image.image (
     fun (x,_)->Strung.enclose (Str.global_replace (Str.regexp_string "\n") "\\n" x) 
  ) nonalphanumeric_lexemes in
  let temp2=String.concat "\n  | " temp1 in
  "\n  | "^temp2^" as op {add_to_list lexbuf (read_word op);usual lexbuf}";;
  
   
let give_instructions_for_nonalphanumeric_lexemes ()=
   let s=instructions_for_nonalphanumeric_lexemes 
   and beg_m="(* instructions for nonalphanumeric chars begin here *)"
   and end_m="(* instructions for nonalphanumeric chars end here *)" in
   Replace_inside.overwrite_between_markers_inside_file 
    (Overwriter.of_string s)
    (beg_m,end_m)
    (Absolute_path.of_string "Php_analizer/php_lexer.mll");;

let token_category tok=
      Php_projected_token.token_category(form tok);;
  


let projected_version tok=
    match Php_projected_token.constant_part (form tok) with
     Some(ctok)-> Php_constant_token.to_string ctok
     | None->Token_category.to_string (token_category tok);;
 

let precedence tok=Php_projected_token.precedence(form tok);;




let test ctok tok=(tok=constant(ctok));;



