{
(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2012 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)


module Ast = Ast_ml
module Flag = Flag_parsing_ml



(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* src: http://caml.inria.fr/pub/docs/manual-ocaml/lex.html *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* Parse_ml.tokens will catch this exception and print the position
 * of the offending token using the current state of lexbuf, so
 * no need to add position information here.
 *)
exception Lexical of string

let tok     lexbuf  = 
  Lexing.lexeme lexbuf
let tokinfo lexbuf  = 
  Parse_info.tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

let error s =
  if !Flag.exn_when_lexical_error
  then raise (Lexical (s))
  else 
    if !Flag.verbose_lexing
    then Common.pr2_once ("LEXER: " ^ s)
    else ()

(* ---------------------------------------------------------------------- *)
(* Keywords *)
(* ---------------------------------------------------------------------- *)
(* src: http://caml.inria.fr/pub/docs/manual-ocaml/lex.html *)
let keyword_table = Common.hash_of_list [

  "fun", (fun ii -> Parser_ml.Tfun ii);
  "function", (fun ii -> Parser_ml.Tfunction ii);
  "rec", (fun ii -> Parser_ml.Trec ii);

  "type", (fun ii -> Parser_ml.Ttype ii);
  "of", (fun ii -> Parser_ml.Tof ii);

  "if", (fun ii -> Parser_ml.Tif ii);
  "then", (fun ii -> Parser_ml.Tthen ii);
  "else", (fun ii -> Parser_ml.Telse ii);

  "match", (fun ii -> Parser_ml.Tmatch ii);
  "with", (fun ii -> Parser_ml.Twith ii);
  "when", (fun ii -> Parser_ml.Twhen ii);

  "let", (fun ii -> Parser_ml.Tlet ii);
  "in", (fun ii -> Parser_ml.Tin ii);

  "as", (fun ii -> Parser_ml.Tas ii);

  "try", (fun ii -> Parser_ml.Ttry ii);
  "exception", (fun ii ->Parser_ml. Texception ii);

  "begin", (fun ii -> Parser_ml.Tbegin ii);
  "end", (fun ii -> Parser_ml.Tend ii);

  "for", (fun ii -> Parser_ml.Tfor ii);
  "do", (fun ii -> Parser_ml.Tdo ii);
  "done", (fun ii -> Parser_ml.Tdone ii);
  "downto", (fun ii -> Parser_ml.Tdownto ii);
  "while", (fun ii -> Parser_ml.Twhile ii);
  "to", (fun ii -> Parser_ml.Tto ii);

  "val", (fun ii -> Parser_ml.Tval ii);
  "external", (fun ii -> Parser_ml.Texternal ii);

  "true", (fun ii -> Parser_ml.Ttrue ii);
  "false", (fun ii -> Parser_ml.Tfalse ii);

  "module", (fun ii -> Parser_ml.Tmodule ii);
  "open", (fun ii -> Parser_ml.Topen ii);
  "functor", (fun ii -> Parser_ml.Tfunctor ii);
  "include", (fun ii -> Parser_ml.Tinclude ii);
  "sig", (fun ii -> Parser_ml.Tsig ii);
  "struct", (fun ii -> Parser_ml.Tstruct ii);

  "class", (fun ii -> Parser_ml.Tclass ii);
  "new", (fun ii -> Parser_ml.Tnew ii);
  "inherit", (fun ii -> Parser_ml.Tinherit ii);
  "constraint", (fun ii -> Parser_ml.Tconstraint ii);
  "initializer", (fun ii -> Parser_ml.Tinitializer ii);
  "method", (fun ii -> Parser_ml.Tmethod ii);
  "object", (fun ii -> Parser_ml.Tobject ii);
  "private", (fun ii -> Parser_ml.Tprivate ii);
  "virtual", (fun ii -> Parser_ml.Tvirtual ii);

  "lazy", (fun ii -> Parser_ml.Tlazy ii);
  "mutable", (fun ii -> Parser_ml.Tmutable ii);
  "assert", (fun ii -> Parser_ml.Tassert ii);

   (* used when doing mutually recursive definitions *)
  "and", (fun ii -> Parser_ml.Tand ii);

  (* was INFIXOP3 and INFIXOP4 in original grammar apparently *)
  "or", (fun ii -> Parser_ml.Tor ii);
  "mod", (fun ii -> Parser_ml.Tmod ii);
  "lor", (fun ii -> Parser_ml.Tlor ii);
  "lsl", (fun ii -> Parser_ml.Tlsl ii);
  "lsr", (fun ii -> Parser_ml.Tlsr ii);
  "lxor", (fun ii -> Parser_ml.Tlxor ii);
  "asr", (fun ii -> Parser_ml.Tasr ii);
  "land", (fun ii -> Parser_ml.Tland ii);

  (* "ref" is not a keyword ? it's actually a function returning
   * a {mutable contents = ...}
   *)
]

}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let letter = ['A'-'Z' 'a'-'z']
let digit  = ['0'-'9']
let hexa = digit | ['A' 'F' 'a' 'f']
(* could add ['\n' '\r'], or just use dos2unix on your files *)
let newline = '\n'
let space = [' ' '\t']

let lowerletter = ['a'-'z']
let upperletter = ['A'-'Z']

let ident      = (lowerletter | '_') (letter | digit | '_' | "'")*
let upperident = upperletter (letter | digit | '_' | "'")*
let label_name = (lowerletter | '_') (letter | digit | '_' | "'")*

let operator_char = 
 '!'| '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' 
  | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' 
let prefix_symbol = 
  ('!' | '?' | '~') operator_char*
let infix_symbol = 
  ('=' | '<' | '>' | '@' | '^' | '|'| '&' | '+' | '-' | '*'| '/' | '$'|'%' )
   operator_char*

(*****************************************************************************)
(* Rule token *)
(*****************************************************************************)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)

  (* note: this lexer generate tokens for comments, so you can not
   * give this lexer as-is to the parsing function. You must have an
   * intermediate layer that filter those tokens.
   *)

  | newline { Parser_ml.TCommentNewline (tokinfo lexbuf) }
  | space+ { Parser_ml.TCommentSpace (tokinfo lexbuf) }
  | "(*" { 
      let info = tokinfo lexbuf in 
      let com = comment lexbuf in
      Parser_ml.TComment(info |> Parse_info.tok_add_s com)
    }

  (* ext: fsharp *)
  | "///" [^ '\n']* { Parser_ml.TComment (tokinfo lexbuf) }

  | "#" space* digit+ space* ("\"" [^ '"']* "\"")? 
      { Parser_ml.TCommentMisc (tokinfo lexbuf) }

  (* just doing "#"[^'\n']+  can also match method calls. Would be good
   * to enforce in first column but ocamllex can't do that natively
   *)
  | "#!"[^'\n']+ 
      { Parser_ml.TSharpDirective (tokinfo lexbuf) }

  (* todo: hmmm but could be ambiguous with method call too ? *)
  | "#load" space+ [^'\n']+ 
      { Parser_ml.TSharpDirective (tokinfo lexbuf) }
  | "#directory" space+ [^'\n']+ 
      { Parser_ml.TSharpDirective (tokinfo lexbuf) }

  (* many people use sometimes -pp cpp so let's support simple cpp idioms *)
  | "#if" [^ '\n']* { Parser_ml.TCommentMisc (tokinfo lexbuf) }
  | "#else" [^ '\n']* { Parser_ml.TCommentMisc (tokinfo lexbuf) }
  | "#endif" [^ '\n']* { Parser_ml.TCommentMisc (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)
  (* 
   * !=    #     &     &&    '     (     )     *     +     ,     -
   * -.    ->    .     ..    :     ::    :=    :>    ;     ;;    <
   * <-    =     >     >]    >}    ?     ??    [     [<    [>    [|
   * ]     _     `     {     {<    |     |]    }     ~
   *)

  | "="  { Parser_ml.TEq (tokinfo lexbuf) }

  | "(" { Parser_ml.TOParen(tokinfo lexbuf) }  | ")" { Parser_ml.TCParen(tokinfo lexbuf) }
  | "{" { Parser_ml.TOBrace(tokinfo lexbuf) }  | "}" { Parser_ml.TCBrace(tokinfo lexbuf) }
  | "[" { Parser_ml.TOBracket(tokinfo lexbuf) }  | "]" { Parser_ml.TCBracket(tokinfo lexbuf) }
  | "[<" { Parser_ml.TOBracketLess(tokinfo lexbuf) }  | ">]" { Parser_ml.TGreaterCBracket(tokinfo lexbuf) }
  | "[|" { Parser_ml.TOBracketPipe(tokinfo lexbuf) }  | "|]" { Parser_ml.TPipeCBracket(tokinfo lexbuf) }
  | "{<" { Parser_ml.TOBraceLess(tokinfo lexbuf) }  | ">}" { Parser_ml.TGreaterCBrace(tokinfo lexbuf) }

  | "[>" { Parser_ml.TOBracketGreater(tokinfo lexbuf) }

  | "+" { Parser_ml.TPlus(tokinfo lexbuf) }  | "-" { Parser_ml.TMinus(tokinfo lexbuf) }
  | "<" { Parser_ml.TLess(tokinfo lexbuf) }  | ">" { Parser_ml.TGreater(tokinfo lexbuf) }

  | "!=" { Parser_ml.TBangEq(tokinfo lexbuf) (* INFIXOP0 *) }

  | "#" { Parser_ml.TSharp(tokinfo lexbuf) }
  | "&" { Parser_ml.TAnd(tokinfo lexbuf) }
  | "&&" { Parser_ml.TAndAnd(tokinfo lexbuf) }

  (* also used as beginning of a char *)
  | "'" { Parser_ml.TQuote(tokinfo lexbuf) }

  | "`" { Parser_ml.TBackQuote(tokinfo lexbuf) }

  | "*" { Parser_ml.TStar(tokinfo lexbuf) }
  | "," { Parser_ml.TComma(tokinfo lexbuf) }
  | "->" { Parser_ml.TArrow(tokinfo lexbuf) }
  | "." { Parser_ml.TDot(tokinfo lexbuf) }
  | ".." { Parser_ml.TDotDot(tokinfo lexbuf) }
  | ":" { Parser_ml.TColon(tokinfo lexbuf) }
  | "::" { Parser_ml.TColonColon(tokinfo lexbuf) }
  | ";" { Parser_ml.TSemiColon(tokinfo lexbuf) }  
  | ";;" { Parser_ml.TSemiColonSemiColon(tokinfo lexbuf) }
  | "?" { Parser_ml.TQuestion(tokinfo lexbuf) }
  | "??" { Parser_ml.TQuestionQuestion(tokinfo lexbuf) }
  | "_" { Parser_ml.TUnderscore(tokinfo lexbuf) }
  | "|" { Parser_ml.TPipe(tokinfo lexbuf) }
  | "~" { Parser_ml.TTilde(tokinfo lexbuf) }

  | ":=" { Parser_ml.TAssign (tokinfo lexbuf) }
  | "<-" { Parser_ml.TAssignMutable (tokinfo lexbuf) }

  | ":>" { Parser_ml.TColonGreater(tokinfo lexbuf) }

  (* why -. is mentionned in grammar as a keyword and not +. ? *)
  | "-." { Parser_ml.TMinusDot(tokinfo lexbuf) }

  | "+." { Parser_ml.TPlusDot(tokinfo lexbuf) }

  (* why ! is not mentionned as a keyword ? *)
  | "!" { Parser_ml.TBang(tokinfo lexbuf) }

  | prefix_symbol { Parser_ml.TPrefixOperator (tok lexbuf, tokinfo lexbuf) }
  | infix_symbol { Parser_ml.TInfixOperator (tok lexbuf, tokinfo lexbuf) }
  (* pad: used in js_of_ocaml, not sure why not part of infix_symbol *)
  | "##" { Parser_ml.TInfixOperator (tok lexbuf, tokinfo lexbuf) }

  (* camlp4 reserved: 
   * parser    <<    <:    >>    $     $$    $:
   *)


  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | ident {
      let info = tokinfo lexbuf in
      let s = tok lexbuf in
      match Common2.optionise (fun () -> Hashtbl.find keyword_table s) with
      | Some f -> f info
      | None -> Parser_ml.TLowerIdent (s, info)
    }

  | upperident {
      let s = tok lexbuf in
      Parser_ml.TUpperIdent (s, tokinfo lexbuf)
    }

  | '~' label_name ':' {
      let s = tok lexbuf in
      Parser_ml.TLabelDecl (s, tokinfo lexbuf)
    }
  | '?' label_name ':' {
      let s = tok lexbuf in
      Parser_ml.TOptLabelDecl (s, tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  | '-'? digit 
        (digit | '_')*
  | '-'? ("0x" | "0X") (digit | ['A' 'F' 'a' 'f'])
                       (digit | ['A' 'F' 'a' 'f'] | '_')*
  | '-'? ("0o" | "0O")   ['0'-'7']
                       ( ['0'-'7'] | '_')*
  | '-'? ("0b" | "0B")   ['0'-'1']
                       ( ['0'-'1'] | '_')* 
   {
     let s = tok lexbuf in
     Parser_ml.TInt (s, tokinfo lexbuf)
   }

  | '-'?
    digit (digit | '_')*
    ('.' (digit | '_')*)?
    ( ('e' |'E') ['+' '-']? digit (digit | '_')* )? 
     {
     let s = tok lexbuf in
     Parser_ml.TFloat (s, tokinfo lexbuf)
    }


  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  | '"' {
      (* opti: use Buffer because some autogenerated ml files can
       * contains huge strings
       *)

      let info = tokinfo lexbuf in
      let buf = Buffer.create 100 in
      string buf lexbuf;
      let s = Buffer.contents buf in
      Parser_ml.TString (s, info |> Parse_info.tok_add_s (s ^ "\""))
    }

  (* ----------------------------------------------------------------------- *)
  (* Chars *)
  (* ----------------------------------------------------------------------- *)

  | "'" (_ as c) "'" {
      Parser_ml.TChar (String.make 1 c, tokinfo lexbuf)
    }

  | "'" 
    (
        '\\' ( '\\' | '"' | "'" | 'n' | 't' | 'b' | 'r')
      | '\\' digit digit digit
      | '\\' 'x' hexa hexa
    )
    "'" 
   {
      let s = tok lexbuf in
      Parser_ml.TChar (s, tokinfo lexbuf)
    }

  | "'" "\\" _ {
      error ("unrecognised escape, in token rule:"^tok lexbuf);
      Parser_ml.TUnknown (tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  (* Misc *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF (tokinfo lexbuf) }

  | _ {
      error ("unrecognised symbol, in token rule:"^tok lexbuf);
      Parser_ml.TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)
(* Rule string *)
(*****************************************************************************)
(* we need to use a Buffer to parse strings as lex and yacc autogenerated
 * ml files contains big strings with \\ characters
 *)

and string buf = parse
  | '"'           { Buffer.add_string buf "" }
  (* opti: *)
  | [^ '"' '\\']+ { 
      Buffer.add_string buf (tok lexbuf);
      string buf lexbuf 
    }

  | ("\\" (_ as v)) as x { 
      (* todo: check char ? *)
      (match v with
      | _ -> ()
      );
      Buffer.add_string buf x;
      string buf lexbuf
    }
  | eof { error "WEIRD end of file in double quoted string" }

(*****************************************************************************)
(* Rule comment *)
(*****************************************************************************)
and comment = parse
  | "*)" { tok lexbuf }

  | "(*" { 
      (* in ocaml comments are nestable *)
      let s = comment lexbuf in
      "(*" ^ s ^ comment lexbuf
    }

  (* noteopti: bugfix, need add '(' too *)

  | [^'*''(']+ { let s = tok lexbuf in s ^ comment lexbuf } 
  | "*"     { let s = tok lexbuf in s ^ comment lexbuf }
  | "("     { let s = tok lexbuf in s ^ comment lexbuf }
  | eof { 
      error "end of file in comment";
      "*)"
    }
  | _  { 
      let s = tok lexbuf in
      error ("unrecognised symbol in comment:"^s);
      s ^ comment lexbuf
    }
