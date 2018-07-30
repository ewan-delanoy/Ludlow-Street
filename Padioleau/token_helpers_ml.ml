(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

(* open Parser_ml *)
module PI = Parse_info

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)

let is_eof = function
  | Parser_ml.EOF _ -> true
  | _ -> false

let is_comment = function
  | Parser_ml.TComment _ | Parser_ml.TCommentSpace _ | Parser_ml.TCommentNewline _ -> true
  | Parser_ml.TCommentMisc _ -> true
  | _ -> false 

(*
let is_just_comment = function
  | Parser_ml.TComment _ -> true
  | _ -> false 
*)

let token_kind_of_tok t =
  match t with
  | Parser_ml.TOBrace _ -> PI.LBrace
  | Parser_ml.TCBrace _ -> PI.RBrace
  | Parser_ml.TOParen _ -> PI.LPar
  | Parser_ml.TCParen _ -> PI.RPar
  | Parser_ml.TComment _ | Parser_ml.TCommentMisc _ -> PI.Esthet PI.Comment
  | Parser_ml.TCommentSpace _ -> PI.Esthet PI.Space
  | Parser_ml.TCommentNewline _ -> PI.Esthet PI.Newline
  | _ -> PI.Other

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)
let info_of_tok = function

  | Parser_ml.TCommentSpace ii -> ii
  | Parser_ml.TCommentNewline ii -> ii
  | Parser_ml.TComment ii -> ii
  | Parser_ml.TCommentMisc ii -> ii
  | Parser_ml.TUnknown ii -> ii
  | Parser_ml.EOF ii -> ii

  | Parser_ml.TSharpDirective ii -> ii

  | Parser_ml.TInt  (_s, ii) -> ii
  | Parser_ml.TFloat  (_s, ii) -> ii
  | Parser_ml.TChar  (_s, ii) -> ii
  | Parser_ml.TString  (_s, ii) -> ii
  | Parser_ml.TLowerIdent  (_s, ii) -> ii
  | Parser_ml.TUpperIdent  (_s, ii) -> ii
  | Parser_ml.TLabelUse  (_s, ii) -> ii
  | Parser_ml.TLabelDecl  (_s, ii) -> ii
  | Parser_ml.TOptLabelUse  (_s, ii) -> ii
  | Parser_ml.TOptLabelDecl  (_s, ii) -> ii

  | Parser_ml.TPrefixOperator (_s, ii) -> ii
  | Parser_ml.TInfixOperator (_s, ii) -> ii

  | Parser_ml.Tfun ii -> ii
  | Parser_ml.Tfunction ii -> ii
  | Parser_ml.Trec ii -> ii
  | Parser_ml.Ttype ii -> ii
  | Parser_ml.Tof ii -> ii
  | Parser_ml.Tif ii -> ii
  | Parser_ml.Tthen ii -> ii
  | Parser_ml.Telse ii -> ii
  | Parser_ml.Tmatch ii -> ii
  | Parser_ml.Twith ii -> ii
  | Parser_ml.Twhen ii -> ii
  | Parser_ml.Tlet ii -> ii
  | Parser_ml.Tin ii -> ii
  | Parser_ml.Tas ii -> ii
  | Parser_ml.Ttry ii -> ii
  | Parser_ml.Texception ii -> ii
  | Parser_ml.Tbegin ii -> ii
  | Parser_ml.Tend ii -> ii
  | Parser_ml.Tfor ii -> ii
  | Parser_ml.Tdo ii -> ii
  | Parser_ml.Tdone ii -> ii
  | Parser_ml.Tdownto ii -> ii
  | Parser_ml.Twhile ii -> ii
  | Parser_ml.Tto ii -> ii
  | Parser_ml.Tval ii -> ii
  | Parser_ml.Texternal ii -> ii
  | Parser_ml.Ttrue ii -> ii
  | Parser_ml.Tfalse ii -> ii
  | Parser_ml.Tmodule ii -> ii
  | Parser_ml.Topen ii -> ii
  | Parser_ml.Tfunctor ii -> ii
  | Parser_ml.Tinclude ii -> ii
  | Parser_ml.Tsig ii -> ii
  | Parser_ml.Tstruct ii -> ii
  | Parser_ml.Tclass ii -> ii
  | Parser_ml.Tnew ii -> ii
  | Parser_ml.Tinherit ii -> ii
  | Parser_ml.Tconstraint ii -> ii
  | Parser_ml.Tinitializer ii -> ii
  | Parser_ml.Tmethod ii -> ii
  | Parser_ml.Tobject ii -> ii
  | Parser_ml.Tprivate ii -> ii
  | Parser_ml.Tvirtual ii -> ii
  | Parser_ml.Tlazy ii -> ii
  | Parser_ml.Tmutable ii -> ii
  | Parser_ml.Tassert ii -> ii
  | Parser_ml.Tand ii -> ii
  | Parser_ml.Tor ii -> ii
  | Parser_ml.Tmod ii -> ii
  | Parser_ml.Tlor ii -> ii
  | Parser_ml.Tlsl ii -> ii
  | Parser_ml.Tlsr ii -> ii
  | Parser_ml.Tlxor ii -> ii
  | Parser_ml.Tasr ii -> ii
  | Parser_ml.Tland ii -> ii
  | Parser_ml.TOParen ii -> ii
  | Parser_ml.TCParen ii -> ii
  | Parser_ml.TOBrace ii -> ii
  | Parser_ml.TCBrace ii -> ii
  | Parser_ml.TOBracket ii -> ii
  | Parser_ml.TCBracket ii -> ii
  | Parser_ml.TOBracketPipe ii -> ii
  | Parser_ml.TPipeCBracket ii -> ii
  | Parser_ml.TOBracketLess ii -> ii
  | Parser_ml.TGreaterCBracket ii -> ii
  | Parser_ml.TOBraceLess ii -> ii
  | Parser_ml.TGreaterCBrace ii -> ii
  | Parser_ml.TOBracketGreater ii -> ii
  | Parser_ml.TColonGreater ii -> ii
  | Parser_ml.TLess ii -> ii
  | Parser_ml.TGreater ii -> ii
  | Parser_ml.TDot ii -> ii
  | Parser_ml.TDotDot ii -> ii
  | Parser_ml.TComma ii -> ii
  | Parser_ml.TEq ii -> ii
  | Parser_ml.TAssign ii -> ii
  | Parser_ml.TAssignMutable ii -> ii
  | Parser_ml.TColon ii -> ii
  | Parser_ml.TColonColon ii -> ii
  | Parser_ml.TBang ii -> ii
  | Parser_ml.TBangEq ii -> ii
  | Parser_ml.TTilde ii -> ii
  | Parser_ml.TPipe ii -> ii
  | Parser_ml.TSemiColon ii -> ii
  | Parser_ml.TSemiColonSemiColon ii -> ii
  | Parser_ml.TQuestion ii -> ii
  | Parser_ml.TQuestionQuestion ii -> ii
  | Parser_ml.TUnderscore ii -> ii
  | Parser_ml.TStar ii -> ii
  | Parser_ml.TArrow ii -> ii
  | Parser_ml.TQuote ii -> ii
  | Parser_ml.TBackQuote ii -> ii
  | Parser_ml.TAnd ii -> ii
  | Parser_ml.TAndAnd ii -> ii
  | Parser_ml.TSharp ii -> ii
  | Parser_ml.TMinusDot ii -> ii
  | Parser_ml.TPlusDot ii -> ii
  | Parser_ml.TPlus ii -> ii
  | Parser_ml.TMinus ii -> ii


let visitor_info_of_tok f = function
  | Parser_ml.TCommentSpace ii -> Parser_ml.TCommentSpace (f ii)
  | Parser_ml.TCommentNewline ii -> Parser_ml.TCommentNewline (f ii)
  | Parser_ml.TComment ii -> Parser_ml.TComment (f ii)
  | Parser_ml.TCommentMisc ii -> Parser_ml.TCommentMisc (f ii)
  | Parser_ml.TUnknown ii -> Parser_ml.TUnknown (f ii)
  | EOF (ii) -> EOF (f ii)

  | Parser_ml.TSharpDirective ii -> Parser_ml.TSharpDirective (f ii)

  | Parser_ml.TInt (s, ii) -> Parser_ml.TInt (s, f ii)
  | Parser_ml.TFloat (s, ii) -> Parser_ml.TFloat (s, f ii)
  | Parser_ml.TChar (s, ii) -> Parser_ml.TChar (s, f ii)
  | Parser_ml.TString (s, ii) -> Parser_ml.TString (s, f ii)
  | Parser_ml.TLowerIdent (s, ii) -> Parser_ml.TLowerIdent (s, f ii)
  | Parser_ml.TUpperIdent (s, ii) -> Parser_ml.TUpperIdent (s, f ii)
  | Parser_ml.TLabelUse (s, ii) -> Parser_ml.TLabelUse (s, f ii)
  | Parser_ml.TLabelDecl (s, ii) -> Parser_ml.TLabelDecl (s, f ii)
  | Parser_ml.TOptLabelUse (s, ii) -> Parser_ml.TOptLabelUse (s, f ii)
  | Parser_ml.TOptLabelDecl (s, ii) -> Parser_ml.TOptLabelDecl (s, f ii)
  | Parser_ml.TPrefixOperator (s, ii) -> Parser_ml.TPrefixOperator (s, f ii)
  | Parser_ml.TInfixOperator (s, ii) -> Parser_ml.TInfixOperator (s, f ii)

  | Parser_ml.Tfun (ii) -> Parser_ml.Tfun (f ii)
  | Parser_ml.Tfunction (ii) -> Parser_ml.Tfunction (f ii)
  | Parser_ml.Trec (ii) -> Parser_ml.Trec (f ii)
  | Parser_ml.Ttype (ii) -> Parser_ml.Ttype (f ii)
  | Parser_ml.Tof (ii) -> Parser_ml.Tof (f ii)
  | Parser_ml.Tif (ii) -> Parser_ml.Tif (f ii)
  | Parser_ml.Tthen (ii) -> Parser_ml.Tthen (f ii)
  | Parser_ml.Telse (ii) -> Parser_ml.Telse (f ii)
  | Parser_ml.Tmatch (ii) -> Parser_ml.Tmatch (f ii)
  | Parser_ml.Twith (ii) -> Parser_ml.Twith (f ii)
  | Parser_ml.Twhen (ii) -> Parser_ml.Twhen (f ii)
  | Parser_ml.Tlet (ii) -> Parser_ml.Tlet (f ii)
  | Parser_ml.Tin (ii) -> Parser_ml.Tin (f ii)
  | Parser_ml.Tas (ii) -> Parser_ml.Tas (f ii)
  | Parser_ml.Ttry (ii) -> Parser_ml.Ttry (f ii)
  | Parser_ml.Texception (ii) -> Parser_ml.Texception (f ii)
  | Parser_ml.Tbegin (ii) -> Parser_ml.Tbegin (f ii)
  | Parser_ml.Tend (ii) -> Parser_ml.Tend (f ii)
  | Parser_ml.Tfor (ii) -> Parser_ml.Tfor (f ii)
  | Parser_ml.Tdo (ii) -> Parser_ml.Tdo (f ii)
  | Parser_ml.Tdone (ii) -> Parser_ml.Tdone (f ii)
  | Parser_ml.Tdownto (ii) -> Parser_ml.Tdownto (f ii)
  | Parser_ml.Twhile (ii) -> Parser_ml.Twhile (f ii)
  | Parser_ml.Tto (ii) -> Parser_ml.Tto (f ii)
  | Parser_ml.Tval (ii) -> Parser_ml.Tval (f ii)
  | Parser_ml.Texternal (ii) -> Parser_ml.Texternal (f ii)
  | Parser_ml.Ttrue (ii) -> Parser_ml.Ttrue (f ii)
  | Parser_ml.Tfalse (ii) -> Parser_ml.Tfalse (f ii)
  | Parser_ml.Tmodule (ii) -> Parser_ml.Tmodule (f ii)
  | Parser_ml.Topen (ii) -> Parser_ml.Topen (f ii)
  | Parser_ml.Tfunctor (ii) -> Parser_ml.Tfunctor (f ii)
  | Parser_ml.Tinclude (ii) -> Parser_ml.Tinclude (f ii)
  | Parser_ml.Tsig (ii) -> Parser_ml.Tsig (f ii)
  | Parser_ml.Tstruct (ii) -> Parser_ml.Tstruct (f ii)
  | Parser_ml.Tclass (ii) -> Parser_ml.Tclass (f ii)
  | Parser_ml.Tnew (ii) -> Parser_ml.Tnew (f ii)
  | Parser_ml.Tinherit (ii) -> Parser_ml.Tinherit (f ii)
  | Parser_ml.Tconstraint (ii) -> Parser_ml.Tconstraint (f ii)
  | Parser_ml.Tinitializer (ii) -> Parser_ml.Tinitializer (f ii)
  | Parser_ml.Tmethod (ii) -> Parser_ml.Tmethod (f ii)
  | Parser_ml.Tobject (ii) -> Parser_ml.Tobject (f ii)
  | Parser_ml.Tprivate (ii) -> Parser_ml.Tprivate (f ii)
  | Parser_ml.Tvirtual (ii) -> Parser_ml.Tvirtual (f ii)
  | Parser_ml.Tlazy (ii) -> Parser_ml.Tlazy (f ii)
  | Parser_ml.Tmutable (ii) -> Parser_ml.Tmutable (f ii)
  | Parser_ml.Tassert (ii) -> Parser_ml.Tassert (f ii)
  | Parser_ml.Tand (ii) -> Parser_ml.Tand (f ii)
  | Parser_ml.Tor (ii) -> Parser_ml.Tor (f ii)
  | Parser_ml.Tmod (ii) -> Parser_ml.Tmod (f ii)
  | Parser_ml.Tlor (ii) -> Parser_ml.Tlor (f ii)
  | Parser_ml.Tlsl (ii) -> Parser_ml.Tlsl (f ii)
  | Parser_ml.Tlsr (ii) -> Parser_ml.Tlsr (f ii)
  | Parser_ml.Tlxor (ii) -> Parser_ml.Tlxor (f ii)
  | Parser_ml.Tasr (ii) -> Parser_ml.Tasr (f ii)
  | Parser_ml.Tland (ii) -> Parser_ml.Tland (f ii)
  | Parser_ml.TOParen (ii) -> Parser_ml.TOParen (f ii)
  | Parser_ml.TCParen (ii) -> Parser_ml.TCParen (f ii)
  | Parser_ml.TOBrace (ii) -> Parser_ml.TOBrace (f ii)
  | Parser_ml.TCBrace (ii) -> Parser_ml.TCBrace (f ii)
  | Parser_ml.TOBracket (ii) -> Parser_ml.TOBracket (f ii)
  | Parser_ml.TCBracket (ii) -> Parser_ml.TCBracket (f ii)
  | Parser_ml.TOBracketPipe (ii) -> Parser_ml.TOBracketPipe (f ii)
  | Parser_ml.TPipeCBracket (ii) -> Parser_ml.TPipeCBracket (f ii)
  | Parser_ml.TOBracketLess (ii) -> Parser_ml.TOBracketLess (f ii)
  | Parser_ml.TGreaterCBracket (ii) -> Parser_ml.TGreaterCBracket (f ii)
  | Parser_ml.TOBraceLess (ii) -> Parser_ml.TOBraceLess (f ii)
  | Parser_ml.TGreaterCBrace (ii) -> Parser_ml.TGreaterCBrace (f ii)
  | Parser_ml.TOBracketGreater (ii) -> Parser_ml.TOBracketGreater (f ii)
  | Parser_ml.TColonGreater (ii) -> Parser_ml.TColonGreater (f ii)
  | Parser_ml.TLess (ii) -> Parser_ml.TLess (f ii)
  | Parser_ml.TGreater (ii) -> Parser_ml.TGreater (f ii)
  | Parser_ml.TDot (ii) -> Parser_ml.TDot (f ii)
  | Parser_ml.TDotDot (ii) -> Parser_ml.TDotDot (f ii)
  | Parser_ml.TComma (ii) -> Parser_ml.TComma (f ii)
  | Parser_ml.TEq (ii) -> Parser_ml.TEq (f ii)
  | Parser_ml.TAssign (ii) -> Parser_ml.TAssign (f ii)
  | Parser_ml.TAssignMutable (ii) -> Parser_ml.TAssignMutable (f ii)
  | Parser_ml.TColon (ii) -> Parser_ml.TColon (f ii)
  | Parser_ml.TColonColon (ii) -> Parser_ml.TColonColon (f ii)
  | Parser_ml.TBang (ii) -> Parser_ml.TBang (f ii)
  | Parser_ml.TBangEq (ii) -> Parser_ml.TBangEq (f ii)
  | Parser_ml.TTilde (ii) -> Parser_ml.TTilde (f ii)
  | Parser_ml.TPipe (ii) -> Parser_ml.TPipe (f ii)
  | Parser_ml.TSemiColon (ii) -> Parser_ml.TSemiColon (f ii)
  | Parser_ml.TSemiColonSemiColon (ii) -> Parser_ml.TSemiColonSemiColon (f ii)
  | Parser_ml.TQuestion (ii) -> Parser_ml.TQuestion (f ii)
  | Parser_ml.TQuestionQuestion (ii) -> Parser_ml.TQuestionQuestion (f ii)
  | Parser_ml.TUnderscore (ii) -> Parser_ml.TUnderscore (f ii)
  | Parser_ml.TStar (ii) -> Parser_ml.TStar (f ii)
  | Parser_ml.TArrow (ii) -> Parser_ml.TArrow (f ii)
  | Parser_ml.TQuote (ii) -> Parser_ml.TQuote (f ii)
  | Parser_ml.TBackQuote (ii) -> Parser_ml.TBackQuote (f ii)
  | Parser_ml.TAnd (ii) -> Parser_ml.TAnd (f ii)
  | Parser_ml.TAndAnd (ii) -> Parser_ml.TAndAnd (f ii)
  | Parser_ml.TSharp (ii) -> Parser_ml.TSharp (f ii)
  | Parser_ml.TMinusDot (ii) -> Parser_ml.TMinusDot (f ii)
  | Parser_ml.TPlusDot (ii) -> Parser_ml.TPlusDot (f ii)
  | Parser_ml.TPlus (ii) -> Parser_ml.TPlus (f ii)
  | Parser_ml.TMinus (ii) -> Parser_ml.TMinus (f ii)

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let line_of_tok tok = 
  let info = info_of_tok tok in
  PI.line_of_info info
