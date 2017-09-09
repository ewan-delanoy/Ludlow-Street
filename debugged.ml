(*

#use"debugged.ml";;

*)

(*
let u1=Manage_lexed_data.get_data
["symblog";"phpbb"];;

let u2=Explicit.image Level_one.level_one u1;;

let u3=Debugging_tools.image Level_one.level_one u1;;



let u3=List.nth u1 1;;

let bad1=Level_one.level_one u3;;

let u4=Debugging_tools.star Beaver_for_statement.parser u3;;

let u5=Positioned_php_token_list.big_head 7 u4;;
let cu5=Recreating_tools.encode_postokenlist u5;;
*)

(*

let cu5=
  [((((None, None, None), Php_projected_token.variable), "$loader"),
  ({Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 918},
   {Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 924}));
 ((((None, None, Some Php_operator.t_equals), Php_projected_token.iint), ""),
  ({Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 926},
   {Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 926}));
 ((((Some Php_keyword.t_require, None, None), Php_projected_token.iint), ""),
  ({Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 928},
   {Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 934}));
 ((((None, None, None), Php_projected_token.ident), "__DIR__"),
  ({Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 936},
   {Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 942}));
 ((((None, None, Some Php_operator.t_dot), Php_projected_token.iint), ""),
  ({Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 943},
   {Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 943}));
 ((((None, None, None), Php_projected_token.single_quoted),
   "/../app/autoload.php"),
  ({Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 944},
   {Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 965}));
 ((((None, Some Php_punctuator.t_semicolon, None), Php_projected_token.iint),
   ""),
  ({Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 966},
   {Lexing.pos_fname =
     "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
    pos_lnum = 15; pos_bol = 917; pos_cnum = 966}))];;

let u5=Recreating_tools.decode_postokenlist cu5;;

let bad2=Beaver_for_statement.parser ( u5);;

let (elt1,l1)=List.hd
(!(Beaver_for_statement.bad_cases));;
let trmt=Termite.of_string elt1.unadbriged_content;;
let (Termite.Trmt(l_trmt))=trmt;;

let w1=snd(List.nth l_trmt 0);;
let w2=snd(List.nth l_trmt 1);;
let w3=snd(List.nth l_trmt 2);;
let w4=snd(List.nth l_trmt 3);;

*)

let elt1=Option.find_really(
  fun elt->elt.Beaver_for_statement.name="assign_to_simple"
) (!(Beaver_for_statement.current_data_list));;

let cl1=[((((None, None, None), Php_projected_token.variable), "$loader"),
({Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 918},
 {Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 924}));
((((None, None, Some Php_operator.t_equals), Php_projected_token.iint), ""),
({Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 926},
 {Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 926}));
((((Some Php_keyword.t_require, None, None), Php_projected_token.iint), ""),
({Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 928},
 {Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 934}));
((((None, None, None), Php_projected_token.ident), "__DIR__"),
({Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 936},
 {Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 942}));
((((None, None, Some Php_operator.t_dot), Php_projected_token.iint), ""),
({Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 943},
 {Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 943}));
((((None, None, None), Php_projected_token.single_quoted),
 "/../app/autoload.php"),
({Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 944},
 {Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 965}));
((((None, Some Php_punctuator.t_semicolon, None), Php_projected_token.iint),
 ""),
({Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 966},
 {Lexing.pos_fname =
   "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Web/app_dev.php";
  pos_lnum = 15; pos_bol = 917; pos_cnum = 966}))];;


let l1=Recreating_tools.decode_postokenlist cl1;;  

let opt2=Termite.parse 
(Termite.of_string elt1.unadbriged_content) l1;;

let (l2,cr2,peurrest)=Option.unpack opt2;;
let bad3=elt1.helper l2 cr2;;