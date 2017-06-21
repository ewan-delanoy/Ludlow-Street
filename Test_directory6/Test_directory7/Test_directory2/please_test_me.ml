(*

#use"Haag1/Haag2/please_test_me.ml";;

*)

let h1=
{Positioned_php_token_list.contained=
Image.image
(fun (x,y)->Positioned_php_token.PPL(x,y))
[(Php_token.Constant (Php_constant_token.Kwd Php_keyword.T_ECHO),                                                                                 ({Lexing.pos_fname =                                                                                                                        
       "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Vendor/Symfony/Symfony/Src/Symfony/Component/Templating/Tests/Fixtures/Templates/foo.php";
      pos_lnum = 1; pos_bol = 0; pos_cnum = 7},
     {Lexing.pos_fname =
       "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Vendor/Symfony/Symfony/Src/Symfony/Component/Templating/Tests/Fixtures/Templates/foo.php";
      pos_lnum = 1; pos_bol = 0; pos_cnum = 10}));
   (Php_token.Variable "$foo",
    ({Lexing.pos_fname =
       "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Vendor/Symfony/Symfony/Src/Symfony/Component/Templating/Tests/Fixtures/Templates/foo.php";
      pos_lnum = 1; pos_bol = 0; pos_cnum = 12},
     {Lexing.pos_fname =
       "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Vendor/Symfony/Symfony/Src/Symfony/Component/Templating/Tests/Fixtures/Templates/foo.php";
      pos_lnum = 1; pos_bol = 0; pos_cnum = 15}));
   (Php_token.External_echo "\n",
    ({Lexing.pos_fname =
       "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Vendor/Symfony/Symfony/Src/Symfony/Component/Templating/Tests/Fixtures/Templates/foo.php";
      pos_lnum = 1; pos_bol = 0; pos_cnum = 16},
     {Lexing.pos_fname =
       "/Users/Ewandelanoy/Documents/Sites/Symblog/Symblogproject/Vendor/Symfony/Symfony/Src/Symfony/Component/Templating/Tests/Fixtures/Templates/foo.php";
      pos_lnum = 1; pos_bol = 0; pos_cnum = 19}))]
};;

let h2=Option.find_really (fun x->x.Beaver_for_statement.name="echo2") 
(!(Beaver_for_statement.current_data_list));;

let h3=Beaver_for_statement.classical_parser h2 h1;;






(*

This is a string : 

*)



(*
let some_value=5;;

module Boogie=struct

let b=6;;

module Woogie=struct

let parker=7;;

module Andrew=struct

let d=8;;

let first_user=d+1;;

end;;

let second_user=Andrew.d+2;;

let fleury=9;;

end;;

let third_user=Woogie.Andrew.d+3;;

let burp=10;;

end;;



let fourth_user=Boogie.Woogie.Andrew.d+3;;


let g=48+Boogie.Woogie.parker;;
let h=49+some_value;;

*)

(*

let f x=match x.Ocaml_gsyntax_item.category with                                                                     
  | Ocaml_gsyntax_category.Type
  | Ocaml_gsyntax_category.Exception->(1,x)
  | Ocaml_gsyntax_category.Module_opener->(2,x)
  | Ocaml_gsyntax_category.Module_closer->(3,x)
  | Ocaml_gsyntax_category.Module_inclusion->(4,x);;

module Mood=struct

type mytype= A |B |C |D |E;;

end;;

let f x=match x with Mood.A|Mood.B|Mood.C->(1,x) |Mood.D->(2,Mood.D) |Mood.E->(3,Mood.E);;


module Weak=struct
let e=6;;
end;;

*)

