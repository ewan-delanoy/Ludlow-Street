(*

#use"Remembered/Tests/some_tests.ml";; 

*)

(*

module Test1=struct

*)

let gg s=image Positioned_php_token.unveil 
(Php_lexer.parse_string s);;

let expected (b1,b2)=
(b1.Lexing.pos_cnum = 11)&&(b2.Lexing.pos_cnum = 46);;


let hh s=List.filter(fun (_,b)->
expected b
)(gg s);;


let pre_g0=
[
" ?>What happiness means to me <?php ";
"\"?>What happiness means to me <?php\"";
"'?>What happiness means to me <?php'";
"<<<MARK\n>What happiness means\nMARK;\n";
"<<<'MARK'\n>What happiness mea\nMARK;\n";
"<<<\"MARK\"\n>What happiness mea\nMARK;\n";
"/*>What happiness means to me <?ph*/";
];;

(Tidel.diforchan(image String.length pre_g0))=Ordered.S[36];;

let g0=image (fun s->"<?php 789 "^s^" 8901") pre_g0;;

let g1=List.filter (fun s->hh (s)=[]) g0;;

let s1=(if g0=[] then "" else List.hd(g0));;
let g1=gg s1;;




(*

end;;


*)










