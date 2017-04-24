(*

#use"isolated_occurences.ml";;

Used to detect mentions of previously defined names in
the same OCaml module.

An occurrence of a substring is isolated when it 
cannot be extended to a meaningful Ocaml name. So we look at
the surrounding characters, on the left and on the right.


*)

module Private=struct

exception Unclear_char of char;;

let rejected_chars=
  [
   	'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';
    'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';
    'u';'v';'w';'x';'y';'z';
    'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';
    'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';
    'U';'V';'W';'X';'Y';'Z';
    '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
    '_';
  ];;

let admitted_chars=
  [
   	'(' ; ')' ; ';' ; ' ' ;'\n';'\r';'=';'<';'>';'+';'*';'/';'-';
  ];;

let test_for_admissiblity c=
   if List.mem c rejected_chars then false else
   if List.mem c admitted_chars then true else
   raise(Unclear_char(c));;

end;;

let isolated_occurrences_of_in substr s=
  let l_substr=String.length substr 
  and n=String.length(s) in
  let small_test=(fun v0 j->
    (j=v0)||(Private.test_for_admissiblity (String.get s j))
  ) in
  let main_test= (
    fun k->
      ( small_test 1 (k-2) )
      &&
      ((String.sub s (k-1) l_substr)=substr) 
      &&
      ( small_test (String.length s) (l_substr+k-1) )
      
  ) in
  Option.filter_and_unpack(
     fun k->
       if main_test k
       then Some(k,k+l_substr-1)
       else None
  ) (Ennig.ennig 2 (n-l_substr));;


   
(*   
   
isolated_occurrences "garfield" 
"let x=garfield in let y=subgarfield and z=garfield2 in";;

*)   