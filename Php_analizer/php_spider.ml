(*

#use"Php_analizer/php_spider.ml";;

Partial grammar definition, with no recursion
(but with references to previously defined terminals. )

*)

module Private=struct

type t=Sp of (string*(string list)) list;;

let unveil (Sp l)=l;;

(* Definition of PHP spider begins here *)

let php_ref=ref(Sp [

  "optional_pblock",["_l_ () _r?_"];
  "namespace_name",["_l_ id _u_ nmspc _rd_"];
  "assignable",[
    "coerce           id ()";
    "nmspc            _l_ :: id _r?_ optional_pblock";
    "id ::            id ()";
    "id () ?: no_semicolon";
    "id () .          sqs";
    "id ()            ";
    "hdoc ";
    "include_like     _l_ loose= _r*_ ";
    "int          ";
    "new id           ()";
    "new nmspc        ()";
    "sqs . vvar . dqs . vvar -> id () . dqs . vvar -> id () . dqs";
    "sqs . vvar . sqs";
    "sqs";
    "vvar [ sqs ]";
    "vvar . sqs";
    "vvar = sqs";
    "vvar -> id optional_pblock _l_ -> id optional_pblock _r*_";
    "vvar + _l_ loose= _r*_ ";
    "vvar";
    "@                id ()";
    "() ?  string_or_var  :  string_or_var  "
               ];
    "beheaded_ivy",[
      "exit ;";
      "{}   _l_ else if () {} _r*_      else {} ";
    ];
    "beheaded_iwy",[
      "_l_ no_ivies _r*_ if () : _l_ no_ivies _r*_ else : _l_ no_ivies _r*_ endif ; _l_ no_ivies _r*_";
      "_l_ no_ivies _r*_ if () : _l_ no_ivies _r*_ endif ; _l_ no_ivies _r*_";
      "_l_ no_ivies _r*_"
    ];  
    "statement",[
      "vvar [ ] = assignable ;"; "vvar assign & assignable ;";                                                "vvar -> id_or_var  =  assignable ;"; "id :: id_or_var =  assignable ;";                             
      "vvar assign  assignable ;";
      "vvar [  int_or_string_or_var  ] = assignable ;";
      "vvar [  int_or_string_or_var   ]  =   & assignable  ;";
      "abstract class _l_ no_left_brace _r*_ {}";
      "final class _l_ no_left_brace _r*_ {}"; "class _l_ no_left_brace _r*_ {}";
      "declare () ;"; "echo vvar ext"; "echo _l_ no_semicolon _r*_ ;"; "exit ;";
      "foreach () {}"; "foreach () :  _l_ no_breach _r*_  endforeach ;";
      "@ id () ;"; "function id () {}"; "return  function () {} ;";
      "include_like _l_ stringy _r*_ ;"; "interface _l_ no_left_brace _r*_ {}";
      "if () beheaded_ivy"; "if () : beheaded_iwy endif ;"; "namespace nmspc ;";
      "namespace id ;"; "namespace  namespace_name {}";
      "return _l_ no_semicolon _r*_ ;"; "namespace  {}"; "ext";
      " id :: id () _l_ -> id () _r+_  ;";
      "vvar _l_ -> id_or_var optional_pblock  _r+_ ;";
      "static vvar assign assignable ;"; "id :: id () ;"; "nmspc :: id () ;";
      "switch () {}"; "trait id {}"; "try {} catch () {}"; "while () {}";
      "use _l_ no_semicolon _r*_ ;"
    ];   
]);;

(* Definition of PHP spider ends here *)

let php ()=unveil(!php_ref);;

let print_stringlist_naively l=
  let temp1=Image.image (fun s->(Strung.enclose s)) l in
  let temp2=String.concat ";" temp1 in
  "["^temp2^"]\n";;  

let print_stringlist_with_offset l w=
    let offset=String.make w ' ' in
    let temp1=Image.image (fun s->offset^"  "^(Strung.enclose s)) l in
    let temp2=("[")::(temp1@[offset^"]\n"]) in
    String.concat "\n" temp2;;

let print_stringlist w l=
     if List.length(l)=1
     then print_stringlist_naively l
     else print_stringlist_with_offset l w;;

let padding=3;;

let print_spider_item (s,l)=
    let n=String.length(s) in
    let padder=String.make padding ' ' in    
    padder^(Strung.enclose s)^","^
    (print_stringlist (n+padding+3) l);;

let print_spider (Sp l)=
   let temp1=Image.image print_spider_item l in
   let temp2=String.concat "\n" temp1 in
   "["^temp2^"]";;

let helper_for_rememberance new_spider=
  "\n\n\n let php_ref=ref(Sp\n"^(print_spider new_spider)^");;\n\n\n";;

end;;  
  
let php=Private.php;;
 
    

