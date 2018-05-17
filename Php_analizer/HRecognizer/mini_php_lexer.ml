(*

#use"Php_analizer/HRecognizer/mini_php_lexer.ml";;

*)

let alphabetic_keywords=
  [
    "abstract";
    "array";
    "catch";
    "const";
    "define";
    "do";
    "echo";
    "extends";
    "false";
    "for";
    "foreach";
    "final";
    "finally";
    "function";
    "global";
    "class";
    "include";
    "include_once";
    "interface";
    "new";
    "null";
    "NULL";
    "namespace";
    "private";
    "protected";
    "public";
    "require";
    "require_once";
    "return";
    "static";
    "switch";
    "throw";
    "true";
    "try";
    "var";
    "while";
    "use";

  ];;

let ordered_alphabetic_keywords=
  Ordered.diforchan_plaen Total_ordering.for_longest_match alphabetic_keywords;;

let other_keywords=
  [
    
  ] ;; 

exception Unnamed_block of char;;

let get_enclosed_name c=
    try List.assoc c ['(',"parenthesed";'[',"bracketed";'{',"braced"] with
    _->raise(Unnamed_block(c));;

let one_more_step s i=
   let n=String.length s in
   if i>n
   then None
   else 
   let c=String.get s (i-1) in
   if List.mem c Charset.list_of_whites 
   then let opt=After.after_whites s i in
        let j=Option.unpack opt in
        Some(("whites",Cull_string.interval s i (j-1)),j)
   else 
   let opt1=Option.seek (fun (a,b)->c=a) Charset.enclosers in
   if opt1<>None
   then let enclosers=Option.unpack opt1 in
        let j=After.after_closing_character enclosers s (i,0)  in
        let enclosed_name=(get_enclosed_name c)^"_block" in
        Some((enclosed_name,Cull_string.interval s i (j-1)),j)
   else 
   if c='\''
   then let j=After.after_simple_quoted_string s i in
        Some(("sq",Cull_string.interval s i (j-1)),j)
   else
   if c='"'
   then let j=After.after_double_quoted_string s i in
        Some(("dq",Cull_string.interval s i (j-1)),j)
   else
   if List.mem c Charset.php_label_first_letters
   then let opt=After.after_star Charset.php_label_nonfirst_letters s (i+1) in
        let j=Option.unpack opt in
        let word=Cull_string.interval s i (j-1) in
        let block_name=(
        if List.mem word alphabetic_keywords 
        then word
        else "php_name"
        ) in 
        Some((block_name,Cull_string.interval s i (j-1)),j)
   else Some(("c",Cull_string.interval s i i),i+1);;

let iterator s =
    let n=String.length s in
    let rec tempf=(fun (graet,i)->
        if i>n then List.rev graet else
        match one_more_step s i with
        None->List.rev(graet)
        |Some(data,j)->tempf(data::graet,j)
    ) in
    tempf;;

let lex s=iterator s ([],1);;









  