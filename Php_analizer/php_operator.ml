(*

#use"Php_analizer/php_operator.ml";;

*)


let left=Associativity.Left_associative;;
let right=Associativity.Right_associative;;
let nonassoc=Associativity.Non_associative;;

(* from http://php.net/manual/en/language.operators.precedence.php *)
type t=
    [
     `T_CLONE
    |`T_NEW
    |`T_LBRACKET
    |`T_RBRACKET
    |`T_STAR_STAR
    |`T_PLUS_PLUS
    |`T_MINUS_MINUS
    |`T_TILDA
    |`T_COERCE_TO_INT
    |`T_COERCE_TO_FLOAT
    |`T_COERCE_TO_STRING
    |`T_COERCE_TO_ARRAY
    |`T_COERCE_TO_OBJECT
    |`T_COERCE_TO_BOOL
    |`T_AT
    |`T_INSTANCEOF
    |`T_EXCLAMATION
    |`T_STAR
    |`T_DIVIDE
    |`T_PERCENTAGE
    |`T_PLUS
    |`T_MINUS
    |`T_DOT
    |`T_LESS_LESS
    |`T_MORE_MORE
    |`T_LESS
    |`T_LESS_EQUALS
    |`T_MORE
    |`T_MORE_EQUALS
    |`T_EQUALS_EQUALS
    |`T_EXCLAMATION_EQUALS
    |`T_EQUALS_EQUALS_EQUALS
    |`T_EXCLAMATION_EQUALS_EQUALS
    |`T_LESS_MORE
    |`T_AMPERSAND
    |`T_CIRCUMFLEX
    |`T_VLINE
    |`T_AMPERSAND_AMPERSAND
    |`T_VLINE_VLINE
    |`T_QUESTION
    |`T_COLON
    |`T_EQUALS
    |`T_PLUS_EQUALS
    |`T_MINUS_EQUALS
    |`T_STAR_EQUALS
    |`T_STAR_STAR_EQUALS
    |`T_DIVIDE_EQUALS
    |`T_DOT_EQUALS
    |`T_PERCENTAGE_EQUALS
    |`T_AMPERSAND_EQUALS
    |`T_VLINE_EQUALS
    |`T_CIRCUMFLEX_EQUALS
    |`T_LESS_LESS_EQUALS
    |`T_MORE_MORE_EQUALS
    |`T_EQUALS_MORE
    |`T_AND
    |`T_XOR
    |`T_OR
    ];;

    let t_clone = (`T_CLONE:t) ;;
    let t_new = (`T_NEW:t) ;;
    let t_lbracket = (`T_LBRACKET:t) ;;
    let t_rbracket = (`T_RBRACKET:t) ;;
    let t_star_star = (`T_STAR_STAR:t) ;;
    let t_plus_plus = (`T_PLUS_PLUS:t) ;;
    let t_minus_minus = (`T_MINUS_MINUS:t) ;;
    let t_tilda = (`T_TILDA:t) ;;
    let t_coerce_to_int = (`T_COERCE_TO_INT:t) ;;
    let t_coerce_to_float = (`T_COERCE_TO_FLOAT:t) ;;
    let t_coerce_to_string = (`T_COERCE_TO_STRING:t) ;;
    let t_coerce_to_array = (`T_COERCE_TO_ARRAY:t) ;;
    let t_coerce_to_object = (`T_COERCE_TO_OBJECT:t) ;;
    let t_coerce_to_bool = (`T_COERCE_TO_BOOL:t) ;;
    let t_at = (`T_AT:t) ;;
    let t_instanceof = (`T_INSTANCEOF:t) ;;
    let t_exclamation = (`T_EXCLAMATION:t) ;;
    let t_star = (`T_STAR:t) ;;
    let t_divide = (`T_DIVIDE:t) ;;
    let t_percentage = (`T_PERCENTAGE:t) ;;
    let t_plus = (`T_PLUS:t) ;;
    let t_minus = (`T_MINUS:t) ;;
    let t_dot = (`T_DOT:t) ;;
    let t_less_less = (`T_LESS_LESS:t) ;;
    let t_more_more = (`T_MORE_MORE:t) ;;
    let t_less = (`T_LESS:t) ;;
    let t_less_equals = (`T_LESS_EQUALS:t) ;;
    let t_more = (`T_MORE:t) ;;
    let t_more_equals = (`T_MORE_EQUALS:t) ;;
    let t_equals_equals = (`T_EQUALS_EQUALS:t) ;;
    let t_exclamation_equals = (`T_EXCLAMATION_EQUALS:t) ;;
    let t_equals_equals_equals = (`T_EQUALS_EQUALS_EQUALS:t) ;;
    let t_exclamation_equals_equals = (`T_EXCLAMATION_EQUALS_EQUALS:t) ;;
    let t_less_more = (`T_LESS_MORE:t) ;;
    let t_ampersand = (`T_AMPERSAND:t) ;;
    let t_circumflex = (`T_CIRCUMFLEX:t) ;;
    let t_vline = (`T_VLINE:t) ;;
    let t_ampersand_ampersand = (`T_AMPERSAND_AMPERSAND:t) ;;
    let t_vline_vline = (`T_VLINE_VLINE:t) ;;
    let t_question = (`T_QUESTION:t) ;;
    let t_colon = (`T_COLON:t) ;;
    let t_equals = (`T_EQUALS:t) ;;
    let t_plus_equals = (`T_PLUS_EQUALS:t) ;;
    let t_minus_equals = (`T_MINUS_EQUALS:t) ;;
    let t_star_equals = (`T_STAR_EQUALS:t) ;;
    let t_star_star_equals = (`T_STAR_STAR_EQUALS:t) ;;
    let t_divide_equals = (`T_DIVIDE_EQUALS:t) ;;
    let t_dot_equals = (`T_DOT_EQUALS:t) ;;
    let t_percentage_equals = (`T_PERCENTAGE_EQUALS:t) ;;
    let t_ampersand_equals = (`T_AMPERSAND_EQUALS:t) ;;
    let t_vline_equals = (`T_VLINE_EQUALS:t) ;;
    let t_circumflex_equals = (`T_CIRCUMFLEX_EQUALS:t) ;;
    let t_less_less_equals = (`T_LESS_LESS_EQUALS:t) ;;
    let t_more_more_equals = (`T_MORE_MORE_EQUALS:t) ;;
    let t_equals_more = (`T_EQUALS_MORE:t) ;;
    let t_and = (`T_AND:t) ;;
    let t_xor = (`T_XOR:t) ;;
    let t_or = (`T_OR:t) ;;

    let data=(([
    (t_clone,nonassoc,1,"clone","clone");
    (t_new,nonassoc,1,"new","new");
    (t_lbracket,left,2,"[","lbracket");
    (t_rbracket,left,2,"]","rbracket");
    (t_star_star,right,3,"**","star_star");
    (t_plus_plus,right,4,"++","plus_plus");
    (t_minus_minus,right,4,"--","minus_minus");
    (t_tilda,right,4,"~","tilda");
    (t_coerce_to_int,right,4,"(int)","coerce_to_int");
    (t_coerce_to_float,right,4,"(float)","coerce_to_float");
    (t_coerce_to_string,right,4,"(string)","coerce_to_string");
    (t_coerce_to_array,right,4,"(array)","coerce_to_array");
    (t_coerce_to_object,right,4,"(object)","coerce_to_object");
    (t_coerce_to_bool,right,4,"(bool)","coerce_to_bool");
    (t_at,right,4,"@","at");
    (t_instanceof,nonassoc,5,"instanceof","instanceof");
    (t_exclamation,right,6,"!","exclamation");
    (t_star,left,7,"*","star");
    (t_divide,left,7,"/","divide");
    (t_percentage,left,7,"%","percentage");
    (t_plus,left,8,"+","plus");
    (t_minus,left,8,"-","minus");
    (t_dot,left,8,".","dot");
    (t_less_less,left,9,"<<","less_less");
    (t_more_more,left,9,">>","more_more");
    (t_less,nonassoc,10,"<","less");
    (t_less_equals,nonassoc,10,"<=","less_equals");
    (t_more,nonassoc,10,">","more");
    (t_more_equals,nonassoc,10,">=","more_equals");
    (t_equals_equals,nonassoc,11,"==","equals_equals");
    (t_exclamation_equals,nonassoc,11,"!=","exclamation_equals");
    (t_equals_equals_equals,nonassoc,11,"===","equals_equals_equals");
    (t_exclamation_equals_equals,nonassoc,11,"!==","exclamation_equals_equals");
    (t_less_more,nonassoc,11,"<>","less_more");
    (t_ampersand,left,12,"&","ampersand");
    (t_circumflex,left,13,"^","circumflex");
    (t_vline,left,14,"|","vline");
    (t_ampersand_ampersand,left,15,"&&","ampersand_ampersand");
    (t_vline_vline,left,16,"||","vline_vline");
    (t_question,left,17,"?","question");
    (t_colon,left,17,":","colon");
    (t_equals,left,18,"=","equals");
    (t_plus_equals,left,18,"+=","plus_equals");
    (t_minus_equals,left,18,"-=","minus_equals");
    (t_star_equals,left,18,"*=","star_equals");
    (t_star_star_equals,left,18,"**=","star_star_equals");
    (t_divide_equals,left,18,"/=","divide_equals");
    (t_dot_equals,left,18,".=","dot_equals");
    (t_percentage_equals,left,18,"%=","percentage_equals");
    (t_ampersand_equals,left,18,"&=","ampersand_equals");
    (t_vline_equals,left,18,"|=","vline_equals");
    (t_circumflex_equals,left,18,"^=","circumflex_equals");
    (t_less_less_equals,left,18,"<<=","less_less_equals");
    (t_more_more_equals,left,18,">>=","more_more_equals");
    (t_equals_more,left,18,"=>","equals_more");
    (t_and,left,19,"and","and");
    (t_xor,left,20,"xor","xor");
    (t_or,left,21,"or","or");
 ]
    ): (t * Associativity.t * int * string * string) list);;
     



let precedence=((
    fun op->
      let (_,_,prec,_,_)=Option.find_really(
          fun (op1,_,_,_,_)->op1=op
      ) data in
      prec
   ) : t -> int );;

let associativity=
  (
    (fun op->
  let (_,asc,_,_,_)=Option.find_really(
    fun (op1,_,_,_,_)->op1=op
    ) data in
    asc
  ) : t -> Associativity.t );;


let all=Image.image (fun (op,asc,prec,viz,sn)->op) data;;  
 
exception Unknown_visible of string;; 



let make_visible=(( 
    fun op->
    let (_,_,_,viz,_)=Option.find_really(
        fun (op1,_,_,_,_)->op1=op
    ) data in
    viz
) : t -> string );;

let from_visible=((function viz->
  match Option.find_it(
    fun (_,_,_,viz1,_)->viz1=viz
    ) data with
   None->raise(Unknown_visible(viz))
  |Some(op,_,_,_,_)->op) : string -> t);;

let readable=make_visible;;



let level=((fun s->
  let p0=precedence(from_visible s) in
  List.filter (fun op->precedence(op)=p0) all) : string -> t list);;  
  

  
  
   
