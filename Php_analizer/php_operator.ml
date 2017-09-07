(*

#use"Php_analizer/php_operator.ml";;

*)


let left=Associativity.Left_associative;;
let right=Associativity.Right_associative;;
let nonassoc=Associativity.Non_associative;;

(* from http://php.net/manual/en/language.operators.precedence.php *)
type t=
     T_CLONE
    |T_NEW
    |T_LBRACKET
    |T_RBRACKET
    |T_STAR_STAR
    |T_PLUS_PLUS
    |T_MINUS_MINUS
    |T_TILDA
    |T_COERCE_TO_INT
    |T_COERCE_TO_FLOAT
    |T_COERCE_TO_STRING
    |T_COERCE_TO_ARRAY
    |T_COERCE_TO_OBJECT
    |T_COERCE_TO_BOOL
    |T_AT
    |T_INSTANCEOF
    |T_EXCLAMATION
    |T_STAR
    |T_DIVIDE
    |T_PERCENTAGE
    |T_PLUS
    |T_MINUS
    |T_DOT
    |T_LESS_LESS
    |T_MORE_MORE
    |T_LESS
    |T_LESS_EQUALS
    |T_MORE
    |T_MORE_EQUALS
    |T_EQUALS_EQUALS
    |T_EXCLAMATION_EQUALS
    |T_EQUALS_EQUALS_EQUALS
    |T_EXCLAMATION_EQUALS_EQUALS
    |T_LESS_MORE
    |T_AMPERSAND
    |T_CIRCUMFLEX
    |T_VLINE
    |T_AMPERSAND_AMPERSAND
    |T_VLINE_VLINE
    |T_QUESTION
    |T_COLON
    |T_EQUALS
    |T_PLUS_EQUALS
    |T_MINUS_EQUALS
    |T_STAR_EQUALS
    |T_STAR_STAR_EQUALS
    |T_DIVIDE_EQUALS
    |T_DOT_EQUALS
    |T_PERCENTAGE_EQUALS
    |T_AMPERSAND_EQUALS
    |T_VLINE_EQUALS
    |T_CIRCUMFLEX_EQUALS
    |T_LESS_LESS_EQUALS
    |T_MORE_MORE_EQUALS
    |T_EQUALS_MORE
    |T_AND
    |T_XOR
    |T_OR;;

    let t_clone = (T_CLONE) ;;
    let t_new = (T_NEW) ;;
    let t_lbracket = (T_LBRACKET) ;;
    let t_rbracket = (T_RBRACKET) ;;
    let t_star_star = (T_STAR_STAR) ;;
    let t_plus_plus = (T_PLUS_PLUS) ;;
    let t_minus_minus = (T_MINUS_MINUS) ;;
    let t_tilda = (T_TILDA) ;;
    let t_coerce_to_int = (T_COERCE_TO_INT) ;;
    let t_coerce_to_float = (T_COERCE_TO_FLOAT) ;;
    let t_coerce_to_string = (T_COERCE_TO_STRING) ;;
    let t_coerce_to_array = (T_COERCE_TO_ARRAY) ;;
    let t_coerce_to_object = (T_COERCE_TO_OBJECT) ;;
    let t_coerce_to_bool = (T_COERCE_TO_BOOL) ;;
    let t_at = (T_AT) ;;
    let t_instanceof = (T_INSTANCEOF) ;;
    let t_exclamation = (T_EXCLAMATION) ;;
    let t_star = (T_STAR) ;;
    let t_divide = (T_DIVIDE) ;;
    let t_percentage = (T_PERCENTAGE) ;;
    let t_plus = (T_PLUS) ;;
    let t_minus = (T_MINUS) ;;
    let t_dot = (T_DOT) ;;
    let t_less_less = (T_LESS_LESS) ;;
    let t_more_more = (T_MORE_MORE) ;;
    let t_less = (T_LESS) ;;
    let t_less_equals = (T_LESS_EQUALS) ;;
    let t_more = (T_MORE) ;;
    let t_more_equals = (T_MORE_EQUALS) ;;
    let t_equals_equals = (T_EQUALS_EQUALS) ;;
    let t_exclamation_equals = (T_EXCLAMATION_EQUALS) ;;
    let t_equals_equals_equals = (T_EQUALS_EQUALS_EQUALS) ;;
    let t_exclamation_equals_equals = (T_EXCLAMATION_EQUALS_EQUALS) ;;
    let t_less_more = (T_LESS_MORE) ;;
    let t_ampersand = (T_AMPERSAND) ;;
    let t_circumflex = (T_CIRCUMFLEX) ;;
    let t_vline = (T_VLINE) ;;
    let t_ampersand_ampersand = (T_AMPERSAND_AMPERSAND) ;;
    let t_vline_vline = (T_VLINE_VLINE) ;;
    let t_question = (T_QUESTION) ;;
    let t_colon = (T_COLON) ;;
    let t_equals = (T_EQUALS) ;;
    let t_plus_equals = (T_PLUS_EQUALS) ;;
    let t_minus_equals = (T_MINUS_EQUALS) ;;
    let t_star_equals = (T_STAR_EQUALS) ;;
    let t_star_star_equals = (T_STAR_STAR_EQUALS) ;;
    let t_divide_equals = (T_DIVIDE_EQUALS) ;;
    let t_dot_equals = (T_DOT_EQUALS) ;;
    let t_percentage_equals = (T_PERCENTAGE_EQUALS) ;;
    let t_ampersand_equals = (T_AMPERSAND_EQUALS) ;;
    let t_vline_equals = (T_VLINE_EQUALS) ;;
    let t_circumflex_equals = (T_CIRCUMFLEX_EQUALS) ;;
    let t_less_less_equals = (T_LESS_LESS_EQUALS) ;;
    let t_more_more_equals = (T_MORE_MORE_EQUALS) ;;
    let t_equals_more = (T_EQUALS_MORE) ;;
    let t_and = (T_AND) ;;
    let t_xor = (T_XOR) ;;
    let t_or = (T_OR) ;;
    
    let all_fourtuples=[
    (t_clone,nonassoc,1,"clone");
    (t_new,nonassoc,1,"new");
    (t_lbracket,left,2,"[");
    (t_rbracket,left,2,"]");
    (t_star_star,right,3,"**");
    (t_plus_plus,right,4,"++");
    (t_minus_minus,right,4,"--");
    (t_tilda,right,4,"~");
    (t_coerce_to_int,right,4,"(int)");
    (t_coerce_to_float,right,4,"(float)");
    (t_coerce_to_string,right,4,"(string)");
    (t_coerce_to_array,right,4,"(array)");
    (t_coerce_to_object,right,4,"(object)");
    (t_coerce_to_bool,right,4,"(bool)");
    (t_at,right,4,"@");
    (t_instanceof,nonassoc,5,"instanceof");
    (t_exclamation,right,6,"!");
    (t_star,left,7,"*");
    (t_divide,left,7,"/");
    (t_percentage,left,7,"%");
    (t_plus,left,8,"+");
    (t_minus,left,8,"-");
    (t_dot,left,8,".");
    (t_less_less,left,9,"<<");
    (t_more_more,left,9,">>");
    (t_less,nonassoc,10,"<");
    (t_less_equals,nonassoc,10,"<=");
    (t_more,nonassoc,10,">");
    (t_more_equals,nonassoc,10,">=");
    (t_equals_equals,nonassoc,11,"==");
    (t_exclamation_equals,nonassoc,11,"!=");
    (t_equals_equals_equals,nonassoc,11,"===");
    (t_exclamation_equals_equals,nonassoc,11,"!==");
    (t_less_more,nonassoc,11,"<>");
    (t_ampersand,left,12,"&");
    (t_circumflex,left,13,"^");
    (t_vline,left,14,"|");
    (t_ampersand_ampersand,left,15,"&&");
    (t_vline_vline,left,16,"||");
    (t_question,left,17,"?");
    (t_colon,left,17,":");
    (t_equals,left,18,"=");
    (t_plus_equals,left,18,"+=");
    (t_minus_equals,left,18,"-=");
    (t_star_equals,left,18,"*=");
    (t_star_star_equals,left,18,"**=");
    (t_divide_equals,left,18,"/=");
    (t_dot_equals,left,18,".=");
    (t_percentage_equals,left,18,"%=");
    (t_ampersand_equals,left,18,"&=");
    (t_vline_equals,left,18,"|=");
    (t_circumflex_equals,left,18,"^=");
    (t_less_less_equals,left,18,"<<=");
    (t_more_more_equals,left,18,">>=");
    (t_equals_more,left,18,"=>");
    (t_and,left,19,"and");
    (t_xor,left,20,"xor");
    (t_or,left,21,"or");
 ];;

let to_string op=
    let (_,_,_,viz)=Option.find_really(
        fun (op1,_,_,_)->op1=op
    ) all_fourtuples in
    viz;;

let precedence op=
      let (_,_,prec,_)=Option.find_really(
          fun (op1,_,_,_)->op1=op
      ) all_fourtuples in
      prec;;

let associativity op=
  let (_,asc,_,_)=Option.find_really(
    fun (op1,_,_,_)->op1=op
    ) all_fourtuples in
    asc;;

let all_pairs=
    let temp1=Image.image (fun (op,asc,prec,viz)->(viz,op)) all_fourtuples in
    Ordered.forget_order
      (Ordered.diforchan Keyval_ordering.ko temp1);;  

let all_operators=Image.image snd all_pairs;;  
 
exception Unknown_operator_string of string;; 
 
let of_prudent_string s=
   Option.find_it (fun oprtr->to_string(oprtr)=s) all_operators ;;
 
let of_string s=
  match of_prudent_string s with
   None->raise(Unknown_operator_string(s))
  |Some(oprtr)->oprtr;;
  
let level s=
  let p0=precedence(of_string s) in
  List.filter (fun op->precedence(op)=p0) all_operators;;  
  
let all_strings=Image.image fst all_pairs;;   
 
  
  
   
