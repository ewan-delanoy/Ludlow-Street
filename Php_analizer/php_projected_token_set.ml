(*

#use"Php_analizer/php_projected_token_set.ml";;

*)
type t=N of Php_projected_token.t Ordered.old_set;;


module Private=struct
(* Inherited set operations *)

let from_list l=N(Ordered.diforchan Php_projected_token.order l);;

let kengeij (N x) (N y)=N(Ordered.kengeij 
   Php_projected_token.order x y
);;
    
let lemel (N x) (N y)=N(Ordered.lemel
Php_projected_token.order x y
);;

let whole=from_list Php_projected_token.all_tokens;;

let complement x=lemel whole x;;

let complement_from_list l=complement(from_list l);;


(* Naming used sets *)

let namelist=ref([]:(t*string) list);;

let name_counter=ref(0);;

let get_name_for_set x opt=
    match Option.find_it(fun (y,n)->y=x)(!namelist) with
     Some(_,name1)->name1
    |None->
      (
        match opt with
        Some(name2)->
           let _=(namelist:=(x,name2)::(!namelist)) in
           name2
        |None-> 
           let p=(!name_counter)+1 in
           let name3="tokset_"^(string_of_int p) in
           let _=(
                  name_counter:=p;
                  namelist:=(x,name3)::(!namelist)
                  ) in
           name3        
      );;

exception Unused_name of string;;

let get_set_for_name name=
  match Option.find_it(fun (y,n)->n=name)(!namelist) with
  Some(x,_)->x
 |None->raise(Unused_name(name));;


(* Particular sets *)

let assign=from_list( Image.image 
           (fun x->Php_projected_token.Constant
                (Php_constant_token.Op(x)))
          [
           Php_operator.T_EQUALS; 
           Php_operator.T_VLINE_EQUALS; 
           Php_operator.T_PLUS_EQUALS; 
           Php_operator.T_MINUS_EQUALS;
           Php_operator.T_STAR_EQUALS
           ]
);;   

get_name_for_set assign (Some "assign");;

let coerce=from_list( Image.image 
(fun x->Php_projected_token.Constant
     (Php_constant_token.Op(x)))
[
  Php_operator.T_COERCE_TO_INT; 
  Php_operator.T_COERCE_TO_BOOL;
  Php_operator.T_COERCE_TO_STRING;
  Php_operator.T_COERCE_TO_ARRAY;
  Php_operator.T_COERCE_TO_OBJECT; 
  Php_operator.T_COERCE_TO_BOOL;
]
);;   

get_name_for_set coerce (Some "coerce");;


let id_or_var=from_list( 
[
  Php_projected_token.Variable; 
  Php_projected_token.Ident; 
]
);;   

get_name_for_set id_or_var (Some "id_or_var");;

let include_like=from_list( 
    Image.image (fun x->Php_projected_token.Constant (Php_constant_token.Kwd x))
    [Php_keyword.T_INCLUDE; 
     Php_keyword.T_INCLUDE_ONCE;                                
     Php_keyword.T_REQUIRE; 
     Php_keyword.T_REQUIRE_ONCE]   
  );;   
  
get_name_for_set include_like (Some "include_like");;

let int_or_string_or_var=from_list( 
  [
    Php_projected_token.Variable; 
    Php_projected_token.Ident; 
    Php_projected_token.Single_quoted;
    Php_projected_token.Double_quoted;
  ]
  );;   
  
get_name_for_set int_or_string_or_var (Some "int_or_string_or_var");;

let no_breach=complement_from_list( 
  Image.image (fun x->Php_projected_token.Constant(Php_constant_token.Kwd(x)))
  [
    Php_keyword.T_FOREACH;
    Php_keyword.T_ENDFOREACH;
  ]
  );;   
  
get_name_for_set no_breach (Some "no_breach");;

let no_colon=complement_from_list( 
  [
    Php_projected_token.Constant(Php_constant_token.Op Php_operator.T_COLON)
  ]
  );;   
  
get_name_for_set no_colon (Some "no_colon");;

let no_ivies=complement_from_list( 
  Image.image (fun x->Php_projected_token.Constant(Php_constant_token.Kwd(x)))
  [
    Php_keyword.T_IF; 
    Php_keyword.T_ELSE; 
    Php_keyword.T_ELSEIF; 
    Php_keyword.T_ENDIF;
  ]
  );;   
  
get_name_for_set no_ivies (Some "no_ivies");;

let no_left_brace=complement_from_list( 
  [
    Php_projected_token.Constant(Php_constant_token.Punct Php_punctuator.T_LBRACE)
  ]
  );;   
  
get_name_for_set no_left_brace (Some "no_left_brace");;

let no_semicolon=complement_from_list( 
  [
    Php_projected_token.Constant(Php_constant_token.Punct Php_punctuator.T_SEMICOLON)
  ]
  );;   
  
get_name_for_set no_semicolon (Some "no_semicolon");;

let no_ternary=complement_from_list( 
  Image.image (fun x->Php_projected_token.Constant(Php_constant_token.Op(x)))
  [
    Php_operator.T_QUESTION; 
    Php_operator.T_COLON;
  ]
  );;   
  
get_name_for_set no_ternary (Some "no_ternary");;

let stringy=complement_from_list( 
  (
    Image.image (fun x->Php_projected_token.Constant(x))
    [
      Php_constant_token.Op Php_operator.T_DOT; 
      Php_constant_token.Op Php_operator.T_LBRACKET;
      Php_constant_token.Op Php_operator.T_RBRACKET;
      Php_constant_token.Op Php_operator.T_QUESTION;
      Php_constant_token.Op Php_operator.T_COLON;
      Php_constant_token.Op Php_operator.T_EQUALS_MORE;
      Php_constant_token.Punct Php_punctuator.T_COLON_COLON;
      Php_constant_token.Punct Php_punctuator.T_LPARENTHESIS;
      Php_constant_token.Punct Php_punctuator.T_RPARENTHESIS;
      Php_constant_token.Punct Php_punctuator.T_COMMA;
      Php_constant_token.Punct Php_punctuator.T_ARROW;
    ]
  )
  @
  (
    [
      Php_projected_token.Variable; 
      Php_projected_token.Ident; 
      Php_projected_token.Comment; 
      Php_projected_token.Single_quoted;
      Php_projected_token.Double_quoted; 
      Php_projected_token.Heredoc; 
      Php_projected_token.Nowdoc
    ]
  ) 
  );;   
  
get_name_for_set stringy (Some "stringy");;

end;;

let get_name_for_set=Private.get_name_for_set;;
let get_set_for_name=Private.get_set_for_name;;


