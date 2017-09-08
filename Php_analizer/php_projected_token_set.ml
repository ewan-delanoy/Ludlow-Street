(*

#use"Php_analizer/php_projected_token_set.ml";;

*)
type t=N of Php_projected_token.t list;;


module Private=struct
(* Inherited set operations *)

let from_list l=N(Ordered.diforchan_plaen Php_projected_token.order l);;

let kengeij (N x) (N y)=N(Ordered.kengeij_plaen 
   Php_projected_token.order x y
);;

let kengeij_goullo x y=((kengeij x y)=N[]);;
    
let lemel (N x) (N y)=N(Ordered.lemel_plaen
Php_projected_token.order x y
);;

exception Acts_only_once_exn;;

let acts_only_once (N x)=
  let l=List.length(x) in
  if l=0 then raise(Acts_only_once_exn) else
  if l>1 then false else
  Php_projected_token.acts_only_once(List.hd(x));;

let whole=from_list Php_projected_token.all_tokens;;

let complement x=lemel whole x;;

let complement_from_list l=complement(from_list l);;


let from_precedence sol op=
    from_list(
                Php_projected_token.precedence_neutral_tokens
                @
                (
                  List.filter 
                    ( fun ptok->
                          match Php_projected_token.precedence(ptok) with
                            None->false
                          |Some(p)->Strict_or_loose.test 
                              sol p (Php_operator.precedence op)
                    )
                  Php_projected_token.all_tokens
                )
    );;


(* Naming used sets *)

let namelist=ref([]:(string*t) list);;

let name_counter=ref(0);;

let get_name_for_set x opt=
    match Option.find_it(fun (n,y)->y=x)(!namelist) with
     Some(name1,_)->name1
    |None->
      (
        match opt with
        Some(name2)->
           let _=(namelist:=(name2,x)::(!namelist)) in
           name2
        |None-> 
           let p=(!name_counter)+1 in
           let name3="tokset_"^(string_of_int p) in
           let _=(
                  name_counter:=p;
                  namelist:=(name3,x)::(!namelist)
                  ) in
           name3        
      );;

exception Unused_name of string;;

let get_set_for_name name=
  match Option.find_it(fun (n,y)->n=name)(!namelist) with
  Some(_,x)->x
 |None->raise(Unused_name(name));;

let define_precedence_set sol op=
    get_name_for_set (from_precedence sol op)
    (Some((Strict_or_loose.to_string sol)^(Php_operator.make_visible op)));;

(* Particular sets *)

let assign=from_list( Image.image 
           (fun x->Php_projected_token.Constant
                (Php_constant_token.c_op(x)))
          [
           Php_operator.t_equals; 
           Php_operator.t_vline_equals; 
           Php_operator.t_plus_equals; 
           Php_operator.t_minus_equals;
           Php_operator.t_star_equals
           ]
);;   

get_name_for_set assign (Some "assign");;

let coerce=from_list( Image.image 
(fun x->Php_projected_token.Constant
     (Php_constant_token.c_op(x)))
[
  Php_operator.t_coerce_to_int; 
  Php_operator.t_coerce_to_bool;
  Php_operator.t_coerce_to_string;
  Php_operator.t_coerce_to_array;
  Php_operator.t_coerce_to_object; 
  Php_operator.t_coerce_to_bool;
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
    Image.image (fun x->Php_projected_token.Constant (Php_constant_token.c_kwd x))
    [Php_keyword.t_include; 
     Php_keyword.t_include_once;                                
     Php_keyword.t_require; 
     Php_keyword.t_require_once]   
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
  Image.image (fun x->Php_projected_token.Constant(Php_constant_token.c_kwd(x)))
  [
    Php_keyword.t_foreach;
    Php_keyword.t_endforeach;
  ]
  );;   
  
get_name_for_set no_breach (Some "no_breach");;

let no_colon=complement_from_list( 
  [
    Php_projected_token.Constant(Php_constant_token.c_op Php_operator.t_colon)
  ]
  );;   
  
get_name_for_set no_colon (Some "no_colon");;

let no_ivies=complement_from_list( 
  Image.image (fun x->Php_projected_token.Constant(Php_constant_token.c_kwd(x)))
  [
    Php_keyword.t_if; 
    Php_keyword.t_else; 
    Php_keyword.t_elseif; 
    Php_keyword.t_endif;
  ]
  );;   
  
get_name_for_set no_ivies (Some "no_ivies");;

let no_left_brace=complement_from_list( 
  [
    Php_projected_token.Constant(Php_constant_token.c_punct Php_punctuator.t_lbrace)
  ]
  );;   
  
get_name_for_set no_left_brace (Some "no_left_brace");;

let no_semicolon=complement_from_list( 
  [
    Php_projected_token.Constant(Php_constant_token.c_punct Php_punctuator.t_semicolon)
  ]
  );;   
  
get_name_for_set no_semicolon (Some "no_semicolon");;

let no_ternary=complement_from_list( 
  Image.image (fun x->Php_projected_token.Constant(Php_constant_token.c_op(x)))
  [
    Php_operator.t_question; 
    Php_operator.t_colon;
  ]
  );;   
  
get_name_for_set no_ternary (Some "no_ternary");;

let stringy=complement_from_list( 
  (
    Image.image (fun x->Php_projected_token.Constant(x))
    [
      Php_constant_token.c_op Php_operator.t_dot; 
      Php_constant_token.c_op Php_operator.t_lbracket;
      Php_constant_token.c_op Php_operator.t_rbracket;
      Php_constant_token.c_op Php_operator.t_question;
      Php_constant_token.c_op Php_operator.t_colon;
      Php_constant_token.c_op Php_operator.t_equals_more;
      Php_constant_token.c_punct Php_punctuator.t_colon_colon;
      Php_constant_token.c_punct Php_punctuator.t_lparenthesis;
      Php_constant_token.c_punct Php_punctuator.t_rparenthesis;
      Php_constant_token.c_punct Php_punctuator.t_comma;
      Php_constant_token.c_punct Php_punctuator.t_arrow;
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

define_precedence_set Strict_or_loose.Loose Php_operator.t_equals;;

let all_pairs=
   (
     Image.image 
     (fun (s,ptok)->(s,N([ptok])) )
     Php_projected_token.all_pairs
   )
   @
   (!namelist);;

end;;

let acts_only_once=Private.acts_only_once;;
let all_pairs=Private.all_pairs;;

let from_precedence=Private.from_precedence;;

let get_name_for_set=Private.get_name_for_set;;
let get_set_for_name=Private.get_set_for_name;;

let kengeij_goullo=Private.kengeij_goullo;;

let test (N l) x=Ordered.elfenn_plaen Php_projected_token.order x l;; 


let order=((fun (N x) (N y)->
  Total_ordering.silex_compare Php_projected_token.order x y
) : t Total_ordering.t);;
