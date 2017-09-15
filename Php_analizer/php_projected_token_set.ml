(*

#use"Php_analizer/php_projected_token_set.ml";;

*)
type t=N of Php_projected_token.t list;;


module Private=struct
(* Inherited set operations *)

let from_list l=N(Ordered.diforchan_plaen Php_projected_token.order l);;

let intersection (N x) (N y)=N(Ordered.kengeij_plaen 
   Php_projected_token.order x y
);;

let empty_intersection x y=((intersection x y)=N[]);;
    
let setminus (N x) (N y)=N(Ordered.lemel_plaen
Php_projected_token.order x y
);;

let union l=N(Ordered.teuzin_kalz_plaen
Php_projected_token.order (Image.image (fun (N z)->z) l)
);;

exception Acts_only_once_exn;;

let acts_only_once (N x)=
  let l=List.length(x) in
  if l=0 then raise(Acts_only_once_exn) else
  if l>1 then false else
  Php_projected_token.acts_only_once(List.hd(x));;

let whole=from_list Php_projected_token.all_tokens;;

let complement x=setminus whole x;;

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

exception Unnamed_set of t;;

let get_old_name_for_set x=
  match Option.seek(fun (n,y)->y=x)(!namelist) with
  Some(name1,_)->name1
  |None->raise(Unnamed_set(x));;

let get_name_for_set x opt=
    match Option.seek(fun (n,y)->y=x)(!namelist) with
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
  match Option.seek(fun (n,y)->n=name)(!namelist) with
  Some(_,x)->x
 |None->raise(Unused_name(name));;

let define_precedence_set sol op=
    get_name_for_set (from_precedence sol op)
    (Some((Strict_or_loose.to_string sol)^(Php_operator.make_visible op)));;

(* Particular sets *)

let noneof blckr=
    let (lt,rt)=Php_projected_token.pair_for_blocker blckr in
    complement_from_list [lt;rt];;

let list_for_block_complements =
    Image.image
    (
      fun blckr->
        let (l,r)=Php_blocker.make_visible blckr in
        (
          "noneof"^l^r,
          noneof blckr
        )
    )
    Php_blocker.all;;

 
let _=Image.image (fun (name,z)->get_name_for_set z (Some name)) 
list_for_block_complements;;





let assign=from_list( Image.image 
           (fun x->Php_projected_token.constant
                (Php_constant_token.of_operator(x)))
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
(fun x->Php_projected_token.constant
     (Php_constant_token.of_operator(x)))
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
  Php_projected_token.variable; 
  Php_projected_token.ident; 
]
);;   

get_name_for_set id_or_var (Some "id_or_var");;

let include_like=from_list( 
    Image.image (fun x->Php_projected_token.constant (Php_constant_token.of_keyword x))
    [Php_keyword.t_include; 
     Php_keyword.t_include_once;                                
     Php_keyword.t_require; 
     Php_keyword.t_require_once]   
  );;   
  
get_name_for_set include_like (Some "include_like");;

let int_or_string_or_var=from_list( 
  [
    Php_projected_token.variable; 
    Php_projected_token.iint; 
    Php_projected_token.single_quoted;
    Php_projected_token.double_quoted;
  ]
  );;   
  
get_name_for_set int_or_string_or_var (Some "int_or_string_or_var");;

let no_breach=complement_from_list( 
  Image.image (fun x->Php_projected_token.constant(Php_constant_token.of_keyword(x)))
  [
    Php_keyword.t_foreach;
    Php_keyword.t_endforeach;
  ]
  );;   
  
get_name_for_set no_breach (Some "no_breach");;

let no_colon=complement_from_list( 
  [
    Php_projected_token.constant(Php_constant_token.of_operator Php_operator.t_colon)
  ]
  );;   
  
get_name_for_set no_colon (Some "no_colon");;

let no_ivies=complement_from_list( 
  Image.image (fun x->Php_projected_token.constant(Php_constant_token.of_keyword(x)))
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
    Php_projected_token.constant(Php_constant_token.of_punctuator Php_punctuator.t_lbrace)
  ]
  );;   
  
get_name_for_set no_left_brace (Some "no_left_brace");;

let no_semicolon=complement_from_list( 
  [
    Php_projected_token.constant(Php_constant_token.of_punctuator Php_punctuator.t_semicolon)
  ]
  );;   
  
get_name_for_set no_semicolon (Some "no_semicolon");;


let stringy=complement_from_list( 
  (
    Image.image (fun x->Php_projected_token.constant(x))
    [
      Php_constant_token.of_operator Php_operator.t_dot; 
      Php_constant_token.of_operator Php_operator.t_lbracket;
      Php_constant_token.of_operator Php_operator.t_rbracket;
      Php_constant_token.of_operator Php_operator.t_question;
      Php_constant_token.of_operator Php_operator.t_colon;
      Php_constant_token.of_operator Php_operator.t_equals_more;
      Php_constant_token.of_punctuator Php_punctuator.t_colon_colon;
      Php_constant_token.of_punctuator Php_punctuator.t_lparenthesis;
      Php_constant_token.of_punctuator Php_punctuator.t_rparenthesis;
      Php_constant_token.of_punctuator Php_punctuator.t_comma;
      Php_constant_token.of_punctuator Php_punctuator.t_arrow;
    ]
  )
  @
  (
    [
      Php_projected_token.variable; 
      Php_projected_token.ident; 
      Php_projected_token.comment; 
      Php_projected_token.single_quoted;
      Php_projected_token.double_quoted; 
      Php_projected_token.heredoc; 
      Php_projected_token.nowdoc
    ]
  ) 
  );;   
  
get_name_for_set stringy (Some "stringy");;

let string_or_var=from_list( 
  [
    Php_projected_token.variable; 
    Php_projected_token.single_quoted;
    Php_projected_token.double_quoted;
  ]
  );;   
  
get_name_for_set string_or_var (Some "string_or_var");;

define_precedence_set Strict_or_loose.Loose Php_operator.t_equals;;

let readables_and_toksets=
   (
     Image.image 
     (fun (s,ptok)->(s,N([ptok])) )
     Php_projected_token.readables_and_tokens
   )
   @
   (!namelist);;


let helper_for_generated_algebra l new_elt=
    let temp1=Image.image 
      (fun (x,descr)->(setminus x new_elt,(new_elt,false)::descr) ) l 
    and temp2=Image.image 
      (fun (x,descr)->(intersection x new_elt,(new_elt,true)::descr) ) l in
    let temp3=Image.image fst   (snd(List.hd l)) in
    let temp4=(new_elt,true)::(Image.image (fun t->(t,false)) temp3) 
    and temp5=setminus new_elt (union temp3) in
    let temp6=(temp5,temp4)::(temp1@temp2) in
    List.filter(fun (N y,descr)->y<>[]) temp6;;
    
let generated_algebra=function  
   []->[]
   |a::peurrest->
     let start=[a,[a,true]] in
     List.fold_left helper_for_generated_algebra start peurrest;;



end;;

let acts_only_once=Private.acts_only_once;;
let readables_and_toksets=Private.readables_and_toksets;;

let noneof=Private.noneof;;
let left_blocker blckr=
  let (lt,_)=Php_projected_token.pair_for_blocker blckr in
  N [lt];;
let right_blocker blckr=
    let (_,rt)=Php_projected_token.pair_for_blocker blckr in
    N [rt];;  


let from_precedence=Private.from_precedence;;

let get_name_for_set=Private.get_name_for_set;;
let get_set_for_name=Private.get_set_for_name;;
let get_old_name_for_set=Private.get_old_name_for_set;;

let empty_intersection=Private.empty_intersection;;
let generated_algebra=Private.generated_algebra;;

let test (N l) x=Ordered.elfenn_plaen Php_projected_token.order x l;; 


let order=((fun (N x) (N y)->
  Total_ordering.silex_compare Php_projected_token.order x y
) : t Total_ordering.t);;
