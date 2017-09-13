(*

#use"Php_analizer/Beavers/beaver_for_statement.ml";;




*)

type postok = ( Php_token.t * (Lexing.position * Lexing.position)  ) ;;
type plex= postok ;;
type plexl=Php_positioned_token_list.t;;

 

type element={
    name               : string;
    content            : string;
    catalyser          : string;
    helper             : (plexl list -> Php_char_range.t -> Beaver_result.t);
};;   
   
let element_cmp elt1 elt2=
   let step1=Total_ordering.lex_for_strings elt1.name elt2.name in
   if step1<>Total_ordering.Equal
   then step1
   else Tidel.cmp
         (elt1.content,elt1.catalyser)     
         (elt2.content,elt2.catalyser);;
          
let element_order=(element_cmp: element Total_ordering.t);; 



let classical_parser elt=
   let f=(fun l->
      let opt2=Termite.parse (Termite.of_string elt.content) l in
      if opt2=None then None else
      let (l2,cr2,peurrest)=Option.unpack opt2 in
      let catalyser_check=(
        if elt.catalyser=""
        then true
        else (Termite.parse (Termite.of_string elt.catalyser) peurrest)<>None
      ) in
      if catalyser_check
      then Some(elt.helper l2 cr2,cr2,peurrest)
      else None
   ) in
   (f : Beaver_result.t Php_parser.t);;


let current_data_list=ref ([]:element list);;


let expand_element elt={
    name               = elt.name;
    content            = elt.content;
    catalyser          = elt.catalyser;
    helper             = elt.helper;
};;   

let add_data a b c d=
   let old_version=Ordered.safe_set element_order (!current_data_list) in
   let elt={
    name               = a;
    content            = b;
    catalyser          = c;
    helper             = d;
   }  in
   let new_set=Ordered.insert element_order elt old_version in
   let new_version=Ordered.forget_order new_set in 
   (current_data_list:=new_version);;
    
let update_data_list ()=
   let new_list=Image.image expand_element (!current_data_list) in
   current_data_list:=new_list;;    

    
let current_main_parser=
  (
    fun l->Option.find_and_stop 
    (fun tr->classical_parser tr l) (!current_data_list)
  );;

let display_data ()=
   let m1=snd(Max.maximize_it(fun elt->String.length(elt.name) 
   ) (!current_data_list)) in  
   let temp1=Image.image (fun 
      elt->
         let d=m1-(String.length(elt.name)) in
         let filler=String.make d ' ' in
         "\""^elt.name^"\""^filler^"   ,   \""^elt.content^"\""
   )(!current_data_list) in
   let temp2="\n\n\n"^(String.concat "\n" temp1)^"\n\n\n" in
   print_string temp2;;
   
   

let helper_for_byref_append l1 cr=
   let a=List.hd(List.hd l1) in
   Beaver_result.make 
    "appending" 
    [a]
    ["assignable",List.nth l1 1]  
    cr;;

add_data 
	"append_byref"
	"vvar [ ] = assignable ;"
	""
	helper_for_byref_append
	;;
   

let helper_for_assignment_byref l1 cr=
    let a=List.hd(List.nth l1 0) 
    and b=List.hd(List.nth l1 1)  in
    Beaver_result.make 
    "assignment_by_ref"
    [a;b]
    ["assignable",List.nth l1 2]
     cr;;   
 

add_data 
	"assign_byref"
	"vvar assign & assignable ;"
	""
	helper_for_assignment_byref;;
	
let helper_for_servant_assign l1 cr=
   let tf=(fun j->List.hd(List.nth l1 j)) in
   Beaver_result.make 
   "assign_on_servant"
   [tf 0;tf 1]
   ["assignable",List.nth l1 2]
    cr;;   

add_data 
	"assign_on_servant"
	"vvar -> id_or_var  =  assignable ;"
	""
	helper_for_servant_assign;;

add_data 
	"assign_on_static"
	"id :: id_or_var =  assignable ;"
	""
	helper_for_servant_assign
	;;

let helper_for_assignment l1 cr=
    let a=List.hd(List.nth l1 0) 
    and b=List.hd(List.nth l1 1)  in
    Beaver_result.make 
     "assignment"
     [a;b]
     ["assignable",List.nth l1 2]
      cr;;   
	
add_data 
  "assign_usual"
  "vvar assign  assignable ;"
  ""
  helper_for_assignment
  ;;


let helper_for_cell_assign l1 cr=
   let a=List.hd(List.hd l1) 
   and b=List.hd(List.nth l1 1) in
   Beaver_result.make 
   "cell_assignment"
   [a;b]
   ["assignable",List.nth l1 2]
    cr;;  


add_data 
	"cell_assign"
	"vvar [  int_or_string_or_var  ] = assignable ;"
	""
	helper_for_cell_assign
	;;

add_data 
	"cell_assign_byref"
	"vvar [  int_or_string_or_var   ]  =   & assignable  ;"
	""
	helper_for_cell_assign
	;;


let helper_for_abstract_class l1 cr=
  Beaver_result.make 
  "abstract_class"
  []
  ["class_generalities",List.nth l1 0;
   "class_details",List.nth l1 1; 
  ]
   cr;;  
   
add_data
  "class_abstract"
  "abstract class _l_ no_left_brace _r*_ {}"
  ""
  helper_for_abstract_class
  ;;   

let helper_for_final_class l1 cr=
    Beaver_result.make 
    "final_class"
    []
    ["class_generalities",List.nth l1 0;
     "class_details",List.nth l1 1; 
    ]
     cr;;    

   
add_data 
   "class_final"
   "final class _l_ no_left_brace _r*_ {}"
   ""
   helper_for_final_class;;   

let helper_for_usual_class l1 cr=
    Beaver_result.make 
    "class_usual"
    []
    ["class_generalities",List.nth l1 0;
     "class_details",List.nth l1 1; 
    ]
     cr;;       

add_data
  "class_usual"
  "class _l_ no_left_brace _r*_ {}"
  ""
  helper_for_usual_class
  ;;   

let helper_for_decl l1 cr=
  Beaver_result.make 
  "decl"
  []
  ["declare_arg",List.hd l1 ]
   cr;;    
  

add_data 
	"decl"
	"declare () ;"
	""
	helper_for_decl
	;;


let helper_for_echo1 l1 cr=
  let tf=(fun j->List.hd(List.nth l1 j)) in 
  Beaver_result.make 
  "echo1"
  [tf 0;tf 1]
  [ ]
   cr;; 

add_data 
	"echo1"
	"echo vvar ext"
	""
	helper_for_echo1
	;;



let helper_for_echo2 l1 cr=
    Beaver_result.make 
    "echo2"
    []
    ["echoable",List.hd l1 ]
     cr;;   

add_data 
     "echo2"
     "echo _l_ no_semicolon _r*_ ;"
     ""
     helper_for_echo2
     ;;  

let helper_for_exit l1 cr=
  Beaver_result.make 
  "exit"
  []
  []
   cr;;  

add_data 
	"exit"
	"exit ;"
	""
	helper_for_exit
	;;

let helper_for_foreach l1 cr=
    Beaver_result.make 
    "foreach_loop"
    []
    ["foreach_conditions",List.nth l1 0;
     "statement",List.nth l1 1; 
    ]
     cr;;    



add_data 
	"foreach1"
	"foreach () {}"
	""
	helper_for_foreach
	;;


add_data 
	"foreach2"
	"foreach () :  _l_ no_breach _r*_  endforeach ;"
	""
	helper_for_foreach
	;;

let helper_for_fun_call l1 cr=
    Beaver_result.make 
    "fun_call1"
    [List.hd(List.nth l1 0)]
    [
     "function_args",List.nth l1 1; 
    ]
     cr;;    


add_data 
	"fun_call1"
	"id () ;"
	""
	
	;;


add_data 
	"fun_call2"
	"@ id () ;"
	""
	helper_for_fun_call
	;;

let helper_for_fun_def l1 cr=
  let a=List.hd(List.hd l1) in
    Beaver_result.make 
    "fun_def"
    [a]
    [
     "function_args",List.nth l1 1;
     "function_body",List.nth l1 2; 
    ]
     cr;;    


add_data 
	"fun_def"
	"function id () {}"
	""
	helper_for_fun_def
	;;

let helper_for_fun_returning l1 cr=
    let a=List.hd(List.hd l1) in
      Beaver_result.make 
      "fun_returning"
      [a]
      [
       "function_args",List.nth l1 0;
       "function_body",List.nth l1 1; 
      ]
       cr;;    

add_data 
	"fun_returning"
	"return  function () {} ;"
	""
	helper_for_fun_returning
	;;

let helper_for_include_like l1 cr=
    let a=List.hd(List.hd l1) in
      Beaver_result.make 
      "include_like"
      [a]
      [
       "includable",List.nth l1 1;
      ]
       cr;;   

add_data 
   "include_like"
   "include_like _l_stringy _r*_ ;"
   ""
   helper_for_include_like
   ;; 


let helper_for_interface_decl l1 cr=
  Beaver_result.make 
  "interface_decl"
  []
  ["interface_generalities",List.nth l1 0;
   "interface_details",List.nth l1 1; 
  ]
   cr;;              

add_data 
	"interface_decl"
	"interface _l_ no_left_brace _r*_ {}"
	""
	helper_for_interface_decl
	;;
 
let helper_for_ivy l1 cr=
    Beaver_result.make 
    "ivy"
    []
    ["ivy_conditions",List.nth l1 0;
     "beheaded_ivy",List.nth l1 1; 
    ]
     cr;;      


add_data 
  "ivy"
  "if () beheaded_ivy"
  ""
  helper_for_ivy
  ;;    

let helper_for_iwy l1 cr=
    Beaver_result.make 
    "iwy"
    []
    ["iwy_conditions",List.nth l1 0;
     "beheaded_iwy",List.nth l1 1; 
    ]
     cr;;      



add_data 
   "iwy"
   "if () : beheaded_iwy endif ;"
   ""
   helper_for_iwy
   ;; 



let helper_for_nonroot_namespace_use l1 cr=
    Beaver_result.make 
      "nonroot_namespace_use"
      []
      [
        "namespace_name",List.nth l1 0;
        "statement_list",List.nth l1 1;        
      ]
       cr;;   

add_data 
	"nonroot_namespace_use"
	"namespace  namespace_name {}"
	""
	helper_for_nonroot_namespace_use
	;;

let helper_for_nmspc_definition l1 cr=
    let a=List.hd(List.hd l1) in
    Beaver_result.make 
      "namespace_definition"
      [a]
      []
      cr;;  

add_data 
	"nmspc_long_definition"
	"namespace nmspc ;"
	""
	helper_for_nmspc_definition
	;;


add_data 
	"nmspc_short_definition"
	"namespace id ;"
	""
	helper_for_nmspc_definition
	;;

let helper_for_returning l1 cr=
    Beaver_result.make 
      "returning"
      []
      ["returnable",List.hd l1]
      cr;;    


add_data 
	"returning"
	"return _l_ no_semicolon _r*_ ;"
	""
	helper_for_returning
	;;

let helper_for_root_namespace_use l1 cr=
    Beaver_result.make 
      "root_namespace_use"
      []
      [
        "statement_list",List.nth l1 0;        
      ]
       cr;;   

add_data 
	"root_namespace_use"
	"namespace  {}"
	""
	helper_for_root_namespace_use
	;;  


let helper_for_singleton l1 cr=
    let a=List.hd(List.hd l1) in
    Beaver_result.make 
      "singleton"
      [a]
      []
      cr;;  
   
add_data 
  "singleton"
  "ext"
  ""
  helper_for_singleton
  ;;   

let helper_for_snake_on_var l1 cr=
    let tf=(fun j->List.hd(List.nth l1 j)) in  
    Beaver_result.make 
      "snake_on_var"
      [tf 0]
      ["snake",List.nth l1 1;
      ]
       cr;;     

add_data 
	"snake_on_var"
	"vvar _l_ -> id_or_var optional_pblock  _r+_ ;"
	""
	helper_for_snake_on_var
	;;

let helper_for_snake_on_meth_call l1 cr=
    let tf=(fun j->List.hd(List.nth l1 j)) in  
    Beaver_result.make 
      "snake_on_meth_call"
      [tf 0;tf 1]
      ["snake",List.nth l1 2;
      ]
       cr;;     
  
add_data 
       "snake_on_meth_call"
       " id :: id () _l_ -> id () _r+_  ;"
       ""
       helper_for_snake_on_meth_call
       ;;     
   

add_data 
  "static_assignment"
  "static vvar assign assignable ;"
  ""
  helper_for_assignment
  ;;



let helper_for_static_meth l1 cr=
   let a1=List.hd(List.hd l1) 
   and a2=List.hd(List.nth l1 1) in
   Beaver_result.make 
   "static_meth"
   [a1;a2]
   ["function_args",List.nth l1 2;
   ]
    cr;;   

add_data 
	"static_meth"
	"id :: id () ;"
	""
	helper_for_static_meth
	;;

add_data 
	"static_meth_on_nmspc"
	"nmspc :: id () ;"
	""
	helper_for_static_meth
	;;  

let helper_for_switch l1 cr=
    let a=List.hd(List.hd l1) in
      Beaver_result.make 
      "switch"
      [a]
      [
       "switch_body",List.nth l1 1; 
      ]
       cr;;    

add_data 
	"switch"
	"switch () {}"
	""
	helper_for_switch
	;;

let helper_for_trait_decl l1 cr=
    let a=List.hd(List.hd l1) in
      Beaver_result.make 
      "trait_declaration"
      [a]
      [
       "trait_body",List.nth l1 1; 
      ]
       cr;;    

add_data 
	"trait_decl"
	"trait id {}"
	""
	helper_for_trait_decl
	;;

let helper_for_trycatch l1 cr=
      Beaver_result.make 
      "trait_declaration"
      []
      [
       "expression",List.nth l1 0; 
       "qualified_name",List.nth l1 1;
       "statement_list",List.nth l1 2;
      ]
       cr;;    


add_data 
	"trycatch"
	"try {} catch () {}"
	""
	helper_for_trycatch
	;;

let helper_for_while_loop l1 cr=
    Beaver_result.make 
    "trait_declaration"
    []
    [
     "while_condition",List.nth l1 0;
     "statement_list",List.nth l1 1;
    ]
     cr;;    

add_data 
	"while_loop"
	"while () {}"
	""
	helper_for_while_loop
	;;

let helper_for_yuze_decl l1 cr=
  Beaver_result.make 
  "yuze_declaration"
  []
  [
   "usable",List.nth l1 0;
  ]
   cr;;    

add_data 
	"yuze_decl"
	"use _l_ no_semicolon _r*_ ;"
	""
	helper_for_yuze_decl
	;;



let parser=(current_main_parser: Beaver_result.t Php_parser.t);; 



 
 



