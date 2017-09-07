(*

#use"Php_analizer/php_keyword.ml";;

*)



(* from https://github.com/php/php-langspec/blob/master/spec/09-lexical-structure.md#keywords 

   added the "exit" keyword
*)

type t=
     T_ABSTRACT
    |T_AS
    |T_BREAK
    |T_CALLABLE
    |T_CASE
    |T_CATCH
    |T_CLASS
    |T_CONST
    |T_CONTINUE
    |T_DECLARE
    |T_DEFAULT
    |T_DO
    |T_ECHO
    |T_ELSE
    |T_ELSEIF
    |T_ENDDECLARE
    |T_ENDFOR
    |T_ENDFOREACH
    |T_ENDIF
    |T_ENDSWITCH
    |T_ENDWHILE
    |T_EXIT
    |T_EXTENDS
    |T_FINAL
    |T_FINALLY
    |T_FOR
    |T_FOREACH
    |T_FUNCTION
    |T_GLOBAL
    |T_GOTO
    |T_IF
    |T_IMPLEMENTS
    |T_INCLUDE
    |T_INCLUDE_ONCE
    |T_INSTEADOF
    |T_INTERFACE
    |T_LIST
    |T_NAMESPACE
    |T_PRINT
    |T_PRIVATE
    |T_PROTECTED
    |T_PUBLIC
    |T_REQUIRE
    |T_REQUIRE_ONCE
    |T_RETURN
    |T_STATIC
    |T_SWITCH
    |T_THROW
    |T_TRAIT
    |T_TRY
    |T_USE
    |T_VAR
    |T_WHILE
    |T_YIELD;;

let t_abstract = (T_ABSTRACT) ;;
let t_as = (T_AS) ;;
let t_break = (T_BREAK) ;;
let t_callable = (T_CALLABLE) ;;
let t_case = (T_CASE) ;;
let t_catch = (T_CATCH) ;;
let t_class = (T_CLASS) ;;
let t_const = (T_CONST) ;;
let t_continue = (T_CONTINUE) ;;
let t_declare = (T_DECLARE) ;;
let t_default = (T_DEFAULT) ;;
let t_do = (T_DO) ;;
let t_echo = (T_ECHO) ;;
let t_else = (T_ELSE) ;;
let t_elseif = (T_ELSEIF) ;;
let t_enddeclare = (T_ENDDECLARE) ;;
let t_endfor = (T_ENDFOR) ;;
let t_endforeach = (T_ENDFOREACH) ;;
let t_endif = (T_ENDIF) ;;
let t_endswitch = (T_ENDSWITCH) ;;
let t_endwhile = (T_ENDWHILE) ;;
let t_exit = (T_EXIT) ;;
let t_extends = (T_EXTENDS) ;;
let t_final = (T_FINAL) ;;
let t_finally = (T_FINALLY) ;;
let t_for = (T_FOR) ;;
let t_foreach = (T_FOREACH) ;;
let t_function = (T_FUNCTION) ;;
let t_global = (T_GLOBAL) ;;
let t_goto = (T_GOTO) ;;
let t_if = (T_IF) ;;
let t_implements = (T_IMPLEMENTS) ;;
let t_include = (T_INCLUDE) ;;
let t_include_once = (T_INCLUDE_ONCE) ;;
let t_insteadof = (T_INSTEADOF) ;;
let t_interface = (T_INTERFACE) ;;
let t_list = (T_LIST) ;;
let t_namespace = (T_NAMESPACE) ;;
let t_print = (T_PRINT) ;;
let t_private = (T_PRIVATE) ;;
let t_protected = (T_PROTECTED) ;;
let t_public = (T_PUBLIC) ;;
let t_require = (T_REQUIRE) ;;
let t_require_once = (T_REQUIRE_ONCE) ;;
let t_return = (T_RETURN) ;;
let t_static = (T_STATIC) ;;
let t_switch = (T_SWITCH) ;;
let t_throw = (T_THROW) ;;
let t_trait = (T_TRAIT) ;;
let t_try = (T_TRY) ;;
let t_use = (T_USE) ;;
let t_var = (T_VAR) ;;
let t_while = (T_WHILE) ;;
let t_yield = (T_YIELD) ;;

let all_pairs=[
  ("abstract",t_abstract);
("as",t_as);
("break",t_break);
("callable",t_callable);
("case",t_case);
("catch",t_catch);
("class",t_class);
("const",t_const);
("continue",t_continue);
("declare",t_declare);
("default",t_default);
("do",t_do);
("echo",t_echo);
("else",t_else);
("elseif",t_elseif);
("enddeclare",t_enddeclare);
("endfor",t_endfor);
("endforeach",t_endforeach);
("endif",t_endif);
("endswitch",t_endswitch);
("endwhile",t_endwhile);
("exit",t_exit);
("extends",t_extends);
("final",t_final);
("finally",t_finally);
("for",t_for);
("foreach",t_foreach);
("function",t_function);
("global",t_global);
("goto",t_goto);
("if",t_if);
("implements",t_implements);
("include",t_include);
("include_once",t_include_once);
("insteadof",t_insteadof);
("interface",t_interface);
("list",t_list);
("namespace",t_namespace);
("print",t_print);
("private",t_private);
("protected",t_protected);
("public",t_public);
("require",t_require);
("require_once",t_require_once);
("return",t_return);
("static",t_static);
("switch",t_switch);
("throw",t_throw);
("trait",t_trait);
("try",t_try);
("use",t_use);
("var",t_var);
("while",t_while);
("yield",t_yield);
];;

    

let to_string x=
  fst(Option.find_really (fun (s,y)->y=x) all_pairs);;
  
let all_keywords=Image.image snd all_pairs;; 

exception Unknown_keyword_string of string;; 


let of_string viz=
    match Option.find_it(
      fun (viz1,_)->viz1=viz
      ) all_pairs with
     None->raise(Unknown_keyword_string(viz))
    |Some(_,kwd)->kwd;;

  
let all_strings=Image.image to_string all_keywords;;        
 
  
  
   
