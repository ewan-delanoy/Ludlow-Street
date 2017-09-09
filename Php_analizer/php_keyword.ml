(*

#use"Php_analizer/php_keyword.ml";;

*)



(* from https://github.com/php/php-langspec/blob/master/spec/09-lexical-structure.md#keywords 

   added the "exit" keyword
*)

type t=
    [ 
     `T_ABSTRACT
    |`T_AS
    |`T_BREAK
    |`T_CALLABLE
    |`T_CASE
    |`T_CATCH
    |`T_CLASS
    |`T_CONST
    |`T_CONTINUE
    |`T_DECLARE
    |`T_DEFAULT
    |`T_DO
    |`T_ECHO
    |`T_ELSE
    |`T_ELSEIF
    |`T_ENDDECLARE
    |`T_ENDFOR
    |`T_ENDFOREACH
    |`T_ENDIF
    |`T_ENDSWITCH
    |`T_ENDWHILE
    |`T_EXIT
    |`T_EXTENDS
    |`T_FINAL
    |`T_FINALLY
    |`T_FOR
    |`T_FOREACH
    |`T_FUNCTION
    |`T_GLOBAL
    |`T_GOTO
    |`T_IF
    |`T_IMPLEMENTS
    |`T_INCLUDE
    |`T_INCLUDE_ONCE
    |`T_INSTEADOF
    |`T_INTERFACE
    |`T_LIST
    |`T_NAMESPACE
    |`T_PRINT
    |`T_PRIVATE
    |`T_PROTECTED
    |`T_PUBLIC
    |`T_REQUIRE
    |`T_REQUIRE_ONCE
    |`T_RETURN
    |`T_STATIC
    |`T_SWITCH
    |`T_THROW
    |`T_TRAIT
    |`T_TRY
    |`T_USE
    |`T_VAR
    |`T_WHILE
    |`T_YIELD
    ];;

let t_abstract = (`T_ABSTRACT:t) ;;
let t_as = (`T_AS:t) ;;
let t_break = (`T_BREAK:t) ;;
let t_callable = (`T_CALLABLE:t) ;;
let t_case = (`T_CASE:t) ;;
let t_catch = (`T_CATCH:t) ;;
let t_class = (`T_CLASS:t) ;;
let t_const = (`T_CONST:t) ;;
let t_continue = (`T_CONTINUE:t) ;;
let t_declare = (`T_DECLARE:t) ;;
let t_default = (`T_DEFAULT:t) ;;
let t_do = (`T_DO:t) ;;
let t_echo = (`T_ECHO:t) ;;
let t_else = (`T_ELSE:t) ;;
let t_elseif = (`T_ELSEIF:t) ;;
let t_enddeclare = (`T_ENDDECLARE:t) ;;
let t_endfor = (`T_ENDFOR:t) ;;
let t_endforeach = (`T_ENDFOREACH:t) ;;
let t_endif = (`T_ENDIF:t) ;;
let t_endswitch = (`T_ENDSWITCH:t) ;;
let t_endwhile = (`T_ENDWHILE:t) ;;
let t_exit = (`T_EXIT:t) ;;
let t_extends = (`T_EXTENDS:t) ;;
let t_final = (`T_FINAL:t) ;;
let t_finally = (`T_FINALLY:t) ;;
let t_for = (`T_FOR:t) ;;
let t_foreach = (`T_FOREACH:t) ;;
let t_function = (`T_FUNCTION:t) ;;
let t_global = (`T_GLOBAL:t) ;;
let t_goto = (`T_GOTO:t) ;;
let t_if = (`T_IF:t) ;;
let t_implements = (`T_IMPLEMENTS:t) ;;
let t_include = (`T_INCLUDE:t) ;;
let t_include_once = (`T_INCLUDE_ONCE:t) ;;
let t_insteadof = (`T_INSTEADOF:t) ;;
let t_interface = (`T_INTERFACE:t) ;;
let t_list = (`T_LIST:t) ;;
let t_namespace = (`T_NAMESPACE:t) ;;
let t_print = (`T_PRINT:t) ;;
let t_private = (`T_PRIVATE:t) ;;
let t_protected = (`T_PROTECTED:t) ;;
let t_public = (`T_PUBLIC:t) ;;
let t_require = (`T_REQUIRE:t) ;;
let t_require_once = (`T_REQUIRE_ONCE:t) ;;
let t_return = (`T_RETURN:t) ;;
let t_static = (`T_STATIC:t) ;;
let t_switch = (`T_SWITCH:t) ;;
let t_throw = (`T_THROW:t) ;;
let t_trait = (`T_TRAIT:t) ;;
let t_try = (`T_TRY:t) ;;
let t_use = (`T_USE:t) ;;
let t_var = (`T_VAR:t) ;;
let t_while = (`T_WHILE:t) ;;
let t_yield = (`T_YIELD:t) ;;






let data=([
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
] : (string * t) list);;

    


  
let all=(Image.image snd data : t list);; 

exception Unknown_readable of string;; 

let readable=((
  fun x->
 fst(Option.find_really (fun (s,y)->y=x) data)
 ) : t -> string);;

let from_readable=(
  (fun viz->
    match Option.find_it(
      fun (viz1,_)->viz1=viz
      ) data with
     None->raise(Unknown_readable(viz))
    |Some(_,kwd)->kwd
  ): string -> t);;

let make_visible=readable;;
  

let from_visible=from_readable;;

  
   
