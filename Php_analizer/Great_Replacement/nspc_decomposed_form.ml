(*

#use"Php_analizer/Great_Replacement/nspc_decomposed_form.ml";;

*)

(*

When there is at least one namespace, namespacable is None.
When there is no namespace and a declare statement, namespacable
stores all that's after that statement, while before_namespaces
contains the rest (including the statement itself).

The items in the namespaced_parts list are quadruples (a,b,c,d)
where a is the namespace declaration line, b is the
namepsaced content, anc c is either an empty string or just
a closing brace (depending on whether the namespace declaration
is semicoloned or braced) and d is either an empty string
or the part of the code after the closing of the namespace above.

*)

type t={
    before_namespaces : string;
    namespacable : string option;
    namespaced_parts : (string*string*string*string) list;
};;

let before_namespaces x=x.before_namespaces;;
let namespacable x=x.namespacable;;
let namespaced_parts x=x.namespaced_parts;;

let make a b c={
      before_namespaces =a;
      namespacable =b;
      namespaced_parts =c;
  };;
  





