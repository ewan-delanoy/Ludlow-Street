(*

#use"naked_module.ml";;

A module name, or a candidate for one. Uncapitalized. Should contain no slashes.

*)


let of_string s=Naked_module_t.N (String.uncapitalize_ascii s);; 
let to_string (Naked_module_t.N s)=s;;

let add_prefix_and_capitalize prefix (Naked_module_t.N name)=
  Naked_module_t.N(String.capitalize_ascii(prefix^name));;



