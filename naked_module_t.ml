(*

#use"naked_module_t.ml";;

A module name, or a candidate for one. Uncapitalized. 
Should contain no slashes.

*)



let of_string s=Naked_module.N (String.uncapitalize_ascii s);; 
let to_string (Naked_module.N s)=s;;

