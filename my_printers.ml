
(*
 #use"/Users/ewandelanoy/Documents/OCaml/Ordinary/my_loadings.ml";;
*)





(*Registered printers start here *)



#install_printer Binary_constraint.print_out;;
#install_printer Decimal.print_out;;
#install_printer Positioned_php_token.print_out;;
#install_printer Rat_frac.print_out;;
#install_printer Simplex_relation.print_out;;
#install_printer Constraint.print_out;;
#install_printer Relation.print_out;;
#install_printer Descriptive_system.print_out;;
#install_printer Vector.print_out;;
#install_printer Polynomial.print_out;;
#install_printer Rational.print_out;;
#install_printer Absolute_path.print_out;;



(*Registered printers end here *)

(* Below are printers needed only in utop's inefficient environment. *)

#install_printer Positioned_php_token.print_out_list;;