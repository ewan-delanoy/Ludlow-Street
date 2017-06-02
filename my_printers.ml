
(*
 #use"/Users/ewandelanoy/Documents/OCaml/Ordinary/my_printers.ml";;
*)


(*Registered printers start here *)



#install_printer Rational.print_out;;
#install_printer Polynomial.print_out;;
#install_printer Hidden_vector.print_out;;
#install_printer Binary_constraint.print_out;;
#install_printer Absolute_path.print_out;;
#install_printer Decimal_number.print_out;;
#install_printer Simplex_relation.print_out;;
#install_printer Rat_frac.print_out;;
#install_printer Positioned_php_token.print_out;;
#install_printer Small_int_based_rational.print_out;;
#install_printer Big_int_based_rational.print_out;;
#install_printer Positioned_php_token_list.print_out;;



(*Registered printers end here *)

(* Below are printers needed only in utop's environment which is
unfriendly to Ocaml printers. *)

(*
let phptoklist=
	Gather_preprinter_result.printer_of_preprinter(Old_preprinter_example.positioned_php_token_list);;

#install_printer phptoklist;;
*)