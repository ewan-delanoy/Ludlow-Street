(*

#use"pretty_print_list.ml";;

*)


let pretty_print_list
  (initial_printer,max_line_length,separator) l=
  let lsep=String.length(separator) in
  let rec helper=( fun 
   (graet,s_between,da_ober)->
     match da_ober with
     []->(if s_between=""
          then graet
          else graet^"\n"^s_between)
     |a::peurrest->
        let s_a=initial_printer(a) in
        if String.length(s_between)+lsep+String.length(s_a)>max_line_length
        then helper(graet^"\n"^s_between,s_a,peurrest)
        else helper(graet,s_between^separator^s_a,peurrest)     
  ) in
  helper("","",l);;
  
(*

print_string(pretty_print_list (string_of_int,15," ") (ennig 1 200));;

*)  