
(* 

#use"Country/Alaska/alaskan_printer_equipped_types.ml";;


*)


let instructions printer_equipped_types=
  let temp2=List.rev_map (
    function (x,compiled_correctly)->
      if compiled_correctly 
      then "#install_printer "^(Half_dressed_module.capitalized_module_name x)^".print_out;"^";"
      else ""
  ) printer_equipped_types in
  let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
  let part2=String.concat "\n" temp3 in
  part2;;  
 
let declare_printer hm0 l=hm0::l;;
         
let undeclare_printer hm0 l=
  List.filter (fun hm->hm<>hm0) l;;    
 