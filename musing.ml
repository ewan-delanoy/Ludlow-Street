let z1=hmx "main";;

let z2=(abo "main")@[z1];;

let z3=image (fun hm->
Mlx_ended_absolute_path.to_absolute_path(
Mlx_ended_absolute_path.join hm
  Ocaml_ending.Ml)) z2;;

let z4=image (fun ap->
  snd(Modularize.modularize ap)
)z3;;  

let z5=String.concat "\n\n\n" z4;;

let ap_main=(List.hd(List.rev z3));;

Io.erase_file_and_fill_it_with_string
ap_main z5;;
