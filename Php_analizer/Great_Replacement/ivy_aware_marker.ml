(*

#use"Php_analizer/Great_Replacement/ivy_aware_marker.ml";;

*)


let central_salt="LIFMANPOHJALA";;

let salt_before = "\n\nmark_on_stone('"^central_salt;;
let salt_after = "');\n\n";;
let dimension = 6;;

let small_inflator_of_int i=
    central_salt^(Strung.left_completed_string_of_int dimension i);;

let inflator_of_int i=
    salt_before^(Strung.left_completed_string_of_int dimension i)^salt_after;;

let length=(String.length salt_before) +dimension +(String.length salt_after);;

let int_of_inflator s=
    let i=(String.length salt_before) +1 in
    let j=i+(dimension-1) in
    int_of_string(Cull_string.interval s i j);;

(*

let example = 37;;
let s=inflator_of_int example;;
let t=int_of_inflator s;;
let check=(t=example);;

*)    