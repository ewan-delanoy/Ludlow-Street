(*

#use"Php_analizer/Great_Replacement/ivy_aware_marker.ml";;

*)

let salt = "\n\nLIFMANPOHJALA";;
let dimension = 6;;

let inflator_of_int i=
    salt^(Strung.left_completed_string_of_int dimension i);;

let int_of_inflator s=
    let i=(String.length salt) +1 in
    let j=i+(dimension-1) in
    int_of_string(Cull_string.interval s i j);;

(*

let example = 1;;
let s=inflator_of_int example;;
let t=int_of_inflator s;;
let check=(t=example);;

*)    