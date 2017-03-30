(*

#use"Haag1/Haag2/please_test_me.ml";;

*)


let a=2;;

module PrivateOne=struct
let b=3;;
end;;

let c=4;;

module Private=struct
include PrivateOne;;
let d=5;;
end;;

module Weak=struct
let e=6;;
end;;

