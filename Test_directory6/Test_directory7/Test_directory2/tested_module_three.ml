(*

#use"Test_directory6/Test_directory7/Test_directory2/tested_module_three.ml";;


*)

let f x y=
  let c=5+6 in
  let d=Tested_module_two.f 7 8 in
  c+d;;