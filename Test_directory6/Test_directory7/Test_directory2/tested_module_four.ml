(*

#use"Test_directory6/Test_directory7/Test_directory2/tested_module_four.ml";;


*)

let f x y=
  let a=1+2 in
  let b=Tested_module_three.f 3 4 in
  a+b;;