(*

#use"Test_directory6/Test_directory7/Test_directory2/testable_executable.ml";;


*)

let unique_argument = (
  fun arr->
    if Array.length(arr)<2
    then " "
    else Array.get arr 1
) (Sys.argv);;

let filler = Total_ordering.standard;;

let message = "Hello World ! \n You just wrote : "^unique_argument^"\n\n";;

print_string message;;