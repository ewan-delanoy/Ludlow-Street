(*

#use"debugger.ml";;
Used for debugging purposes only.


*)



let ap1=Absolute_path.of_string
("/Users/ewandelanoy/Documents/Experimental_Installing/"^
"Experimental_gnumake/010_Almost_cleaned/"^
"commands.c");;

let text1=Io.read_whole_file ap1;;

let test6=Gparser.apply  Gparser_for_c_language.main_prsr text1 1;;

