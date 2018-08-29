
(* 

Useful to avoid empty strings between two successive separators in
an archive string, or to avoid empty strings to be forgotten during unarchiving. 

#use"nonblank.ml";;

*)

let make s=if s="" then "#" else s;;
let decode s=if s="#" then "" else s;;
  
