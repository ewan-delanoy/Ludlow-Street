(*

#use"single_copy_task.ml";;

*)

type t={
   port    : int;
   webhost : string;
   local_root: string;
   remote_root: string;
   filename   :string;  
};;

let execute x=
  let cmd1=(
   if not(String.contains x.filename '/')
   then ""
   else let base=Father_and_son.father x.filename '/' in
        "ssh -p "^(string_of_int x.port)^" "^(x.webhost)^
        " \"mkdir -p "^(x.remote_root)^base^"\""
  ) in
  let i1=Sys.command cmd1  in
  if i1<>0 then 1 else
  let cmd2="scp -P "^(string_of_int x.port)^" "^
  (x.local_root)^(x.filename)^" "^
  (x.webhost)^":"^
  (x.remote_root)^(x.filename) in
  let i2=Sys.command cmd2  in
  if i2<>0 then 2 else
  0;;
  


(*

let example={
   port = 7822;
   webhost = "tribunem@tribunemicael.net";
   local_root ="/Users/ewandelanoy/Documents/";  
   remote_root ="~/private_html/";  
   filename="Labour/pindex.txt";
};;

execute example;;

*)
   
