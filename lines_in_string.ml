(*

#use"lines_in_string.ml";;

*)

let core s=
   let temp1=Str.split (Str.regexp_string "\n") s in
   Ennig.index_everything temp1;;

let interval s i j=
    let temp1=core s in
    let temp2=List.filter (fun (k,_)->(i<=k)&&(k<=j)) temp1  in
    let temp3=Image.image snd temp2 in
    String.concat "\n" temp3;; 

let line_at_index s i=List.assoc i (core s);;

let number_of_lines_in_char_interval s  i j=
   List.length(List.filter (fun k->
       String.get s (k-1)='\n'
   ) (Ennig.ennig i j));; 

let remove_interval s i j=
  let temp1=core s in
  let temp2=List.filter (fun (k,_)->(i>k)||(k>j)) temp1  in
  let temp3=Image.image snd temp2 in
  String.concat "\n" temp3;; 