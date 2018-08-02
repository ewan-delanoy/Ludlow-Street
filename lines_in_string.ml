(*

#use"lines_in_string.ml";;

*)

let core old_s=
   let left_offset=(if Substring.begins_with old_s "\n" then "\n" else "")
   and right_offset=(if Substring.ends_with old_s "\n" then "\n" else "") in
   let s=left_offset^old_s^right_offset in
   let temp1=Str.split (Str.regexp_string "\n") s in
   Ennig.index_everything temp1;;

(*

core "a\nb";;
core "\na\nb";;
core "a\nb\n";;

*)


let interval s i j=
    let temp1=core s in
    let temp2=List.filter (fun (k,_)->(i<=k)&&(k<=j)) temp1  in
    let temp3=Image.image snd temp2 in
    String.concat "\n" temp3;; 

let line_at_index s i=List.assoc i (core s);;


exception Lines_in_char_range_exn of int*int;;

let number_of_lines_in_char_interval s  i j=
   try (List.length(List.filter (fun k->
       String.get s (k-1)='\n'
   ) (Ennig.ennig i j))) with
   _->raise(Lines_in_char_range_exn(i,j));; 

let remove_interval s i j=
  let temp1=core s in
  let temp2=List.filter (fun (k,_)->(i>k)||(k>j)) temp1  in
  let temp3=Image.image snd temp2 in
  String.concat "\n" temp3;; 

let remove_interval_in_file fn i j=
    let s1=Io.read_whole_file fn in
    let s2=remove_interval s1 i j  in
   Io.overwrite_with fn s2;;   