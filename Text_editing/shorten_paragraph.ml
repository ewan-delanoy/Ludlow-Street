(*

#use"Text_editing/shorten_paragraph.ml";;

*)

exception Disconnected_paragraph;;

let sh paragraph=
    if Substring.is_a_substring_of "\n\n" paragraph 
    then raise(Disconnected_paragraph)
    else let temp1=Str.split (Str.regexp_string "\n") paragraph  in
         let temp2=Image.image Cull_string.trim_spaces temp1 in
         let (first_line,temp3)=Listennou.ht temp2 in
         let temp4=Image.image(
             fun line->if Has_suspicious_beginning.hsb line 
                       then " "^line
                       else "n"^line
         ) temp3 in
         String.concat "" (first_line::temp4);;
  
