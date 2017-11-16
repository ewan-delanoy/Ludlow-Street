(*

#use"Text_editing/shorten_paragraphs.ml";;

*)

exception Disconnected_paragraph;;

let in_paragraph paragraph=
    if Substring.is_a_substring_of "\n\n" paragraph 
    then raise(Disconnected_paragraph)
    else let temp1=Str.split (Str.regexp_string "\n") paragraph  in
         let temp2=Image.image Cull_string.trim_spaces temp1 in
         let (first_line,temp3)=Listennou.ht temp2 in
         let temp4=Image.image(
             fun line->if Has_suspicious_beginning.hsb line 
                       then " "^line
                       else "\n"^line
         ) temp3 in
         String.concat "" (first_line::temp4);;
  
let in_string s=
     let temp1=Decompose_into_paragraphs.dec s in
     let temp2=Image.image(
        fun (is_paragraph,(range,text))->
          if is_paragraph
          then in_paragraph text
          else text
     ) temp1 in
     String.concat "" temp2;;

let in_file  ap=
    let old_text=Io.read_whole_file ap in
    let new_text=in_string old_text in
    Io.overwrite_with ap new_text;;        
         
