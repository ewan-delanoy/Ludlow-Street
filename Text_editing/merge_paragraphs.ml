(*

#use"Text_editing/merge_paragraphs.ml";;

*)

let main_transform 
    ((is_paragraph1,(_,text1)),next_par_is_suspicious)=
      if is_paragraph1
      then Cull_string.trim_spaces text1
      else 
      if next_par_is_suspicious
      then " "
      else "\n\n\n";;

let in_string s=
    let temp1=Decompose_into_paragraphs.dec s in
    let last_one=List.hd(List.rev temp1) in
    let temp2=Listennou.universal_delta_list temp1 in
    let temp3=Image.image (fun (p1,(is_paragraph2,(_,text2)))->
       (p1,if is_paragraph2 then Has_suspicious_beginning.hsb text2 else false)
    ) temp2 in
    let temp4=temp3@[last_one,false] in
    let temp5=Image.image main_transform temp4 in
    String.concat "" temp5;;

let in_file  ap=
    let old_text=Io.read_whole_file ap in
    let new_text=in_string old_text in
    Io.overwrite_with ap new_text;;        
