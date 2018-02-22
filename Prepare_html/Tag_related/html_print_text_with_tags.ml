(*

#use"Prepare_html/Tag_related/html_print_text_with_tags.ml";;

*)

let tab_width=3;;

let rec compute_lines j text=match text with
 Html_text_with_tags.Leaf (s)->
    let temp1=Str.split (Str.regexp_string "\n") s in
    let indent=String.make j ' ' in
    Image.image (fun t->indent^t) temp1 
|Html_text_with_tags.Tagged(tag_name,_,x)->
    let indent=String.make j ' ' in
    let first_line=indent^"<"^full_tag^">"
    and last_line= indent^"</"^tag_name^">" in
    let temp1=compute_lines(j+tab_width) x in
    first_line::(temp1@[last_line])
|Html_text_with_tags.Concat(l)->
  let temp2=Image.image (compute_lines j) l in
  List.flatten temp2;;

let print x=
    String.concat "\n" (compute_lines 0 x);;  



  
        



