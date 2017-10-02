(*

#use"localized_html.ml";;

*)

let core_marker="wfmVZdqJAgAThGBo";;

let parentheses_for_file=
    let paren="\n"^core_marker^"F\n" in
    (paren,paren);;

let parentheses_for_item=
  let paren="\n"^core_marker^"I\n" in
  (paren,paren);;

let separator_inside_item="\n"^core_marker^"|\n";;

let separate s=
    Str.split (Str.regexp_string separator_inside_item) s;;
    
exception No_file_locations_specified;;
exception Dimension_mismatch of int list;;

let decompose_localized_text s=
    let temp1=Parenthesed_block.decompose_without_taking_blanks_into_account 
    [parentheses_for_file;parentheses_for_item] s in
    let (temp2,temp3)=List.partition (fun (opt,_)->opt=Some(parentheses_for_file)) temp1 in
    if temp2=[]
    then  raise(No_file_locations_specified)
    else   
    let temp4=separate (snd(List.hd(temp2))) in
    let temp5=Image.image(fun (opt,s)->
      if opt=None then (false,[s]) else (true,separate s)
    ) temp3 in
    let temp6=Option.filter_and_unpack (
       fun (is_item,l)->if is_item then Some(List.length l) else None
    ) temp5 in
    let sz1=List.length(temp4) in
    if List.exists(fun k->k<>sz1) temp6
    then raise(Dimension_mismatch(sz1::temp6))
    else 
    let temp7=Ennig.doyle(
       fun j->
         let ttemp8=Image.image (fun (is_item,l)->
            if is_item
            then List.nth l j
            else List.hd l
         ) temp5 in
       (List.nth temp4 j,String.concat "" ttemp8)
    ) 0 (sz1-1) in
    temp7;;

let enforce_localized_text s=
  List.iter (
      fun (fn,txt)->
        Io.erase_file_and_fill_it_with_string
        (Absolute_path.of_string fn) txt
  ) (decompose_localized_text s);;

let enforce_localized_file ap=
  enforce_localized_text (Io.read_whole_file ap);;

(*

let ap1=Absolute_path.of_string
("/Users/ewandelanoy/Documents/Web_Projects/Online/"^
"Prepare_online/Persistent_example/localized_indexview.php");;
let text1=Io.read_whole_file ap1;;

let tag1=decompose text1;;

let tag2=enforce_localized_text text1;;

let tag3=enforce_localized_file ap1;;



*)