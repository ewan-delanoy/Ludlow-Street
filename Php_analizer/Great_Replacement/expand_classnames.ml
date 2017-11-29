(*

#use"Php_analizer/Great_Replacement/expand_classnames.ml";;

*)

module Private=struct

let get_nspc cm x=
    try List.assoc x cm with _->[];;
 
 let analize_class_creations cm l_helpers text=
    let temp1=Unshadowed_appearance.all text ["new"] ([],1) in
    let temp2=Image.image (
      fun i->
        let j=Option.unpack(After.after_whites text (i+3)) in
        let k=Option.unpack(After.after_php_label text j) in
        let cname=Cull_string.interval text j (k-1) in
        let nspc=(
           try [List.assoc cname l_helpers] with
           _->get_nspc cm cname
        ) in
        (cname,nspc,j)
    ) temp1 in
    List.partition(fun (cname,nspc,i)->List.length(nspc)<>1) temp2;;

exception Bad_stringified_list of string*string;;

let parse_stringified_helpers  stringified_helpers=
     if stringified_helpers="" then [] else
     let temp1=Strung.split '@'  stringified_helpers in
     let temp2=Image.image (fun t->(t,Strung.split '#' t)) temp1 in
     Image.image(
        fun (t,l)->if List.length(l)<>2
                   then raise(Bad_stringified_list(t,stringified_helpers)) 
                   else (List.nth l 0,List.nth l 1)
     ) temp2;;

end;; 

exception Missing_helpers of (string*(string list)) list;;

let in_string cm stringified_helpers text=
    let  l_helpers=Private.parse_stringified_helpers stringified_helpers in
    let (bad_ones,good_ones)=Private.analize_class_creations cm l_helpers text in
    if bad_ones<>[]
    then raise(Missing_helpers(Image.image (fun (cname,nspc,i)->(cname,nspc) ) bad_ones))
    else
    let changes_to_be_made=Image.image (fun (cname,nspc,i)->
      let naive_nspc=List.hd nspc in
      let corrected_nspc=(if Substring.begins_with naive_nspc "\\" 
                          then naive_nspc^"\\"
                          else "\\"^naive_nspc^"\\" ) in
      (corrected_nspc,i) ) good_ones in
    Strung.insert_prefixes_at_indices  changes_to_be_made text;; 

(*

in_string ["py",["really\\hap"]] "" "123 new py 23";;

*)

let file cm stringified_helpers fn=
    let old_text=Io.read_whole_file fn in
    let new_text=in_string cm stringified_helpers old_text in
    Io.overwrite_with fn new_text;;

