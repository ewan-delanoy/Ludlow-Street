(*

#use"Php_analizer/HRecognizer/hpacify_classes.ml";;

*)

let detect_keyword_surrounded_by_whites kwd s i=
   let m=String.length kwd in
   if not(List.mem(String.get s (i-2)) [' ';'\n';'\r';'\t'])
   then false
   else
   if (String.length s)<i+m
   then false
   else
   if not(List.mem(String.get s (i+m-1)) [' ';'\n';'\r';'\t'])
   then false
   else (String.sub s (i-1) m)=kwd;;

let seek_keyword_surrounded_by_whites kwd s a b=
   let m=String.length kwd in
   let range=Ennig.ennig (a+1) (b-m) in
   Option.seek (detect_keyword_surrounded_by_whites kwd s) range;;

(*

seek_keyword_surrounded_by_whites "gelfand" " gelfand " 1 9;;

*)

let parse_comma_separated_words s x y=
  let t=Cull_string.interval s x y in
  let temp1=Str.split (Str.regexp_string ",") t in
  Image.image Cull_string.trim_spaces temp1;;

(*

let s1=" hobby   , against , peter  ";;
parse_comma_separated_words s1 1 (String.length s1);;


*)


let get_class_info s (i,j)=
    (* j+1 is the index at which the left brace appears *)
    (* the first word is either "class" or "interface" *)
    let k1=Strung.finder (fun c->
      not(List.mem c [' ';'\n';'\r';'\t'])
    ) s i in
    let opt1=seek_keyword_surrounded_by_whites "extends" s k1 j in
    let opt2=seek_keyword_surrounded_by_whites "implements" s k1 j in
    let (implemented_ones,bound_for_extended_ones)=(
       match opt2 with
       None->([],j)
       |Some(k3)->
         (* The implements word is 10 characters long *)
         (parse_comma_separated_words s (k3+10) j,k3-2)
    ) in
    let (extended_ones,bound_for_name)=(
       match opt1 with
       None->([],bound_for_extended_ones)
       |Some(k2)->
         (* The extends word is 7 characters long *)
         (parse_comma_separated_words s (k2+7) bound_for_extended_ones,k2-2)
    ) in
    let name=Cull_string.trim_spaces(Cull_string.interval s k1 bound_for_name) in
    (name,extended_ones,implemented_ones);;

(*

let tt s= get_class_info s (1,String.length s);; 

tt " baby come back extends butterfly , over , the , sea implements dog , hand , cat";;
tt " baby come back extends butterfly , over , the , sea ";;
tt " baby come back implements dog , hand , cat";;
tt " baby come back";;


*)

let label_for_class_beginning="class_beginning";;
let label_for_class_ending="class_ending";;

let list_of_labels=
    label_for_class_ending::
    label_for_class_beginning::(Hpacify_namespaces.old_list_of_labels);;

let class_related_labels=
      [
        Hrecognize.label_for_abstract_glass;
        Hrecognize.label_for_glass;
        Hrecognize.label_for_final_glass;
        Hrecognize.label_for_itrfc
      ];;

     
let pacify s ((nspc_name,lbl),(i,j))=
  if not(List.mem lbl class_related_labels)
  then [ (nspc_name,"",[],[],lbl),(i,j) ]
  else 
  let k1=Substring.leftmost_index_of_in_from "{" s i in
  let (cl_name,cl_extended,cl_implemented)=get_class_info s (i,k1-1) in
  let t=Cull_string.interval s (k1+1) (j-1) in
  let temp1=Hrecognize.parse_all t in
  let temp2=Image.image (
    fun (lbl,(old_i,old_j))->
     ((nspc_name,cl_name,cl_extended,cl_implemented,lbl),(old_i+k1,old_j+k1))
  ) temp1 in
      ((nspc_name,"",[],[],label_for_class_beginning ),(i,k1))::
  (
      temp2@
      [((nspc_name,"",[],[],label_for_class_ending),(j,j))]
  )
  ;;

  


let pc s=
    let temp1=Hpacify_namespaces.pn s in
    let temp2=Image.image (pacify s) temp1 in
    List.flatten temp2;;

