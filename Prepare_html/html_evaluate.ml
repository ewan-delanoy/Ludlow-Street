(*

#use"Prepare_html/html_evaluate.ml";;

*)


let eval brcb_table l_param s= 
  let temp1=Html_decompose_into_bracables.dec brcb_table s in
  let temp2=Image.image (Html_bracable.eval l_param) temp1 in
  String.concat "" temp2;;
  
  