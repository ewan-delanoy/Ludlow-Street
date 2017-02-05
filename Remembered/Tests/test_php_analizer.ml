(*

#use"Remembered/Tests/test_php_analizer.ml";; 

*)

let mem1=Memoized.make (More_unix.quick_complete_ls);;
let mem2=Memoized.make(fun s->Php_lexer.parse_file(Absolute_path.of_string s));;


let peggy ()=
   let temp1="/Users/ewandelanoy/Documents/Sites/Symblog/symblogproject" in
   let temp2=mem1 temp1 in
   let temp3=List.filter (fun s->Substring.ends_with s ".php") temp2 in
   let temp4=Explicit.image (fun s->(s,mem2 s)) temp3 in
   let temp5=Explicit.image (fun (s,l)->(s,Level_one.level_one l)) temp4 in
   let temp6=List.filter (fun (s,(_,z))->z<>[]) temp5 in
   let (s1,(_,temp12))=List.hd(temp6) in
   let temp7=big_head 60 temp12 in
   let temp8=image (fun x->Php_token.content(Positioned_php_token.fst x)) temp7 in
   let temp9=String.concat "" temp8 in
   let temp10=Cull_string.beginning (min(String.length temp9) 60) temp9 in
   let temp11=big_head 7 (image Positioned_php_token.fst temp7) in
   (s1,temp10,temp11,(fun ()->temp12));;
   
   
let g1=peggy ();;


let (_,_,_,g2)=g1;;


