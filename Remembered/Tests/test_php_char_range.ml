(*

#use"Remembered/Tests/test_php_char_range.ml";; 

*)

let find_left_position_in_list lpos l=
   Three_parts.select_center_element
   (fun ptok->fst(Positioned_php_token.snd(ptok))=lpos) l;;
   
let find_right_position_in_list rpos l=
   Three_parts.select_center_element
   (fun ptok->snd(Positioned_php_token.snd(ptok))=rpos) l;;   
   
let find_interval_in_list (lpos,rpos) l=
    let (before1,a,after1)=find_left_position_in_list lpos l in
    let (before2,b,after2)=find_left_position_in_list rpos after1 in
    (Option.unpack a)::(before2@[Option.unpack b]);;


let mem1=Memoized.make (More_unix.quick_complete_ls);;
let mem2=Memoized.make(fun s->Php_lexer.parse_file(Absolute_path.of_string s));;

let recompute_level_one_item (s,item)=
   let cr=Level_one.char_range item in
   let (lpos,rpos)=(Php_char_range.fst cr,Php_char_range.snd cr) in
   let itv=find_interval_in_list (lpos,rpos) (mem2 s) in
   Level_one.level_one itv;;
   

let check_level_one_item (s,item)=
  ((recompute_level_one_item (s,item))=([item],[]));;
  


let peggy1=Memoized.make(fun ()->
   let temp1="/Users/ewandelanoy/Documents/Sites/Symblog/symblogproject" in
   let temp2=mem1 temp1 in
   let temp3=List.filter (fun s->Substring.ends_with s ".php") temp2 in
   let temp4=Explicit.image (fun s->(s,mem2 s)) temp3 in
   let temp5=Explicit.image (fun (s,l)->(s,Level_one.level_one l)) temp4 in
   temp5);;
   
let haag1=peggy1();;
let haag2=image (fun (s,(treated,_))->image (fun t->(s,t)) treated) haag1;; 
let haag3=List.flatten haag2;; 
 
let result=List.filter(
  fun t->not( check_level_one_item t)
) haag3;;
   


