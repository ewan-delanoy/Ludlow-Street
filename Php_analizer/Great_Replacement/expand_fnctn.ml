(*

#use"Php_analizer/Great_Replacement/expand_fnctn.ml";;

*)

exception No_function_there;;

let parse_args s_args=
    let temp1=Str.split (Str.regexp_string ",") s_args in
    Image.image(
       fun t->
        let i=Substring.leftmost_index_of_in "=" t in
        if i<1
        then (Cull_string.trim_spaces t,None)
        else (Cull_string.trim_spaces(Cull_string.beginning (i-1) t),
              Some(
                Cull_string.trim_spaces(Cull_string.cobeginning i t)
              ))
             
    ) temp1;;
(*

parse_args "$u,$v,$w=83";;

*)

type parsed_fnctn={
    quality : string;
    name : string;
    args : (string*(string option)) list;
    content : string;
};;

let parse_fnctn s idx=
    let opt=After.after_fnctn s idx in
    if opt=None
    then raise(No_function_there)
    else
    let (i9,(i1,i2,i3,i4,i5,i6,i7,i8))=Option.unpack opt in
    let fnctn_quality=(if i1<i2 then Cull_string.interval s i1 (i2-1) else "")
    and fnctn_name=Cull_string.interval s i4 (i5-1)
    and fnctn_args=Cull_string.interval s (i6+1) (i7-2)
    and fnctn_content=Cull_string.interval s (i8+1) (i9-2) in
    {
        quality=fnctn_quality;
        name =fnctn_name;
        args =parse_args fnctn_args;
        content =fnctn_content;
    };;



(*

parse_fnctn "private function amy($u,$v,$w=83) \n {for($x=7;x++) {a=b;} dann();} unt; " 1;; 
parse_fnctn "function amy($u,$v,$w=83) \n {for($x=7;x++) {a=b;} dann();} unt; " 1;; 


*)


let decompose_fnctn_call s=
    let reversed_s=Strung.reverse s in
    let i1=Substring.leftmost_index_of_in ")" s in
    

