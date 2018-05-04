(*

#use"Php_analizer/Great_Replacement/expand_fnctn_call.ml";;

*)

exception No_function_there;;

let ignore_type_hints s=
    let j=Substring.leftmost_index_of_in " " s in
    if j<1
    then s
    else Cull_string.trim_spaces(Cull_string.cobeginning j s);;

let remove_initial_ampersand s=
    if Substring.is_a_substring_located_at "&" s 1
    then Cull_string.cobeginning 1 s
    else s;;

let simplify s=remove_initial_ampersand(ignore_type_hints(Cull_string.trim_spaces s));;    

let parse_args s_args=
    let temp1=Str.split (Str.regexp_string ",") s_args in
    Image.image(
       fun t->
        let i=Substring.leftmost_index_of_in "=" t in
        if i<1
        then (simplify t,None)
        else (simplify(Cull_string.beginning (i-1) t),
              Some(
                Cull_string.trim_spaces(Cull_string.cobeginning i t)
              ))
             
    ) temp1;;
(*

parse_args "$u,$v,&$x,$w=83, array $amy = array()";;

*)

type parsed_fnctn={
    quality : string;
    name : string;
    args : (string*(string option)) list;
    content : string;
    returned_part: string; 
};;

let parse_fnctn s idx=
    let opt=First_pass_parse.fnctn s idx in
    if opt=None
    then raise(No_function_there)
    else
    let (i10,(i1,i2,i3,i4,i5,i6,i7,i8,i9))=Option.unpack opt in
    let fnctn_quality=(if i1<i2 then Cull_string.interval s i1 (i2-1) else "")
    and fnctn_name=Cull_string.interval s i5 (i6-1)
    and fnctn_args=Cull_string.interval s (i7+1) (i8-2)
    and first_content=Cull_string.interval s (i9+1) (i10-2) in
    let lines=Str.split (Str.regexp_string "\n") first_content in
    let lines2=Image.image Cull_string.trim_spaces lines in
    let lines3=List.filter (fun line->line<>"") lines2 in
    let second_content=String.concat "\n" lines3 in
    let (s1,s2)=Father_and_son.father_and_son second_content '\n' in
    let (final_content,returned_content)=(
         if s1="" then (s2,"") else
         if Substring.begins_with s2 "return"
         then (s1,Cull_string.trim_spaces (Cull_string.cobeginning 6 s2) )
         else (first_content,"")
    ) in
    {
        quality=fnctn_quality;
        name =fnctn_name;
        args =parse_args fnctn_args;
        content =final_content;
        returned_part=returned_content;
    };;



(*

parse_fnctn "private function amy($u,$v,$w=83) \n {for($x=7;x++) {a=b;} dann();} unt; " 1;; 
parse_fnctn "function amy($u,$v,$w=83) \n {for($x=7;x++) {a=b;} dann();} unt; " 1;; 
parse_fnctn "function amy($u,$v,$w=83) \n {for($x=7;x++) {a=b;} dann();\n return $b+$x;} unt; " 1;; 
parse_fnctn "\tpublic function get_all()\n\t{\n\t\t$this->load_config_file();\n\n\t\treturn $this->config_data;\n\t}" 1;;

*)

let analyse_terms_between_method_and_assignment s idx=
    let opt1=After.after_whites s idx in
    if opt1=None then Some("","") else 
    let i1=Option.unpack opt1 in
    let opt2=After.after_one_among_several [">-";"::"] s i1 in
    if opt2=None then None else 
    let n=String.length s in
    let opt3=After.after_whites s (i1+2) in
    if opt3=None then None else 
    let i3=Option.unpack opt3 in
    let rev_s=Strung.reverse s in
    Some(
     Cull_string.interval rev_s 1 (n+1-i3),
     Cull_string.interval rev_s (n-i1) (n+1-i1)
    );;

let analyse_terms_before_method s idx=
    let n=String.length s in
    let opt=Option.seek(fun j->(Strung.get s j)='=')(Ennig.ennig idx n) in
    let (computed_s,receiver)=
        (
            match opt with
            None->(s,"")
            |Some(j0)->
               let temp1=Cull_string.cobeginning j0 s in
               let temp2=Cull_string.trim_spaces temp1 in 
               (Cull_string.beginning (j0-1) s,
                Strung.reverse temp2)
        ) in
    match analyse_terms_between_method_and_assignment computed_s idx with
    None->None
    |Some(caller,methkind)->Some(receiver,caller,methkind);;
          

(*

analyse_terms_before_method "  >- mud :: mad >- muh" 1;;
analyse_terms_before_method "::mud>-madmuh" 1;;
analyse_terms_before_method "  >- mud :: mad >- muh = yggep$ " 1;;


*)

type decomposed_call={
    receiver : string;
    caller : string;
    method_kind : string;
    method_name : string;
    call_args : string list;
};;

let decompose_fnctn_call unreversed_s=
    let s=Strung.reverse unreversed_s in
    let n=String.length s in
    let i1=Substring.leftmost_index_of_in ")" s in
    if i1<1 then None else
    let i2=After.after_closing_character (')','(') s (i1+1,1) in
    let opt3=After.after_whites s i2 in
    if opt3=None then None else 
    let i3=Option.unpack opt3 in
    let opt4=After.after_star 
    Charset.php_label_nonfirst_letters s i3 in
    let (recipient,long_caller,meth,i4)=(
        match opt4 with
         Some(i)->(match analyse_terms_before_method s i with
                   None->("","","",i)
                   |Some(receiver,caller,methkind)->(receiver,caller,methkind,i)
                   )
        |None->("","","",n+1)
    ) in
    let temp1=Cull_string.interval unreversed_s (n+1-(i2-2)) (n+1-(i1+1)) in
    let temp2=Str.split (Str.regexp_string ",") temp1 in
    let temp3=Image.image Cull_string.trim_spaces temp2 in

    Some(
        {
            receiver = recipient;
            caller = Cull_string.trim_spaces long_caller;
            method_kind = meth;
            method_name = Cull_string.interval unreversed_s (n+1-(i4-1)) (n+1-i3);
            call_args = temp3;
        }    
    );;



(*

decompose_fnctn_call "   betty ( $u , $v+3 , 'abcde' )  ; ";;

decompose_fnctn_call "  amy -> und :: seine -> betty ( $u , $v+3 , 'abcde' )  ; ";;

decompose_fnctn_call " $admiral =  betty ( $u , $v+3 , 'abcde' )  ; ";;

*)

exception Bad_parameter_specification of string;;
exception Missing_caller_name;;

let reexpand_from_predecomposed_data 
  dec_fnctn dec_call=
  let temp1=Ennig.index_everything dec_fnctn.args 
  and temp2=dec_call.call_args in
  let m=List.length temp2 in
  let temp3=Option.filter_and_unpack(
     fun (j,(arg,default))->
       if j>m
       then (
              if default=None
              then raise(Bad_parameter_specification(arg))
              else Some(arg,Option.unpack default)
            )
       else let new_arg=List.nth temp2 (j-1) in
            if arg=new_arg
            then None
            else Some(arg,new_arg)
  ) temp1 in
  let temp4=Image.image (fun (arg,arg_val)->
    arg^" = "^arg_val^" ;") temp3 in
  let old_text=dec_fnctn.content in
  let caller_name=dec_call.caller in
  let new_text=(
      if not(Substring.is_a_substring_of "$this" old_text)
      then old_text
      else 
      if caller_name="" 
      then raise(Missing_caller_name)
      else Replace_inside.replace_inside_string
           ("$this",caller_name) old_text
  )  in
  let temp5=Str.split (Str.regexp_string "\n") new_text in
  let temp6=Image.image Cull_string.trim_spaces temp5 in    
  let temp7=List.filter (
    fun line->not(Substring.begins_with line "global ")
  )   temp6 in
  let receiver=dec_call.receiver in
  let temp8=(
    if receiver="" then [] else
    [receiver^" = "^(
        Replace_inside.replace_inside_string
        ("$this",caller_name) dec_fnctn.returned_part
    )]
  ) in
  String.concat "\n" (temp4@temp7@temp8);;

(*  

let dec_fnctn=parse_fnctn 
("function amy($u,$v,&$x,$w=47) {global $z;\nd+$u-$v;\nreturn $this->leeds(7,8);}") 1;;

let dec_call=Option.unpack(decompose_fnctn_call
" $regina =peggy->lee()->amy(45,$v,$x) ; ");;

let see=reexpand_from_predecomposed_data
  dec_fnctn dec_call;;

*)

exception Bad_call;;

let ef fnctn call=
     let dec_fnctn=parse_fnctn fnctn 1
     and opt_dec_call=decompose_fnctn_call call in
     if opt_dec_call=None
     then raise(Bad_call)
     else
     let dec_call = Option.unpack opt_dec_call in
     reexpand_from_predecomposed_data
     dec_fnctn dec_call;;




