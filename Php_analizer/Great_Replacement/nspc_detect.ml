(*

#use"Php_analizer/Great_Replacement/nspc_detect.ml";;

*)


let extract_namespace_name old_s=
    let s=Cull_string.trim_spaces old_s in
    if not(Substring.begins_with s "namespace")
    then None
    else 
    let n=String.length s in
    let opt1=Option.seek(fun j->
        not(List.mem (Strung.get s j) [' ';'\t';'\r'] )
    )(Ennig.ennig 10 n) in
    if opt1=None then None else
    let i1=Option.unpack opt1 in
    let opt2=Option.seek(fun j->
        not(List.mem (Strung.get s j) Characters_in_namespace_name.chars )
    )(Ennig.ennig i1 n) in
    if opt2=None then None else
    let i2=Option.unpack opt2 in
    let opt3=Option.seek(fun j->
    not(List.mem (Strung.get s j) [' ';'\t';'\r'] )
    )(Ennig.ennig i2 n) in
    if opt3<>Some(n) then None else
    let c=Strung.get s n 
    and nahme=Cull_string.interval s i1 (i2-1) in
    if c='{' then Some(nahme,true) else
    if c=';' then Some(nahme,false) else
    None;;
    
(*

extract_namespace_name "  not_a_namespace";;
extract_namespace_name "  namespace 345\\789\\123\\56    ; " ;;
extract_namespace_name "  namespace 345\\789\\123\\56    { " ;;
extract_namespace_name "  namespace                      { " ;;
extract_namespace_name "  namespace { " ;;

*)

let test_for_namespace_line s=(extract_namespace_name(s)<>None);;

let test_for_declaration_line old_s=
  let s=Cull_string.trim_spaces old_s in
  if not(Substring.begins_with s "declare")
  then false
  else 
  let n=String.length s in
  let opt1=Option.seek(fun j->
      not(List.mem (Strung.get s j) [' ';'\t';'\r'] )
  )(Ennig.ennig 8 n) in
  if opt1=None then false else
  let i1=Option.unpack opt1 in
  if Strung.get(s)(i1)<>'(' then false else
  let opt2=Option.seek(fun j->
      (Strung.get s j)=')'
  )(Ennig.ennig (i1+1) n) in
  if opt2=None then false else
  let i2=Option.unpack opt2 in
  let opt3=Option.seek(fun j->
  not(List.mem (Strung.get s j) [' ';'\t';'\r'] )
  )(Ennig.ennig (i2+1) n) in
  if opt3<>Some(n) then false else
  (Strung.get s n)=';' ;;

(*  
test_for_declaration_line " declare( obla-di-obla-da ) ;   " ;; 
*)
