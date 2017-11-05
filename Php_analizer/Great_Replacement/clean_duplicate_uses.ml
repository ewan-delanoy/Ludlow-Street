(*

#use"Php_analizer/Great_Replacement/clean_duplicate_uses.ml";;

*)

let extract_used_item old_s=
  let s=Cull_string.trim_spaces old_s in
  if not(Substring.begins_with s "use")
  then None
  else 
  let n=String.length s in
  let opt1=Option.seek(fun j->
      not(List.mem (Strung.get s j) [' ';'\t';'\r'] )
  )(Ennig.ennig 4 n) in
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
  let used_item=(Cull_string.interval s i1 (i2-1)) in
  if (opt3=Some(n))&&((Strung.get s n)=';') 
  then Some(used_item,None) 
  else
  let i3=Option.unpack opt3 in
  if not(Substring.is_a_substring_located_at "as " s i3)
  then None
  else 
  let opt4=Option.seek(fun j->
  not(List.mem (Strung.get s j) [' ';'\t';'\r'] )
  )(Ennig.ennig (i3+3) n) in
  if opt4=None then None else
  let i4=Option.unpack opt4 in
  let opt5=Option.seek(fun j->
  not(List.mem (Strung.get s j) Characters_in_namespace_name.chars )  
  )(Ennig.ennig i4 n) in
  if opt5=None then None else
  let i5=Option.unpack opt5 in
  let opt6=Option.seek(fun j->
  not(List.mem (Strung.get s j) [' ';'\t';'\r'] )
  )(Ennig.ennig i5 n) in
  let synonym=(Cull_string.interval s i4 (i5-1)) in
  if (opt6=Some(n))&&((Strung.get s n)=';') 
  then Some(used_item,Some(synonym)) 
  else None;;

(*

extract_used_item "   use \\So\\Laid\\Back ;   ";;
extract_used_item "   use \\So\\Laid\\Back    as Peaceful  ;   ";;

*)  
  
let rec main_helper (graet,uses,da_ober)=
     match da_ober with
     []->String.concat "\n" (List.rev graet)
     |line::peurrest->
        (match extract_used_item line with
         None->main_helper(line::graet,uses,peurrest)
         |Some(item,_)->
            if Ordered_string.elfenn item uses
            then main_helper(("// Duplicate : "^line)::graet,uses,peurrest)
            else
            let new_uses=Ordered_string.insert item uses in
            main_helper(line::graet,new_uses,peurrest)
        );;  


let in_namespace s=
    let temp1=Str.split (Str.regexp_string "\n") s in
    main_helper([],Ordered_string.empty_set,temp1);;  

(*

in_namespace "ab\n use Peggy ; \n use Bertrand ; \nuse Peggy;\n use Phoebe; ";;

*)



let in_string s=
   let dec_form=Nspc_split.decompose s in
   let before_namespaces=Nspc_decomposed_form.before_namespaces dec_form in
   match  Nspc_decomposed_form.namespacable dec_form with
   Some(text)->before_namespaces^"\n"^(in_namespace text)
   |None->
   let items=Nspc_decomposed_form.namespaced_parts dec_form in
   let new_items=Image.image(
       fun (a,b,c,d)->(a,in_namespace b,c,d)
    ) items in
    let new_dec_form=Nspc_decomposed_form.make 
        before_namespaces None new_items in
    Nspc_split.recompose new_dec_form;; 

let in_file ap=
    let old_text=Io.read_whole_file ap in
    let new_text=in_string old_text in
    Io.overwrite_with ap new_text;;


