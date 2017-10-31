(*

#use"Php_analizer/Great_Replacement/clean_duplicate_uses.ml";;

Works only on a previously standardized PHP text.


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
  if opt3<>Some(n) then None else
  if (Strung.get s n)<>';' then None else 
  Some(Cull_string.interval s i1 (i2-1));;

(*

extract_used_item "   use \\So\\Laid\\Back ;   ";;

*)  
  
let rec main_helper (graet,uses,da_ober)=
     match da_ober with
     []->String.concat "\n" (List.rev graet)
     |line::peurrest->
        (match extract_used_item line with
         None->main_helper(line::graet,uses,peurrest)
         |Some(item)->
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
   let (before_namespaces,items)=Nspc_split.decompose s in
   let new_items=Image.image(
      fun (a,b,c)->(a,in_namespace b,c)
   ) items in
   let temp2=Nspc_split.recompose (before_namespaces,new_items) in
   Marker.adjust_all_markers temp2;; 

let in_file ap=
    let old_text=Io.read_whole_file ap in
    let new_text=in_string old_text in
    Io.overwrite_with ap new_text;;


