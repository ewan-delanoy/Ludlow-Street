(*

#use"Php_analizer/Great_Replacement/functionlike_decomposition.ml";;


*)


let determine_exact_beginning s i_fnctn=
    let opt=Option.seek(
       fun t->not(List.mem 
         (Strung.get s (i_fnctn-t))
         [' ';'\n';'\r';'\t']
       )
    ) (Ennig.ennig 1 (i_fnctn-1)) in
    if opt=None then i_fnctn else
    let j=i_fnctn-(Option.unpack opt) in
    match Option.seek(fun
       kwd->
        Substring.is_a_substring_located_at kwd s (j-(String.length kwd)+1)
    ) ["abstract";"public";"private";"protected"] with
     None->i_fnctn
    |Some(kwd0)->
     let i2_fnctn=j-(String.length kwd0)+1 in
     if kwd0="abstract" then i2_fnctn else
     let opt2=Option.seek(
      fun t->not(List.mem 
        (Strung.get s (i2_fnctn-t))
        [' ';'\n';'\r';'\t']
      )
   ) (Ennig.ennig 1 (i2_fnctn-1)) in
   if opt2=None then i2_fnctn else
   let j2=i2_fnctn-(Option.unpack opt2) in
   if Substring.is_a_substring_located_at "abstract" s (j2-7)
   then j2-7
   else j2+1;;
    
(*

determine_exact_beginning "123private   function" 14;;
determine_exact_beginning "123public    function" 14;;
determine_exact_beginning "123protected function" 14;;
determine_exact_beginning "123          function" 14;;
determine_exact_beginning "123abstract  function" 14;;
determine_exact_beginning "123abstract private function" 21;;
determine_exact_beginning "123abstract \t private \n function" 23;;

*)    

let reference_for_subtle_case=ref("",0,0);;
exception Subtle_case;;
exception Unknown_qualifier of string;;



let abstract_case s idx jdx opt2 class_name nspc_name graet=
  let (i9,(i0,i1,i2,i3,i4,i5,i6,i7,i8))=Option.unpack opt2 in
  let kind=(
      let s_qualifier=Cull_string.interval s i1 (i2-1) in
      if List.mem s_qualifier ["";"protected"] then Functionlike_kind.abstract_protected_method else
      if s_qualifier="private" then Functionlike_kind.abstract_private_method else
      if s_qualifier="public" then Functionlike_kind.abstract_public_method else
      raise(Unknown_qualifier(s_qualifier))
  ) 
  and fn_name=Cull_string.interval s i5 (i6-1) in
  let item2=Functionlike_item.make 
     kind
     nspc_name 
     class_name
     fn_name
     (Cull_string.interval s jdx (i9-1))
     ""
     (Cull_string.interval s i9 i9)
  in
  if jdx=idx
  then (item2::graet, i9+1) 
  else let item1=
        Functionlike_item.non_function nspc_name class_name
         (Cull_string.interval s idx (jdx-1)) in
       (item2::item1::graet, i9+1);;

let nonabstract_case s idx jdx opt class_name nspc_name graet=
  let (i10,(i1,i2,i3,i4,i5,i6,i7,i8,i9))=Option.unpack opt in
  let kind=(
      if class_name="" then Functionlike_kind.usual_function else
      let s_qualifier=Cull_string.interval s i1 (i2-1) in
      if List.mem s_qualifier ["";"protected"] then Functionlike_kind.protected_method else
      if s_qualifier="private" then Functionlike_kind.private_method else
      if s_qualifier="public" then Functionlike_kind.public_method else
      raise(Unknown_qualifier(s_qualifier))
  ) 
  and fn_name=Cull_string.interval s i5 (i6-1) in
  let item2=Functionlike_item.make 
     kind
     nspc_name 
     class_name
     fn_name
     (Cull_string.interval s jdx i9)
     (Cull_string.interval s (i9+1) (i10-2))
     (Cull_string.interval s (i10-1) (i10-1))
  in
  if jdx=idx
  then (item2::graet, i10) 
  else let item1=
        Functionlike_item.non_function nspc_name class_name
         (Cull_string.interval s idx (jdx-1)) in
       (item2::item1::graet, i10);;


let on_class_contained_text nspc_name class_name s=
   let n=String.length s in
   let rec tempf=(
     fun (graet,idx)->
       if idx>n
       then List.rev graet
       else 
       match Unshadowed_appearance.next 
        s ["function"] idx with
       None->
           let last_item=
              Functionlike_item.non_function nspc_name class_name 
                (Cull_string.interval s idx n) in
              List.rev (last_item::graet)    
       |Some(old_jdx)->
            let jdx=determine_exact_beginning s old_jdx in
            let pair=(
              let opt=First_pass_parse.fnctn s jdx in
              if opt=None
              then let opt2=First_pass_parse.abstract_fnctn s jdx in
                   if opt2=None
                   then (reference_for_subtle_case:=(s,jdx,old_jdx);raise(Subtle_case))
                   else abstract_case s idx jdx opt2 class_name nspc_name graet
              else nonabstract_case s idx jdx opt class_name nspc_name graet     
            ) in
            tempf pair
   ) in
   tempf([],1);; 
           

(*

on_class_contained_text "NPX" "myclass"
("private function peggy ($u,$v) {567;} "^
"public function arthur ($a,$b) {765;}"^
"function betty($g,$h) {678;} 098");;

*)

exception Untreated of Classlike_kind.t;;

let on_classlike_item ci=
  let kind=Classlike_item.kind ci in
  if kind=Classlike_kind.interface
  then [Functionlike_item.interface ci]
  else
  let content=Classlike_item.content ci in
  if kind=Classlike_kind.namespace_line
  then [Functionlike_item.namespace_line content]
  else 
  if kind=Classlike_kind.after_namespace_comments
  then [Functionlike_item.after_namespace_comments content]
  else 
  let main=on_class_contained_text 
     (Classlike_item.namespace ci)
     (Classlike_item.class_name ci) 
     content in
  if kind=Classlike_kind.plain_text
  then main
  else  
  if List.mem kind 
     [
      Classlike_kind.abstract_class;
      Classlike_kind.final_class;
      Classlike_kind.usual_class  
     ] 
  then (Functionlike_item.class_opening ci)::(main@
       [Functionlike_item.class_closing ci])
  else raise(Untreated(kind));;   
 
let on_string s=
   let temp1=Classlike_decomposition.on_string s in
   let temp2=Image.image on_classlike_item  temp1 in
   List.flatten temp2;;