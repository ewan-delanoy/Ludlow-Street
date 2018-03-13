(*

#use"Php_analizer/HRecognizer/hpacify_namespaces.ml";;

*)

let get_nspc_name s (i,j)=
    (* there are nine characters in the namespace keyword *)
    let k1=Strung.finder (fun c->
      not(List.mem c [' ';'\t';'\n';'\r'])
    ) s (i+9) in
    if Strung.get s k1='{'
    then ""
    else
    let k2=Strung.finder (fun c->
      List.mem c [' ';'\t';'\n';'\r';';';'{']
    ) s (k1+1) in
    Cull_string.interval s k1 (k2-1);;

(*

let tt s= get_nspc_name s (1,String.length s);; 

tt "namespace Butterfly\\Over\\The\\Sea   ; ";;
tt "namespace Butterfly\\Over\\The\\Sea; ";;
tt "namespace Butterfly\\Over\\The\\Sea {}";;
tt "namespace Butterfly\\Over\\The\\Sea{}";;
tt "namespace {}";;



*)

let label_for_namespace_beginning="namespace_beginning";;
let label_for_namespace_ending="namespace_ending";;

(*
A newer list of labels is to be found in the h pacify_classes module
*)

let old_list_of_labels=
    label_for_namespace_ending::
    label_for_namespace_beginning::(!(Hrecognize.old_list_of_labels));;

let braced_nspc_case s nspc_name (i,j)=
  let k1=Strung.finder (fun c->c='{') s i 
  and k2=Strung.backwards_finder (fun c->c='}') s j in
  let t=Cull_string.interval s (k1+1) (k2-1) in
  let temp1=Hrecognize.parse_all t in
  let temp2=
  Image.image (
    fun (lbl,(old_i,old_j))->
     ((nspc_name,lbl),(old_i+k1,old_j+k1))
  ) temp1 in
      (("",label_for_namespace_beginning),(i,k1))::
  (
      temp2@
      [(("",label_for_namespace_ending),(k2,j))]
  )
  ;;

exception Mixed_namespaces;;

let semicoloned_nspc_case nspc_name l=
    if List.exists(fun 
    (lbl,(i,j))->List.mem lbl 
      [
          Hrecognize.label_for_semicoloned_nspc;
          Hrecognize.label_for_braced_nspc
      ]
    ) l
    then raise(Mixed_namespaces)
    else 
    Image.image (
    fun (lbl,(i,j))->
     ((nspc_name,lbl),(i,j))
  ) l;;

let rec helper s (graet,da_ober)=
    match da_ober with
    []->List.rev graet
    |(lbl,(i1,j1))::peurrest->
      if lbl=Hrecognize.label_for_semicoloned_nspc 
      then let nspc_name=get_nspc_name  s (i1,j1) in
           let temp1=semicoloned_nspc_case nspc_name peurrest in
           List.rev_append  graet ((("",lbl),(i1,j1))::temp1)
      else
      if lbl=Hrecognize.label_for_braced_nspc
      then let nspc_name=get_nspc_name  s (i1,j1) in
           let temp1=braced_nspc_case s nspc_name (i1,j1) in
           helper s (List.rev_append  temp1 graet,peurrest)
      else helper s ((("",lbl),(i1,j1))::graet,peurrest);;


let pn s=
    let temp1=Hrecognize.parse_all s in
    helper s ([],temp1);;

(*

let s1="/*amy*/namespace Peggy{/*one*//*two*/}namespace Lee{/*three*//*four*/}";;

let res1=Image.image (
  fun ((name,lbl),(i,j))->((name,lbl),(i,j),Cull_string.interval s1 i j)    
) (pn s1);;

let s2="/*amy*/namespace Peggy;/*one*//*two*/";;

let res2=Image.image (
  fun ((name,lbl),(i,j))->((name,lbl),(i,j),Cull_string.interval s1 i j)    
) (pn s2);;

let u1=Hrecognize.parse_all s1;;
let u2=Hrecognize.parse_all s2;;

*)    