(*

#use"Php_analizer/HRecognizer/parametric_hrecognize.ml";;

The returned list of indexes starts with the initial index
and stops just before the "last_i" index.

*)



let rec chain_in_detail (s,i_start,current_i,graet,da_ober)=
  match da_ober with
  []->(Some(current_i,List.rev graet),None)
  |atom::peurrest->
    match Atomic_hrecognize.recgnz atom s current_i with
    None->(None,Some(List.rev graet,atom,current_i,peurrest)) 
    |Some(j)->chain_in_detail (s,i_start,j,current_i::graet,peurrest);;

let chain label l_atoms s i=
  let (opt1,_)=chain_in_detail (s,i,i,[],l_atoms) in
  match opt1 with
  Some(last_i,indexes)->Some(label,indexes,last_i)
  |None->None;;    

