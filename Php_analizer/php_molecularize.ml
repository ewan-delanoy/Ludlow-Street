(*

#use"Php_analizer/php_molecularize.ml";;

*)



module Private=struct

exception Molecularization_exn;;  

exception Pusher_exn;; 

let rec pusher_for_molecularization
(graet,da_ober)=match da_ober with
 []->raise(Pusher_exn)
|a::peurrest->
    (
      let (tok,lxg)=a in
       match Php_token.seek_block_beginning tok with
       None->((Php_molecule.of_token tok,lxg)::graet,da_ober)
      |Some(blckr)->
         let opt=Php_recognize_starting_block.rsb blckr da_ober in
         if opt=None then raise(Molecularization_exn) else
         let (cr,l2)=Option.unpack opt in
         let d=List.length(da_ober)-List.length(l2) in
         let l3=Listennou.big_head d da_ober in
         let mole=Php_molecule.block blckr l3 in
         ((mole,Php_char_range.unveil cr)::graet,l2)
    );;

let rec iterator_for_molecularization
  (graet,da_ober)=match da_ober with
   []->((List.rev(graet)):Php_positioned_molecule_list.t)
  |a::peurrest->
      iterator_for_molecularization(pusher_for_molecularization(graet,da_ober));;

end;;

let molecularize l=Private.iterator_for_molecularization ([],l);;
let demolecularize (l:Php_positioned_molecule_list.t)=
    let temp1=Explicit.image Php_molecule.uniformize l in
    ((List.flatten temp1):Php_positioned_token_list.t);;



