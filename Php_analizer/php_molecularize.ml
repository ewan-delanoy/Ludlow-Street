(*

#use"Php_analizer/php_molecularize.ml";;

*)



module Private=struct

exception Molecularization_exn;;  

let rec iterator_for_molecuralization
  (graet,da_ober)=match da_ober with
   []->((List.rev(graet)):Php_positioned_molecule_list.t)
  |a::peurrest->
      (
        let (tok,lxg)=a in
         match Php_blocker_name.seek_block_beginning tok with
         None->iterator_for_molecuralization((Php_molecule.of_token tok,lxg)::graet,da_ober)
        |Some(blckr)->
           let opt=Php_recognize_starting_block.rsb blckr da_ober in
           if opt=None then raise(Molecularization_exn) else
           let (cr,l2)=Option.unpack opt in
           let d=List.length(da_ober)-List.length(l2) in
           let l3=Listennou.big_head d da_ober in
           let mole=Php_molecule.block blckr l3 in
           iterator_for_molecuralization((mole,Php_char_range.unveil cr)::graet,l2)
      );;

end;;

let molecuralize l=Private.iterator_for_molecuralization ([],l);;
let demolecuralize (l:Php_positioned_molecule_list.t)=
    let temp1=Image.image Php_molecule.uniformize l in
    ((List.flatten temp1):Php_positioned_token_list.t);;



