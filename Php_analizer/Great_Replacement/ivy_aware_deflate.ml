(*

#use"Php_analizer/Great_Replacement/ivy_aware_deflate.ml";;

*)



let item l x=
     let kind =Ivy_aware_item.kind x in
     if kind=Ivy_aware_kind.non_ivy 
     then Ivy_aware_item.full_content x
     else let main_content=Ivy_aware_item.content x in
          let j=Ivy_aware_marker.int_of_inflator main_content in
          if Tidel.elfenn j l
          then Cull_string.cobeginning Ivy_aware_marker.length main_content
          else "";;
          
let string l s=
   let temp1=Ivy_aware_decomposition.on_string s in
   let temp2=Image.image (item l) temp1 in
   String.concat "" temp2;;  

(*

string "123 if(uvw)  {xyz} else {ikj} 456";;

*)   