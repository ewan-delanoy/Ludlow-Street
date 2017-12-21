(*

#use"Php_analizer/Great_Replacement/ivy_aware_inflate.ml";;

*)

let counter=ref(0);;

let access_counter ()=
     let j=(!counter)+1 in
     let _=(counter:=j) in
     j;;

let item x=
     let kind =Ivy_aware_item.kind x in
     if kind=Ivy_aware_kind.non_ivy 
     then x
     else let j=access_counter () in
          let inflator=Ivy_aware_marker.inflator_of_int j in 
         Ivy_aware_item.make
           kind
           (Ivy_aware_item.before_content x)
           (inflator^(Ivy_aware_item.content x))
           (Ivy_aware_item.after_content x);;

let string s=
   let _=(counter:=0) in 
   let temp1=Ivy_aware_decomposition.on_string s in
   let temp2=Image.image (fun x->
    Ivy_aware_item.full_content(item x)) temp1 in
   String.concat "" temp2;;  

let file fn=
      let old_text=Io.read_whole_file fn in
      Io.overwrite_with fn (string old_text);;
      

(*

string "123 if(uvw)  {xyz} else {ikj} 456";;

*)   