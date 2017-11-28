(*

#use"Php_analizer/Great_Replacement/nspc_inflate.ml";;

*)

let namespacable_part 
 (nspc_line,nspc_content,nspc_offset,after_nspc)=
let fixed_content=(
      if Substring.ends_with nspc_content "\n\n\n"
      then nspc_content
      else nspc_content^"\n\n\n"
) in
(nspc_line,fixed_content,nspc_offset,after_nspc);;

let decomposed_form x=
      let old_parts=Nspc_decomposed_form.namespaced_parts x in
      let new_parts=Image.image namespacable_part old_parts in
      Nspc_decomposed_form.make
       (Nspc_decomposed_form.before_namespaces x)
       (Nspc_decomposed_form.namespacable x)
        new_parts;;

let string s=
      let dec_form=Nspc_split.decompose s in
      let new_dec_form=decomposed_form dec_form in
      Nspc_split.recompose new_dec_form;;
        
let file fn=
      let old_text=Io.read_whole_file fn in
      let new_text=string old_text in
      Io.overwrite_with fn new_text;;
                