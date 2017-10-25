(*

#use"Php_analizer/Great_Replacement/marker.ml";;

*)

exception Blind_marker_exn;;

let rec helper_for_blind_marker (k,inserted_item,graet,da_ober)=match da_ober with
   []->raise(Blind_marker_exn)
   |(j,s)::peurrest->
       if j=k
       then List.rev_append graet (inserted_item::(Image.image snd da_ober))
       else helper_for_blind_marker (k,inserted_item,s::graet,peurrest);;

let put_blind_marker_at_line_before_coloring k filename=
   let text=Io.read_whole_file filename in
   let temp1=Str.split (Str.regexp_string "\n") text in
   let temp2=Ennig.index_everything temp1 in
   let temp3=helper_for_blind_marker (k,"marker_here('Mark');",[],temp2) in
   let new_content=String.concat "\n" temp3 in
   Io.erase_file_and_fill_it_with_string filename new_content;;

let rec helper_for_coloured_marker (mark_count,graet,da_ober)=match da_ober with
   []->List.rev graet
   |(j,s)::peurrest->
       if Substring.begins_with s "marker_here("
       then let corrected_line=
              "marker_here("^(string_of_int(mark_count+1))^","^(string_of_int j)^");" in
            helper_for_coloured_marker (mark_count+1,corrected_line::graet,peurrest)
       else helper_for_coloured_marker (mark_count  ,s::graet,peurrest);;

let color_all_markers filename=
   let text=Io.read_whole_file filename in
   let temp1=Str.split (Str.regexp_string "\n") text in
   let temp2=Ennig.index_everything temp1 in
   let temp3=helper_for_coloured_marker (0,[],temp2) in
   let new_content=String.concat "\n" temp3 in
   Io.erase_file_and_fill_it_with_string filename new_content;;   

let put_marker_at_line k filename=
  (put_blind_marker_at_line_before_coloring k filename;
   color_all_markers filename);;   

let remove_all_markers filename=
   let temp1=Str.split (Str.regexp_string "\n") (Io.read_whole_file filename) in
   let temp2=List.filter (fun s->
    not(Substring.begins_with s "marker_here") ) temp1  in
   let new_text=String.concat "\n" temp2 in
   Io.erase_file_and_fill_it_with_string filename new_text;; 