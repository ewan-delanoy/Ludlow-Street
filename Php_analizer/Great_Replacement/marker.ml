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

let put_blind_marker_at_line k text=
        let temp1=Str.split (Str.regexp_string "\n") text in
        let temp2=Ennig.index_everything temp1 in
        let temp3=helper_for_blind_marker (k,"marker_here('Mark');",[],temp2) in
        String.concat "\n" temp3;;       

let rec helper_for_adjusted_marker (mark_count,graet,da_ober)=match da_ober with
   []->List.rev graet
   |(j,s)::peurrest->
       if Substring.begins_with s "marker_here("
       then let corrected_line=
              "marker_here("^(string_of_int(mark_count+1))^","^(string_of_int j)^");" in
            helper_for_adjusted_marker (mark_count+1,corrected_line::graet,peurrest)
       else helper_for_adjusted_marker (mark_count  ,s::graet,peurrest);;

let adjust_all_markers text=
   let temp1=Str.split (Str.regexp_string "\n") text in
   let temp2=Ennig.index_everything temp1 in
   let temp3=helper_for_adjusted_marker (0,[],temp2) in
   String.concat "\n" temp3;;   

let put_marker_at_line k text=
  adjust_all_markers (put_blind_marker_at_line k text);;   

let remove_all_markers text=
   let temp1=Str.split (Str.regexp_string "\n") text in
   let temp2=List.filter (fun s->
    not(Substring.begins_with s "marker_here") ) temp1  in
   String.concat "\n" temp2;; 

let put_marker_at_line_in_file k filename=
    let old_text=Io.read_whole_file filename in
    let new_text=put_marker_at_line k old_text in
    Io.overwrite_with filename new_text;;

let remove_all_markers_in_file filename=
      let old_text=Io.read_whole_file filename in
      let new_text=remove_all_markers old_text in
      Io.overwrite_with filename new_text;;    



