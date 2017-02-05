(*

#use"current_date.ml";;

*)

let current_date ()=
  let module Unix_again=(struct
   include Unix;;
   let day x=x.tm_mday;;
   let month x=x.tm_mon;;
   let year x=x.tm_year;;
  end) in
  let temp=Unix.gmtime(Unix.time()) in
  let year=string_of_int((Unix_again.year temp)+1900)
  and month1=string_of_int((Unix_again.month temp)+1)
  and day1=string_of_int(Unix_again.day temp) in
  let month=Cull_string.resize_from_right month1 2 '0'
  and day=Cull_string.resize_from_right day1 2 '0' in
  year^"_"^month^"_"^day;;

