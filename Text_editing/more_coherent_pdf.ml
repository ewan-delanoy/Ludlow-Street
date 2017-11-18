(*

#use"more_coherent_pdf.ml";;

Utility to cut a large PDF into easily printable chunks.
Each chunk is at most 20 pages long, and separated into
odd/even parts to allow for recto-verso printing. 

*)


type source={
   root_directory :string;
   pdffile_name   :string;
   page_interval  :int*int;
   chunk_size     :int;
};;

let initialize ~rootdir ~pdfname ~interval ~chunksize={
   root_directory =(if Substring.ends_with rootdir "/" then rootdir else rootdir^"/");
   pdffile_name   =(if Substring.ends_with pdfname ".pdf" then pdfname else pdfname^".pdf");
   page_interval  =interval;
   chunk_size    =(match chunksize with None->20 |Some(l)->l);
};;    



module Chunking=struct

let individual_act x (act_description,act_output)=
    let cmd1="/Applications/cpdf "^x.root_directory^"Coherent_PDF/"^act_description^
              " -o "^act_output^".pdf" in
    let cmd2="mv "^act_output^".pdf "^x.root_directory^"Coherent_PDF/" in
    [cmd1;cmd2];;

let individual_command x (i,j)=
    let si=string_of_int i and sj=string_of_int j in
    List.flatten(
    Image.image (fun (a,b)->individual_act x (a,b))
    [
      (x.pdffile_name^" "^si^"-"^sj,"from_"^si^"_to_"^sj);
      ("from_"^si^"_to_"^sj^".pdf even","from_"^si^"_to_"^sj^"_even");
      ("from_"^si^"_to_"^sj^".pdf odd","from_"^si^"_to_"^sj^"_odd");
    ]
    );;

let prepare_premises x=
   ["mkdir -p "^x.root_directory^"Coherent_PDF/";
    "cp "^x.root_directory^x.pdffile_name^" "^x.root_directory^"Coherent_PDF/"];;

let list_of_commands x=
   let (i,j)=x.page_interval 
   and d=x.chunk_size in
   let r=(j-i)/d in
   let intervals=Ennig.doyle (fun k->(i+(k-1)*d,min(i+k*d-1)(j))) 1 (r+1) in
   List.flatten(
    (prepare_premises x)::(Image.image (individual_command x) intervals)
    );; 

end;;

let chunk ~rootdir ~pdfname ~interval ~chunksize=
  let worker=initialize ~rootdir ~pdfname ~interval ~chunksize in
  let cmds=Chunking.list_of_commands worker in
  Explicit.image Unix_command.uc cmds;;

let merge ~rootdir ~pdfname ~interval=
   let (i,j)=interval in
   let temp1=Ennig.doyle (fun k->
      pdfname^"_"^(string_of_int k)^".pdf" ) i j in
   let temp2=String.concat " " temp1 in   
   let cmd="/Applications/cpdf "^temp2^" -o "^pdfname^".pdf" in
   let old_dir=Sys.getcwd() in
   (
     Sys.chdir rootdir;
     let _=Unix_command.hardcore_uc cmd in
     Sys.chdir old_dir;
   );;
   
(*

let example=merge 
 ~rootdir:"/Users/ewandelanoy/Documents/Firpo"
 ~pdfname:"charinq"
 ~interval:(1,315);;




let example=initialize
   (
     "/Users/ewandelanoy/Documents/html_files/Ricciotti/",
     "plain_pages.pdf",
     1,511
     ,
     None
   );;

let example=initialize
   (
     "/Users/ewandelanoy/Documents/html_files/Wilhelm/",
     "ww.pdf",
     1,431
     ,
     None
   );;

let result1=list_of_commands example;;   
let result2=Explicit.image Unix_command.uc result1;;

*)

