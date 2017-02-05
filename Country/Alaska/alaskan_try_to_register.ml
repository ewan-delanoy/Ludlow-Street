
(* 

#use"Country/Alaska/alaskan_try_to_register.ml";;

*)



let mlx_file mdata mlx_file=
    try(Some(Alaskan_register_mlx_file.on_monitored_modules 
        mdata mlx_file)) with _->None;;  

let mlx_files mdata mlx_files=
   let rec tempf=(fun
    (vdata,failures,yet_untreated)->
      match yet_untreated with
      []->(failures,vdata)
      |mlx::others->
      (
        match mlx_file vdata mlx with
        None->tempf(vdata,mlx::failures,others)
        |Some(nfs)->tempf(nfs,failures,others)
      )
   ) in
   tempf(mdata,[],mlx_files);;
 
  
   