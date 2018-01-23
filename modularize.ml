
let module_name_from_path ap=
      let s_ap=Absolute_path.to_string ap in
      let pointed_name=Father_and_son.son s_ap '/' in
      let name=Father_and_son.invasive_father pointed_name '.' in
      Naked_module.of_string name;;


let modularize prefix ap=
      let naked_name=module_name_from_path ap in
      let name=Naked_module.to_string naked_name in  
      let content=Io.read_whole_file(ap) in
      let new_name=String.capitalize_ascii(prefix^name) in
      let new_content=
       "\n\nmodule "^new_name^"=struct\n\n"^content^"\n\nend;"^";\n\n" in
     new_content;;
     
     
    
 
