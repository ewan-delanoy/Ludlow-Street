
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
     
let modularize_several prefix l_ap=     
    let temp1=Image.image module_name_from_path l_ap in
    let replacements = Image.image (
        fun x->match x with
        (Naked_module.N(name))->
        let new_name=String.capitalize_ascii(prefix^name) in
        (x,Naked_module.N(new_name))
    ) temp1 in
    let temp2=Explicit.image (modularize prefix) l_ap in
    let unreplaced_text=String.concat "\n\n\n" temp2 in
    Look_for_module_names.change_several_module_names_in_string
      replacements unreplaced_text;;  
 
