(*

#use"hexdump.ml";;

*)

(***************************************************************************
Freely changed version of 
https://github.com/pchapin/hexdump/blob/master/OCaml/hexdump.ml
by Peter C. Chapin
****************************************************************************)


(** Formats a string as a sequence of hex values (functional style).
    @param buffer The string to be formatted.
    @param count The number of interesting characters in the buffer.
    @return a string consisting of the hex representations of the characters in the buffer.
    The string is padded out with spaces to account for the junk characters in the buffer.
*)
let hex_format buffer count =
    let rec process_buffer buffer ch_number =
        let size = String.length buffer in
        if size = 0 then "" else
            let head = buffer.[0] in
            let tail = String.sub buffer 1 (size - 1) in
            let extra = if ch_number = 8 then "" (*"_PAD1"*) else "" in
                if ch_number <= count then
                    (Printf.sprintf "%02X " (Char.code head)) ^ 
                    extra ^ 
                    (process_buffer tail (ch_number + 1))
                else
                    "" (* "_PAD2" *) ^ extra ^ process_buffer tail (ch_number + 1)
    in process_buffer buffer 1;;


let hexdump_file file_name = 
    let ifile  = open_in_bin file_name in
    let buffer = Bytes.create 16 in
    let count  = ref (-1) in
    let offset = ref 0 in
    let accu   = ref "" in
    let _=(begin
            while (count := input ifile buffer 0 16; !count) <> 0 do
                accu :=(!accu)^(hex_format buffer !count);
                offset := !offset + !count
            done;
            close_in ifile
        end) in
    (!accu);;





