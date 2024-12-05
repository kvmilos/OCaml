let read_file file =
  let in_channel = open_in file in
  let content = really_input_string in_channel (in_channel_length in_channel) in
  close_in in_channel;
  content;;

let write_file file content =
  let out_channel = open_out file in
  output_string out_channel content;
  close_out out_channel;;

let modify_content content =
  let length = String.length content in
  let result = Bytes.of_string content in
  for i = 0 to length - 1 do
    match Bytes.get result i with
    | 'a' -> Bytes.set result i 'b'
    | 'b' -> Bytes.set result i 'a'
    | 'A' -> Bytes.set result i 'B'
    | 'B' -> Bytes.set result i 'A'
    | _ -> ()
  done;
  Bytes.to_string result;;

let count_characters file =
  let counts = Array.make 256 (0, ref 0) in
  for i = 0 to 255 do
    counts.(i) <- (i, ref 0)
  done;
  let in_channel = open_in file in
  (try
     while true do
       let char = input_char in_channel in
       let index = Char.code char in
       let (_, count_ref) = counts.(index) in
       count_ref := !count_ref + 1
     done
   with End_of_file -> ());
  close_in in_channel;
  counts;;

let print_character_table counts =
  let char_to_display code =
    Char.escaped (Char.chr code) (* Dla poprawnego wyświetlania \n, \t itd. *)
  in
  let pp_header fmt () =
    Format.fprintf fmt "@[<v>| Char  | Count |@,|-------+-------|@,@]"
  in
  let pp_row fmt (code, count_ref) =
    if !count_ref > 0 then
      Format.fprintf fmt "| %5s |  %4d |@," (char_to_display code) !count_ref
  in
  let pp_footer fmt () =
    Format.fprintf fmt "|-------+-------|@]@."
  in
  let pp_table fmt counts =
    pp_header fmt ();
    Array.iter (pp_row fmt) counts;
    pp_footer fmt ()
  in
  let out = Format.std_formatter in
  Format.pp_open_vbox out 0;
  pp_table out counts;
  Format.pp_close_box out ();
  Format.pp_print_flush out ();;

exception WrongNumberOfArguments of string;;
exception EmptyFile;;

let () =
  if Array.length Sys.argv <> 3 then
    raise (WrongNumberOfArguments "Program wymaga dokładnie dwóch argumentów: plik input i plik output")
  else
    let input_file = Sys.argv.(1) in
    let output_file = Sys.argv.(2) in
    try
      let content = read_file input_file in
      if String.length content = 0 then raise EmptyFile;
      write_file output_file (modify_content content);
      Printf.printf "Zawartość pliku %s została zmodyfikowana i zapisana do pliku %s\n" input_file output_file;
      Printf.printf "Tabela znaków w pliku %s:\n" input_file;
      print_character_table (count_characters input_file);
    with
    | EmptyFile -> Printf.printf "Plik %s jest pusty\n" input_file
