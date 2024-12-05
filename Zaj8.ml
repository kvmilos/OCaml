let filename = "test.txt" in

(* Otwarcie channeli *)
let in_channel = open_in filename in
let out_channel = open_out_gen [Open_wronly; Open_creat] 0o666 filename in

(* Pierwszy print *)
seek_in in_channel 0;
let content = really_input_string in_channel (in_channel_length in_channel) in
Printf.printf "Na początku: %s " content;

(* Dopisanie *)
seek_out out_channel (out_channel_length out_channel);
output_string out_channel " + nowy tekst";
flush out_channel;

(* Print po dopisaniu *)
seek_in in_channel 0;
let updated_content = really_input_string in_channel (in_channel_length in_channel) in
Printf.printf "Po aktualizacji: %s " updated_content;

(* Zamknięcie channeli *)
close_in in_channel;
close_out out_channel;;