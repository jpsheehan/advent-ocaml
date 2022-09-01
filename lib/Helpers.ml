open Base

type jin_channel =
  | File of string * Caml.in_channel
  | Stdin
  | String of string list * int ref

let jin_input_line = function
  | File (_, chan) -> Caml.input_line chan
  | Stdin -> Caml.input_line Caml.stdin
  | String (lines, pos) -> (
      match List.nth lines !pos with
      | Some line ->
          pos := !pos + 1;
          line
      | None -> raise End_of_file)

let jin_string str = String (String.split_lines str, ref 0)
let jin_file file_path = File (file_path, Caml.In_channel.open_text file_path)
let jin_stdin () = Stdin

let read_generic chan ~f =
  let rec aux ns =
    try
      let line = jin_input_line chan in
      match f line with Some n -> aux (n :: ns) | None -> ns
    with Caml.End_of_file -> ns
  in
  List.rev (aux [])

let read_as_ints = read_generic ~f:Caml.int_of_string_opt
