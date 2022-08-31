open Base

let read_as_ints () =
  let rec aux ns =
    try
    match Caml.read_int_opt () with
    | Some n -> aux (n :: ns)
    | None -> ns
    with Caml.End_of_file -> ns
  in
  List.rev (aux [])