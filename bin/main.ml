open Advent_of_code
open Advent_of_code.Helpers

let print_intro day exercise =
  print_string (Printf.sprintf "Day %02d, Exercise %d: " day exercise)

let day_01 () =
  let nums = read_as_ints (jin_file "inputs/day_01.txt") in

  (* Exercise 1 *)
  print_intro 1 1;
  print_int (Day01.exercise_one nums);
  print_newline ();

  (* Exercise 2 *)
  print_intro 1 2;
  print_int (Day01.exercise_two nums);
  print_newline ()

let day_02 () =
  let directions =
    read_generic (jin_file "inputs/day_02.txt") ~f:Day02.read_directions
  in

  (* Exercise 1 *)
  print_intro 2 1;
  print_int (Day02.exercise_one directions);
  print_newline ();

  (* Exercise 2 *)
  print_intro 2 2;
  print_int (Day02.exercise_two directions);
  print_newline ()

let day_03 () =
  let report =
    read_generic (jin_file "inputs/day_03.txt") ~f:Day03.reader in
  
  (* Exercise 1 *)
  print_intro 3 1;
  print_int (Day03.exercise_one report);
  print_newline ()

let () =
  day_01 ();
  day_02 ();
  day_03 ()