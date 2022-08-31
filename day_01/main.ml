open Base
open Advent_of_code.Helpers

let exercise_one nums =
  let xs = List.drop nums 1 in
  let ys = Option.value_exn (List.drop_last nums) in
  List.fold2_exn xs ys ~init:0 ~f:(fun acc x y -> if x > y then acc + 1 else acc)

let exercise_two nums =
  let xs = List.drop_last_exn (List.drop_last_exn nums) in
  let ys = List.drop_last_exn (List.drop nums 1) in
  let zs = List.drop nums 2 in

  let sums = List.map3_exn xs ys zs ~f:(fun x y z -> x + y + z) in
  exercise_one sums


let () =
  let original_nums = read_as_ints () in

  (* Exercise 1 *)
  Caml.print_string "Day 01, Ex 1: ";
  Caml.print_endline (Caml.string_of_int (exercise_one original_nums));

  (* Exercise 2 *)
  Caml.print_string "Day 01, Ex 2: ";
  Caml.print_endline (Caml.string_of_int (exercise_two original_nums));
