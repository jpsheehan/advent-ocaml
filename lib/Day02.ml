open Base
open Helpers

type direction = Forward of int | Up of int | Down of int

let read_directions line =
  match String.split line ~on:' ' with
  | [ direction; amount ] -> (
      let amount = Caml.int_of_string amount in
      match direction with
      | "forward" -> Some (Forward amount)
      | "up" -> Some (Up amount)
      | "down" -> Some (Down amount)
      | _ -> None)
  | _ -> None

let calculate_absolutes directions =
  List.fold directions ~init:(0, 0) ~f:(fun (pos, depth) -> function
    | Forward x -> (pos + x, depth)
    | Down x -> (pos, depth + x)
    | Up x -> (pos, depth - x))

let exercise_one directions =
  let pos, depth = calculate_absolutes directions in
  pos * depth

let calculate_absolutes_with_aim directions =
  List.fold directions ~init:(0, 0, 0) ~f:(fun (pos, depth, aim) -> function
    | Forward x -> (pos + x, depth + (aim * x), aim)
    | Down x -> (pos, depth, aim + x)
    | Up x -> (pos, depth, aim - x))

let exercise_two directions =
  let pos, depth, _ = calculate_absolutes_with_aim directions in
  pos * depth

let%test_module "Day02" =
  (module struct
    let example_input = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

    let%test "example one" =
      let channel = jin_string example_input in
      exercise_one (read_generic channel ~f:read_directions) = 150

    let%test "example two" =
      let channel = jin_string example_input in
      exercise_two (read_generic channel ~f:read_directions) = 900
  end)
