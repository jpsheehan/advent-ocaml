open Base
open Helpers

let exercise_one nums =
  let xs = List.drop nums 1 in
  let ys = Option.value_exn (List.drop_last nums) in
  List.fold2_exn xs ys ~init:0 ~f:(fun acc x y ->
      if x > y then acc + 1 else acc)

let exercise_two nums =
  let xs = List.drop_last_exn (List.drop_last_exn nums) in
  let ys = List.drop_last_exn (List.drop nums 1) in
  let zs = List.drop nums 2 in

  let sums = List.map3_exn xs ys zs ~f:(fun x y z -> x + y + z) in
  exercise_one sums

let%test_module "Day01" =
  (module struct
    let sample_data = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263"

    let%test "exercise one" =
      let channel = jin_string sample_data in
      exercise_one (read_as_ints channel) = 7

    let%test "exercise_two" =
      let channel = jin_string sample_data in
      exercise_two (read_as_ints channel) = 5
  end)
