open Base
open Helpers

let reader line =
  let ints = ref [] in
  String.iter line ~f:(fun c -> ints := Caml.int_of_string (String.of_char c) :: !ints);
  Some (List.rev !ints)

let mode list_of_lists =
  let ones, zeros = List.partition_tf list_of_lists ~f:( (=) 1) in
  if List.length ones > List.length zeros then 1 else 0

let int_of_bin_list xs = xs
  |> List.rev
  |> List.mapi ~f:(fun i n -> n * (Int.pow 2 i))
  |> List.fold ~init:0 ~f:(+)

let get_gamma_and_epsilon_rates report =
  let raw_gamma = report
    |> List.transpose_exn
    |> List.map ~f:mode in
  let gamma = int_of_bin_list raw_gamma in
  let epsilon = raw_gamma
    |> List.map ~f:(fun x -> if x = 0 then 1 else 0)
    |> int_of_bin_list in
  (gamma, epsilon)

let exercise_one report =
  let gamma, epsilon = get_gamma_and_epsilon_rates report in
  gamma * epsilon

let%test_module "Day03" =
  (module struct
    let sample_data =
      "00100\n\
       11110\n\
       10110\n\
       10111\n\
       10101\n\
       01111\n\
       00111\n\
       11100\n\
       10000\n\
       11001\n\
       00010\n\
       01010\n"

    let%test "mode" =
      let data = [0; 0; 1] in
      mode data = 0

    let%test "mode" =
      let data = [1; 0; 1] in
      mode data = 1
  
    let%test "int_of_bin_list" =
      let data = [1; 0; 1] in
      int_of_bin_list data = 5

    let%test "int_of_bin_list" =
      let data = [1; 0; 1; 1] in
      int_of_bin_list data = 11

    let%test "get_gamma_and_epsilon_rates" =
      let channel = jin_string sample_data in
      let report = (Helpers.read_generic channel ~f:reader) in
      let gamma, epsilon = get_gamma_and_epsilon_rates report in
      gamma = 22 && epsilon = 9
  end)
