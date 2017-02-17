(* 
  Utility methods built so I can get verifications in a 
  'Expected %s but result %s' format 
  @author: Ben Jenkins 
*)


(* verify assertion format_string ... -- Verifies that the boolean
   assertion evaluates to true, continuing silently if so; if the
   assertion fails (evaluates to false) it prints the format_string,
   as per Printf.printf, which can reference further arguments as
   well. Example of usage:

        # let n = 5 in
          verify (n mod 2 = 0) "n is %d, but should be even\n" n ;;
  n is 5, but should be even
  - : unit = ()
  @author: Stuart M. Shieber
 *)

let verify (condition : bool)
     (fmt : ('a, out_channel, unit) format)
         : 'a =
  if condition then Printf.ifprintf stdout fmt
  else Printf.printf fmt ;;







(* Useful toggle for debugging if tests aren't working as expected. *)
let print_true_conditions: bool = false;;

let verify_strings (condition : bool)
                   (test_name : string)
                   (expected : string)
                   (result : string)
                  : 'a =
  (* Turns text red in linux terminal. *)
  let red_text_code = "\027[31m" in 
  if not condition then 
    Printf.fprintf stdout 
      "%sTest '%s' failed. Expected: '%s', but actual: '%s'\n" 
      red_text_code test_name expected result 
  else if print_true_conditions then 
    Printf.fprintf stdout 
      "Test '%s' passed. Value: '%s'\n" test_name expected 
;;

let verify_equals (test_name : string)
                (string_of_a : 'a -> string)
                (expected : 'a)
                (result : 'a)
              : 'b =
  let condition = expected = result in
  let expected_display = string_of_a expected in
  let result_display = string_of_a result in
  verify_strings condition test_name expected_display result_display;;

let verify_ints (condition : bool)
                (test_name : string)
                (expected : int)
                (result : int)
              : 'a = 
  verify_strings condition test_name (string_of_int expected) 
    (string_of_int result);;

let float_equal (x:float) (y:float) : bool = 
  let epsilon = 1.0e-5 in
  x = y || (abs_float (x -. y)) < epsilon;; 

let verify_floats_equal (test_name : string)
                        (expected : float)
                        (result : float)
                      : 'a = 
     verify_strings (float_equal expected result) test_name 
       (string_of_float expected) (string_of_float result);;

let verify_floats_not_equal (test_name : string)
                            (expected : float)
                            (result : float)
                          : 'a = 
  verify_strings (not (float_equal expected result)) test_name 
   (string_of_float expected) (string_of_float result);;       

let verify_bools (condition : bool)
                 (test_name : string)
                 (expected : bool)
                 (result : bool)
                : 'a = 
  verify_strings condition test_name (string_of_bool expected) 
    (string_of_bool result);;

let verify_true (test_name : string) (result : bool) : 'a =
  verify_strings result test_name (string_of_bool true) 
    (string_of_bool result);;

let verify_false (test_name : string) (result : bool) : 'a =
  verify_strings (not result) test_name (string_of_bool false) 
    (string_of_bool result);;

let verify_failed (test_name : string) (message : string) : 'a =
  Printf.fprintf stdout "Test '%s' failed. '%s'\n" test_name message ;;

let verify_none (test_name : string) (opt : float option) : 'a =
  match opt with 
  | Some x -> Printf.fprintf stdout 
                "Test '%s' failed. Expected: 'None', but actual: 'Some'\n" 
                test_name 
  | None -> if print_true_conditions then 
              Printf.fprintf stdout "Test '%s' passed. Value: 'None'\n" 
              test_name 
;;
                      