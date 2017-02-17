open Verify ;;


let test () =
  verify_true "temp" true;
  verify_equals_int "Int equality " 1 2

;;

test();;
print_endline "Tests finished running.";;