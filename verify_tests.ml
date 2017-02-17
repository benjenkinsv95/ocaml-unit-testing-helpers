open Verify ;;


let test () =
  Verify.verify_true "temp" true;

;;

test();;
print_endline "Tests finished running.";;