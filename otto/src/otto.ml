(* Justin Raymond *)

(* Want tree data structure of tests.
 * we cant do this because we want heterogeneous test suites.
 * Instead we use a list of functions : Unit -> Unit.
 * To run the tests we call the functions. The idea is to represent
 * tests as what they do. A test is an action which when run,
 * produces some output. What that output does is determined by the 
 * log parameter, which is responsible for printing the output. *)

(* The logger is a global reference to a function.
 * The pass/fail counter is a global reference to a list.
 * *)
let const x _ = x
let logger_fun = ref (fun _ -> ())
let default_logger = fun s -> Printf.printf "%s" s; flush stdout
let pass_fail_count = ref []
let pop_pass_fail_count () = 
  let (p, f) = List.hd !pass_fail_count in
  pass_fail_count := List.tl !pass_fail_count;
  (p, f)
let pop_and_add_pass_fail_count () =
  let (p, f) = List.hd !pass_fail_count in
  pass_fail_count := List.tl !pass_fail_count;
  let ((pn, fn), l) = if List.length !pass_fail_count > 0
                      then (List.hd !pass_fail_count, List.tl !pass_fail_count)
                      else ((0, 0), []) in
  pass_fail_count := (p + pn, f + fn) :: l;
  (p, f)
let push_pass_fail_count t = pass_fail_count := t :: !pass_fail_count
let inc_pass_count () = 
  let (p, f) = pop_pass_fail_count () in
  push_pass_fail_count (p + 1, f)
let inc_fail_count () = 
  let (p, f) = pop_pass_fail_count () in
  push_pass_fail_count (p, f + 1)
let print_counts () = List.iter (fun (p,f) -> Printf.printf "%i,%i|" p f; flush stdout) !pass_fail_count

type ('a, 'b) either = Left of 'a | Right of 'b

(* kills process, smothers error if process does not exist *)
let kill pid sign = 
  try Unix.kill pid sign with
  | Unix.Unix_error (e,f,p) -> ()
  | e -> raise e;;

(* runs f on arg and stops after time seconds if not completed, returns default if timeout.
 * *)
let timeout f arg time default = 
  let pipe_r,pipe_w = Unix.pipe () in
  (match Unix.fork () with
   | 0 -> let x = Some (f arg) in
          let oc = Unix.out_channel_of_descr pipe_w in
          Marshal.to_channel oc x [];
          close_out oc;
          exit 0
   | pid0 -> 
      (match Unix.fork () with
       | 0 -> Unix.sleep time;
              kill pid0 Sys.sigkill;
              let oc = Unix.out_channel_of_descr pipe_w in
              Marshal.to_channel oc default [];
              close_out oc;
              exit 0
       | pid1 -> let ic = Unix.in_channel_of_descr pipe_r in
                 let result = (Marshal.from_channel ic : 'b option) in
                 result ));;

let catcher func args = try Right (func args) with e -> Left e;;

(* basic testing function
 * msg : message string to associate with the test
 * f : function to apply to the input
 * eq : function to test equality with
 * inp : input
 * ans : expected output
 * returns report
*)
(* TODO compare before marshalling *)
(* TODO deal with failure *)
let otest ?msg:(m="") ?timeout:(t=5) 
          func args ans
          eq show_in show_out =
  fun () ->
    if m = "" then () else !logger_fun (Printf.sprintf "\t%s\n" m);
    let (s, inc) = 
      (match timeout (catcher func) args t None with
      | None ->
          (Printf.sprintf "\t[FAIL] timed out after %i seconds:%s\n" t (show_in args), false)
      | Some (Left e) ->
          (Printf.sprintf "\t[FAIL] Exception %s:%s\n" (Printexc.to_string e) (show_in args), false)
      | Some (Right o) when eq o ans ->
          (Printf.sprintf "\t[PASS]\t%s\n" (show_in args), true)
      | Some (Right o) ->
          (Printf.sprintf "\t[FAIL]\tGot: %s | Expected: %s\n" (show_in args) (show_out ans), false)) in
    if inc then inc_pass_count () else inc_fail_count ();
    !logger_fun s

let tgroup ?msg:(m="") tests =
  fun () ->
    push_pass_fail_count (0, 0);
    !logger_fun (Printf.sprintf "%s\n" m);
    List.map (fun f -> f ()) tests;
    let (p, f) = pop_and_add_pass_fail_count () in
    Printf.printf "%s: PASSED %i | FAILED %i\n" m p f

let tgroup_h ?msg:(m="") eq_fun show_in show_out tests =
  fun () ->
    push_pass_fail_count (0, 0);
    !logger_fun (Printf.sprintf "%s\n" m);
    List.map (fun f -> f eq_fun show_in show_out ()) tests;
    let (p, f) = pop_and_add_pass_fail_count () in
    Printf.printf "%s: PASSED %i | FAILED %i\n" m p f


let run_tests suite logger = 
  logger_fun := logger;
  suite ()
