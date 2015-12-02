

(*** tests ***)
type output =
  | Pass of string
  | Fail of string
  | Group of string * output list


(* we cant do this because we want heterogeneous test suites.
 * Instead we use a list of functions : Unit -> Unit.
 * To run the tests we call the functions. The idea is to represent
 * tests as what they do. A test is an action which when run,
 * produces some output. What that output does is determined by the 
 * log parameter, which is responsible for printing the output. *)
(* I guess the next question I have is how do I count pass/fails.
 * I think the logger function may have some internal state maybe? *)

type ('a, 'b) either = Left of 'a | Right of 'b

(* kills process, smothers error if process does not exist *)
let kill pid sign = 
  try Unix.kill pid sign with
  | Unix.Unix_error (e,f,p) -> ()
  | e -> raise e;;

(* runs f on arg and stops after time seconds if not completed, returns defualt if timeout.
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
let otest ?msg:(m="") ?eq_fun:(eq=(=)) ?timeout:(t=60) func args ans = fun () ->
  if m = "" then () else Printf.printf "%s\n" m; flush stdout;
  (match timeout (catcher func) args t None with
              | None -> Printf.printf "[FAILED] timed out after %i seconds\n" t; flush stdout
              | Some (Left e) -> Printf.printf "[FAILED] Exception %s\n" (Printexc.to_string e); flush stdout
              | Some (Right e) when eq e ans -> Printf.printf "[PASS]\n"; flush stdout
              | _ -> Printf.printf "[FAIL]\n"); flush stdout

let tgroup ?msg:(m="") ?eq_fun:(eq=(=)) ?timeout:(t=60) tests =
  fun () -> Printf.printf "%s\n" m; flush stdout; List.map (fun f -> f ()) tests


    

let run_tests suite = suite ()

let test_list = tgroup ~msg:"arithmetic"
  [ otest ~msg:"inc" ~timeout:1 (fun x -> x + 1) 0 1;
    otest ~timeout:1 (fun s -> s ^ "!") "!" "!!" ]

let _ = run_tests test_list

(* report to string
 * fi : function to turn the input into a string
 * fo : function to turn the output/answer into a string
 * rpt : the report
let report_to_str fi fo rpt =
  let i_str = fi rpt.inp in
  let o_str = fo rpt.outp in
  let a_str = fo rpt.ans in
  if rpt.pass
  then "[PASS] "^rpt.msg^"\t"^i_str^" -> "^o_str ^"\n"
  else "[FAIL] "^rpt.msg^"\n\tgot: "^o_str^"\n\texpected: "^a_str^"\n\tinput:"^i_str^"\n"

 *)
(* lifts an equality function to the level of type output
let output_eq eq_fun out ans =
  match out with
  | OK tout ->
      (match ans with
      | OK tans -> eq_fun tout tans
      | _ -> false)
  | Exn e1 ->
      (match ans with
      | Exn e2 -> e1 = e2
      | _ -> false)
*) 

let print_pf p f = 
  Printf.printf "%i PASSED, %i FAILED\n\n" p f; flush stdout;;
(*
let string_of_output out0 =
  let rec go indent accum out =
    let spaces = String.make indent ' ' in
    (match out with
     | Pass s -> (1, 0, accum ^ "\n" ^ spaces ^ s)
     | Fail s -> (0, 1, accum ^ "\n" ^ spaces ^ s)
     | Group (m, ts) ->
         let (p, f, s) = List.fold_right
          (fun t (pac, fac, sac) ->
            let (p, f, s) = go (indent + 1) sac t in
            (pac + p, fac + f, sac ^ s))
          ts
          (0, 0, accum) 
            in spaces ^ m ^ "\n"
            (pa + p, fa + f)) (0, 0, ) ts in
         (p, f, Printf.sprintf "%s%s [%i PASSED, %i FAILED]\n%s" spaces m p f s)) in
  let (p, f, s) = go 0 "" out0 in
  (p, f, s)
*)
