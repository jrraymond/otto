

(*** tests ***)
type 'a output = Pass of 'a | Fail of 'a | Exn of exn | Timeout

(* we cant do this because we want heterogeneous test suites.
 * Instead we use a list of functions : Unit -> Unit.
 * To run the tests we call the functions. The idea is to represent
 * tests as what they do. A test is an action which when run,
 * produces some output. What that output does is determined by the 
 * log parameter, which is responsible for printing the output. *)
(* I guess the next question I have is how do I count pass/fails.
 * I think the logger function may have some internal state maybe? *)
type ('a, 'b, 'c) test = 
  { msg : string
  ; args : 'a
  ; ans : 'c
  ; time : int
  ; func : 'a -> 'b
  ; eq : 'b -> 'c -> bool
  ; status : 'b output }

let tgroup ?msg:(m="") ?eq_fun:(eq=(=)) ?timeout:(t=60) tests = ();;

let log m r args outp ans = ();;
let logm m = ();;
let utest ?msg:(m="") ?eq_fun:(eq=(=)) ?timeout:(t=60) func args ans = 
  fun () -> let outp = func args in log m (eq outp ans) args outp ans; ()

let ugroup msg tests = fun () -> logm msg; List.map (fun f -> f ()) tests

(*
let otest_unit msg f eq (inp,ans) =
  let f = fun () -> (try OK (f inp) with e -> Exn e) in
  let outp = (match timeout f () 1 None with
              | Some r -> r
              | None -> Exn TIMEOUT) in
  {msg = msg;inp = inp;outp = outp;ans = ans;pass = eq outp ans}
*)  
let test_list = ugroup [ utest ~msg:"inc" (fun x -> x + 1) 0 1;
                         utest (fun s -> s ^ "!") "!" "!!" ]



(* kills process, smothers error if process does not exist *)
let kill pid sign = 
  try Unix.kill pid sign with
  | Unix.Unix_error (e,f,p) -> ()
  | e -> raise e;;
  
(* runs f on arg and stops after time seconds if not completed, returns defualt if timeout.
 * TODO compare before marshalling
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

(* basic testing function
 * msg : message string to associate with the test
 * f : function to apply to the input
 * eq : function to test equality with
 * inp : input
 * ans : expected output
 * returns report
*)
let otest_unit msg f eq (inp,ans) =
  let f = fun () -> (try OK (f inp) with e -> Exn e) in
  let outp = (match timeout f () 1 None with
              | Some r -> r
              | None -> Exn TIMEOUT) in
  {msg = msg;inp = inp;outp = outp;ans = ans;pass = eq outp ans}
  

let otester msg f eq lst =
  let results = List.map (fun pair -> otest_unit msg f eq pair) lst in
  let passed = List.length (List.filter (fun rpt -> rpt.pass) results) in
  let failed = List.length results - passed in
  (passed,failed,results)

(* report to string
 * fi : function to turn the input into a string
 * fo : function to turn the output/answer into a string
 * rpt : the report
 *)
let report_to_str fi fo rpt =
  let i_str = fi rpt.inp in
  let o_str = fo rpt.outp in
  let a_str = fo rpt.ans in
  if rpt.pass
  then "[PASS] "^rpt.msg^"\t"^i_str^" -> "^o_str ^"\n"
  else "[FAIL] "^rpt.msg^"\n\tgot: "^o_str^"\n\texpected: "^a_str^"\n\tinput:"^i_str^"\n"

(* lifts an equality function to the level of type output *)
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
  

let print_pf p f = 
  Printf.printf "%i PASSED, %i FAILED\n\n" p f; flush stdout;;
