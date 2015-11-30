(* JUSTIN RAYMOND 
 * COMP521-DPL
 * HW2 tests
 *
 * Use the run.sh script to run these tests on a homework: ./run.sh <hw-to-test.ml>
 *
 * The __functions__ with underscores  are reference implementations, named as to
 * avoid name clashes.
 *
 * The first part of this file contains a reference implementation.
 * The second part contains helper functions and values.
 * The third part contains the types of tests.
 *   the output of a test is either OK <the result> or
 *      Exn <the exception thrown during execution>.
 *   each test generates a report, which contains a message, the input, the output,
 *      the expected output, and whether the output equaled the expected output.
 * The fourth part  contains lists of tests. The tests are pairs of the input value and the
 * expected output value.
 *    In this this homework, the tests are broken up as follows:
 *    substitution (basic, shadowing, capture)
 *    call-by-value (individual step tests for each rule, full evaluation tests)
 *    normal order (individual step tests for each rule, full evaluation tests)
 *    lambda to DeBruijn tests
 *    DeBruijn to lambda tests
 * The last part contains the tester functions and types.
 *    The timeout function runs a computation for n seconds and returns Some of
 *      the result or None. Due to Marshalling Ocaml datatypes through a Unix
 *      socket, equality testing does not work for exceptions, so odd behavior may occur
 *      such as Exn TIMEOUT <> Exn TIMEOUT. This will be fixed in the future by
 *      doing the equality test before Marshalling the result.
 *    The individual tester function if ftest_unit.
 * The actualling testing functions contain a lot of code duplication that will
 * be eliminated.
  *)
exception NO_RULE;;
exception TEST;;
exception OUT_OF_BOUNDS;;
exception TIMEOUT;;

(* ******************** REFERENCE IMPLEMENTATION ******************** *)
(* types to be tested
type term =
   TmVar of string
  |TmApp of (term * term)
  |TmAbs of (string * term);;

type dbterm = DbTmVar of int
             |DbTmAbs of dbterm
             |DbTmApp of (dbterm * dbterm);;
(* functions to be tested *)
let subst = raise TEST;;
let eval_step = raise TEST;;
let top_eval = raise TEST;;
let no_step = raise TEST;;
let top_no_eval = raise TEST;;
let lam2db = raise TEST;;
let db2lam = raise TEST;;
*)

(* pretty printing *)
let rec __tm_to_str__ t =
  match t with
  | TmVar s -> s
  | TmAbs (s,t) -> "(\\" ^ s ^ "." ^ __tm_to_str__ t ^ ")"
  | TmApp (t1,t2) -> "(" ^ __tm_to_str__ t1 ^ " " ^ __tm_to_str__ t2 ^ ")";;

(*** substitution into lambda terms ***)
let __x__ = ref 0;;
let __make_fresh_var__ () =
  __x__ := !__x__+1;
  "_x"^(string_of_int !__x__);;
let rec __subst__ var s term =
  match term with
  | TmVar y -> if var = y then s else TmVar y
  | TmAbs (y,t1) -> let y' = __make_fresh_var__ () in
                    let t1' = __subst__ y (TmVar y') t1 in
                    TmAbs (y',__subst__ var s t1')
  | TmApp (t1,t2) -> let t1' = __subst__ var s t1 in
                     let t2' = __subst__ var s t2 in
                     TmApp (t1',t2');;

(* cbv normal forms *)
let __is_val__ t =
  match t with
  | TmAbs _ -> true
  | _ -> false


(***** Call By Value *****)
let rec __eval_step__ t =
  match t with
  | TmApp (TmAbs (x,t1),t2) when __is_val__ t2 -> __subst__ x t2 t1
  | TmApp (t1,t2) when __is_val__ t1 -> TmApp (t1, __eval_step__ t2)
  | TmApp (t1,t2) -> TmApp (__eval_step__ t1, t2)
  | _ -> raise NO_RULE;;

(* eval_cbv *)
let rec __eval_cbv__ t = if __is_val__ t then t else __eval_cbv__ (__eval_step__ t);;

let __top_eval__ t = __x__ := 0; __eval_cbv__ t;;


(******** Normal Order **********)
(* non-abstraction normal forms *)
let rec __is_nanf__ t =
  match t with
  | TmVar _ -> true
  | TmApp (t1,t2) -> __is_nanf__ t1 && __is_nf__ t2
  | _ -> false
and (* normal forms *)
  __is_nf__ t =
  match t with
  | TmAbs (x,t1) -> __is_nf__ t1
  | t -> __is_nanf__ t
(* non-abstractions *) 
let __is_na__ t =
  match t with
  | TmVar _ -> true
  | TmApp (t1,t2) -> true
  | _ -> false

(****** normal order evaluation step *****)
let rec __no_step__ t =
  match t with
  | TmApp (t1, t2) when __is_na__ t1 && not (__is_nanf__ t1)-> TmApp (__no_step__ t1, t2)
  | TmApp (t1, t2) when __is_nanf__ t1 -> TmApp (t1, __no_step__ t2)
  | TmAbs (x, t1) -> TmAbs (x, __no_step__ t1)
  | TmApp (TmAbs (x, t1), t2) -> __subst__ x t2 t1
  | _ -> raise NO_RULE

let rec __no_eval__ t = if __is_nf__ t then t else __no_eval__ (__no_step__ t)

let __top_no_eval__ t = __x__ := 0; __no_eval__ t;;


(* convert DeBruijn terms to strings for readability *)
let rec __showdb__ d =
  match d with
  | DbTmVar(n) -> (string_of_int n)
  | DbTmAbs(t) -> "\\"^"."^(__showdb__ t)
  | DbTmApp(t1,t2) -> "("^(__showdb__ t1)^" "^(__showdb__ t2)^")";;

let rec ____index_of___h__ i y xs =
  match xs with
  | [] -> raise OUT_OF_BOUNDS
  | (x::xs') -> if x = y then i else ____index_of___h__ (i+1) y xs';;

let __index_of__ = ____index_of___h__ 0

let __last___index_of____ y xs = let ri = __index_of__ y (List.rev xs) in
                         List.length xs - ri - 1

let rec __db2lam__ ctx tm =
  match tm with
  | DbTmVar x -> TmVar (List.nth ctx x)
  | DbTmAbs t1 -> let x = "x" ^ string_of_int (List.length ctx) in
                  TmAbs (x, __db2lam__ (x::ctx) t1)
  | DbTmApp (t1,t2) -> TmApp (__db2lam__ ctx t1, __db2lam__ ctx t2)

let rec __lam2db__ ctx tm = 
  match tm with
  | TmVar x -> DbTmVar (__index_of__ x ctx)
  | TmAbs (x,t1) -> DbTmAbs (__lam2db__ (x::ctx) t1)
  | TmApp (t1,t2) -> DbTmApp (__lam2db__ ctx t1, __lam2db__ ctx t2)


(* ******************** HELPER FUNCTIONS AND VALUES ******************** *)
(* some bools and church numerals *)
let tru = TmAbs ("x", TmAbs ("y", TmVar "x"))
let fls = TmAbs ("x", TmAbs ("y", TmVar "y"))
let c0 = TmAbs ("s", TmAbs ("z", TmVar "z"))
let c1 = TmAbs ("s", TmAbs ("z", TmApp (TmVar "s", TmVar "z")))
let c2 = TmAbs ("s", TmAbs ("z", TmApp (TmVar "s", TmApp (TmVar "s", TmVar "z"))))
let c3 = TmAbs ("s", TmAbs ("z", TmApp (TmVar "s", TmApp (TmVar "s", TmApp (TmVar "s", TmVar "z")))))
let c4 = TmAbs ("s", TmAbs ("z", TmApp (TmVar "s", TmApp (TmVar "s", TmApp (TmVar "s", TmApp (TmVar "s", TmVar "z"))))))


(* some test functions *)
let and0 = TmAbs ("a", TmAbs ("b", TmApp (TmApp (TmVar "a", TmVar "b"), fls)))
let or0 = TmAbs ("a", TmAbs ("b", TmApp (TmApp (TmVar "a", tru), TmVar "b")))
let ifthenelse = TmAbs ("b", TmAbs ("t", TmAbs ("e", TmApp (TmApp (TmVar "b", TmVar "t"), TmVar "e"))))
let succ = TmAbs ("m", TmAbs ("s", TmAbs ("z", TmApp (TmVar "s",(TmApp (TmApp (TmVar "m", TmVar "s"), TmVar "z"))))))
let plus = TmAbs ("m", TmAbs ("n", TmAbs ("s", TmAbs ("z", TmApp (TmApp (TmVar "m", TmVar "s"), TmApp (TmApp (TmVar "n", TmVar "s"), TmVar "z"))))))


(**** utility functions ****)
let uncurry3 f (a,b,c) = f a b c;;

let rec db_to_str t =
  match t with
  | DbTmVar i -> string_of_int i
  | DbTmAbs t1 -> "\\." ^ db_to_str t1 ^ ""
  | DbTmApp (t1,t2) -> "(" ^ db_to_str t1 ^ " " ^ db_to_str t2 ^ ")"
 
(* remove duplicates from list *) 
let rec __nubs__ xs = 
  (match xs with
  | [] -> []
  | (x::xs) -> x :: (__nubs__ (List.filter ((<>) x) xs)));;

(* returns list of free variables in a term with no duplicates *)
let __free_vars__ t = 
  let rec fv t =
    (match t with 
    | TmVar x -> [x]
    | TmAbs (x,t1) -> List.filter ((<>) x) (fv t1)
    | TmApp (t1,t2) -> (fv t1) @ (fv t2))
  in __nubs__ (fv t)

(* determines if two terms are alpha equivalent *)
let __alpha_eq__ t1 t2 = 
  let d1 = __lam2db__ (__free_vars__ t1) t1 in
  let d2 = __lam2db__ (__free_vars__ t2) t2 in
  d1 = d2


(* ******************** TEST TYPES ******************** *)
type 'a output = OK of 'a | Exn of exn

let output_to_str f o =
  match o with
  | OK a -> "OK " ^ f a
  | Exn e -> "Exn " ^ Printexc.to_string e

type ('a,'b,'c) report =
  { msg : string
  ; inp : 'a
  ; outp : 'b
  ; ans : 'c
  ; pass : bool }

(* and *)
let test_and = [(TmApp (TmApp (and0,tru),tru), OK tru)
               ;(TmApp (TmApp (and0,tru),fls), OK fls)
               ;(TmApp (TmApp (and0,fls),tru), OK fls)
               ;(TmApp (TmApp (and0,fls),fls), OK fls)]
(* or *)
let test_or = [(TmApp (TmApp (or0,tru),tru), OK tru)
              ;(TmApp (TmApp (or0,tru),fls), OK tru)
              ;(TmApp (TmApp (or0,fls),tru), OK tru)
              ;(TmApp (TmApp (or0,fls),fls), OK fls)]

(* if_then_else *)
let test_ifthenelse =
  [TmApp (TmApp (TmApp (ifthenelse,tru),tru),fls), OK tru
  ;TmApp (TmApp (TmApp (ifthenelse,fls),fls),tru), OK tru]

(* test correct behavior of lam2db and db2lam *)
let lam2db_tests = [(TmAbs ("x", TmAbs ("x", TmVar "x"))), OK (DbTmAbs (DbTmAbs (DbTmVar 0)))
                   ;(TmAbs ("x", TmAbs ("x", TmApp (TmVar "x",TmVar "x")))), OK (DbTmAbs (DbTmAbs (DbTmApp (DbTmVar 0, DbTmVar 0))))
                   ;(TmAbs ("z", TmApp (TmAbs ("y",TmApp (TmAbs ("x",TmApp (TmApp (TmVar "z",TmVar "x"),TmApp (TmVar "z",TmVar "y"))),TmApp (TmVar "z",TmVar "y"))),TmAbs("x",TmApp (TmVar "z",TmVar "x")))),
                     OK (DbTmAbs (DbTmApp (DbTmAbs (DbTmApp (DbTmAbs (DbTmApp (DbTmApp (DbTmVar 2,DbTmVar 0),DbTmApp (DbTmVar 2,DbTmVar 1))),DbTmApp (DbTmVar 1,DbTmVar 0))),DbTmAbs(DbTmApp (DbTmVar 1,DbTmVar 0))))))
                   ;(TmAbs ("x", TmAbs ("y", TmApp (TmAbs ("x", TmApp (TmVar "y", TmVar "x")), TmVar "x"))),
                     OK (DbTmAbs (DbTmAbs (DbTmApp (DbTmAbs (DbTmApp (DbTmVar 1, DbTmVar 0)), DbTmVar 1)))))]

let db2lam_tests = [(DbTmAbs (DbTmAbs (DbTmVar 0))), OK (TmAbs ("x", TmAbs ("x", TmVar "x")))
                   ;(DbTmAbs (DbTmAbs (DbTmApp (DbTmVar 0, DbTmVar 0)))), OK (TmAbs ("x", TmAbs ("x", TmApp (TmVar "x",TmVar "x"))))
                   ;(DbTmAbs (DbTmApp (DbTmAbs (DbTmApp (DbTmAbs (DbTmApp (DbTmApp (DbTmVar 2,DbTmVar 0),DbTmApp (DbTmVar 2,DbTmVar 1))),DbTmApp (DbTmVar 1,DbTmVar 0))),DbTmAbs(DbTmApp (DbTmVar 1,DbTmVar 0)))),
                    OK (TmAbs ("z", TmApp (TmAbs ("y",TmApp (TmAbs ("x",TmApp (TmApp (TmVar "z",TmVar "x"),TmApp (TmVar "z",TmVar "y"))),TmApp (TmVar "z",TmVar "y"))),TmAbs("x",TmApp (TmVar "z",TmVar "x"))))))
                   ;(DbTmAbs (DbTmAbs (DbTmApp (DbTmAbs (DbTmApp (DbTmVar 1, DbTmVar 0)), DbTmVar 1))),
                    OK (TmAbs ("x", TmAbs ("y", TmApp (TmAbs ("x", TmApp (TmVar "y", TmVar "x")), TmVar "x")))))]



(* substitution tests
 * Tests are of the form ((x,s,t),t') where [x->s]t = t'
 *)
(* basic correctness *)
let subst_basic_tests =
  [(("x",TmVar "s",TmVar "x"),OK (TmVar "s"))
  ;(("x",TmVar "s",TmVar "z"),OK (TmVar "z"))
  ;(("x",TmVar "s",TmAbs("y",TmVar "x")),OK (TmAbs("y",TmVar "s")))
  ;(("x",TmVar "s",TmAbs("y",TmVar "z")),OK (TmAbs("y",TmVar "z")))
  ;(("x",TmVar "s",TmApp(TmVar "x",TmVar "x")),OK (TmApp(TmVar "s", TmVar "s")))
  ;(("x",TmVar "s",TmApp(TmVar "y",TmVar "z")),OK (TmApp(TmVar "y", TmVar "z"))) ]
(* deals with shadowing *)
let subst_shadowing_tests =
  [(("x",TmVar "s",TmAbs("x",TmVar "x")),OK (TmAbs ("x",TmVar "x")))
  ;(("x",TmVar "s",TmAbs("x",TmVar "y")),OK (TmAbs ("x",TmVar "y")))
  ;(("x",TmVar "s",TmAbs("x",TmApp(TmAbs ("x",TmVar "x"),TmVar "x"))),OK (TmAbs("x",TmApp(TmAbs ("x",TmVar "x"),TmVar "x"))))] (*Y U FAIL, works in interpreter *)
(* avoids capture *)
let subst_capture_tests =
  [(("x",TmVar "s",TmAbs("s",TmApp(TmVar "s",TmVar "x"))),OK (TmAbs ("s1",TmApp(TmVar "s1",TmVar "s"))))
  ;(("x",TmVar "s",TmAbs("s",TmVar "x")),OK (TmAbs ("s1",TmVar "s")))
  ;(("x",TmAbs ("s",TmVar "x"),TmAbs("s",TmVar "x")),OK (TmAbs ("s",TmAbs("s",TmVar "x"))))
  ;(("x",TmAbs ("s",TmApp (TmVar "s",TmVar "x")),TmAbs("s",TmVar "x")),OK (TmAbs ("s",TmAbs("s",TmApp (TmVar "s",TmVar "x")))))
  ;(("x",TmAbs ("x",TmApp (TmVar "s",TmVar "x")),TmAbs("s",TmVar "x")),OK (TmAbs ("s0",TmAbs("x",TmApp (TmVar "s",TmVar "x")))))
  ;(("x",TmAbs ("x",TmApp (TmVar "s",TmVar "y")),TmAbs("s",TmAbs("y",TmVar "x"))),OK (TmAbs ("s0",TmAbs("y1",TmAbs("x",TmApp (TmVar "s",TmVar "y"))))))]
let subst_tests =
  [("subst basic",subst_basic_tests)
  ;("subst shadowing",subst_shadowing_tests)
  ;("subst capture",subst_capture_tests)]

(* call-by-value tests
 * tests are of the form (input,answer) where the answer is the result of 1 step of evaluation
 *)
(* E-APP1 rule *)
let test_cbv_app1 =
  let t1 = TmApp (TmApp (TmAbs ("x",TmVar "x"),tru),TmVar "x") in
  [(t1,OK (__eval_step__ t1))]
(* E-APP2 rule *)
let test_cbv_app2 =
  let t1 = TmApp (TmAbs ("x",tru), TmApp (TmAbs ("x",TmVar "x"),tru)) in
  [(t1,OK (__eval_step__ t1))]
(* E-APPABS rule *)
let test_cbv_appabs = 
  let t1 = TmApp (TmAbs ("x",TmApp (TmAbs ("x",TmVar "x"),TmVar "x")),TmAbs ("y",TmAbs ("x",TmVar "y"))) in
  [(t1,OK (__eval_step__ t1))]
(* random *)
let test_cbv_random = 
  let t1 = TmAbs ("x",TmVar "x") in
  let t2 = TmVar "y" in
  let t3 = TmApp (TmVar "x",TmVar "y") in
  [(t1,Exn NO_RULE)
  ;(t2,Exn NO_RULE)
  ;(t3,Exn NO_RULE)]
let cbv_step_tests =
  [("cbv app1",test_cbv_app1)
  ;("cbv app2",test_cbv_app2)
  ;("cbv appabs",test_cbv_appabs)
  ;("cbv random",test_cbv_random)]
(* test full evaluation *)
let cbv_top_tests =
  let tms = test_and @ test_or @ test_ifthenelse in
  List.map (fun (tm,_) -> (tm, OK (__top_eval__ tm))) tms;;
(* normal order tests
 * tests are of the form (input,answer) where the answer is the result of 1 step of evaluation
 *)
let test_no_app1 =
  let tms = [TmApp (TmApp(and0,tru),fls)] in
  List.map (fun tm -> (tm, OK (__no_step__ tm))) tms;;
let test_no_app2 = 
  (*let tms = [] in*)
  let tms = [TmApp (TmApp (TmVar "x",TmVar "x"),TmApp(and0,tru))] in
  List.map (fun tm -> (tm, OK (__no_step__ tm))) tms;;
let test_no_appabs =
  let tms = [TmApp(and0,tru)
            ;TmApp(TmAbs("x",TmApp(and0,tru)),fls)] in
  List.map (fun tm -> (tm, OK (__no_step__ tm))) tms;;
let test_no_abs =
  let tms = [TmAbs("x",TmApp(and0,tru))
            ;TmAbs("x",TmApp(TmApp(and0,tru),fls))] in
  List.map (fun tm -> (tm, OK (__no_step__ tm))) tms;;
let test_no_top =
  let tms = test_and @ test_or @ test_ifthenelse in
  List.map (fun (tm,_) -> (tm, OK (__top_no_eval__ tm))) tms;;
let no_step_tests =
  [("no app1",test_no_app1)
  ;("no app2",test_no_app2)
  ;("no abs",test_no_abs)
  ;("no appabs",test_no_appabs)]
(* full evaluation *)
let no_top_tests = 
  let tms = test_and @ test_or @ test_ifthenelse in
  List.map (fun (tm,_) -> (tm, OK (__top_no_eval__ tm))) tms;;

(* ******************** TEST FUNCTIONS ******************** *)

(* kills process, smothers error if process does not exist *)
let kill pid sign = 
  try Unix.kill pid sign with
  | Unix.Unix_error (e,f,p) -> ()
  | e -> raise e;;
  
(* runs f on arg and stops after time seconds if not completed, returns defualt if timeout *)
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
let ftest_unit msg f eq (inp,ans) =
  let f = fun () -> (try OK (f inp) with e -> Exn e) in
  let outp = (match timeout f () 1 None with
              | Some r -> r
              | None -> Exn TIMEOUT) in
  {msg = msg;inp = inp;outp = outp;ans = ans;pass = eq outp ans}
  

let ftester msg f eq lst =
  let results = List.map (fun pair -> ftest_unit msg f eq pair) lst in
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
  then"\x1b[32m[PASS]\x1b[39;49m "^rpt.msg^"\t"^i_str^" -> "^o_str ^"\n"
  else"\x1b[31m[FAIL]\x1b[39;49m "^rpt.msg^"\n\tgot: "^o_str^"\n\texpected: "^a_str^"\n\tinput:"^i_str^"\n"

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
  
(* pretty printer to show the input to subst *)          
let subst_inp_to_str (x,s,t) = "["^x^"->"^__tm_to_str__ s ^ "]"^__tm_to_str__ t;;

let print_pf p f = 
  Printf.printf "\x1b[47;30m%i PASSED, %i FAILED\x1b[0m\n\n" p f; flush stdout;;

(* subst testing functions *)
let subst_tester (m,ts) = ftester m (uncurry3 subst) (output_eq __alpha_eq__) ts;;
let subst_printer r = print_string (report_to_str subst_inp_to_str (output_to_str __tm_to_str__) r);;

let test_subst tests =
  let results = List.map subst_tester tests in
  let () = List.iter (fun (p,f,rs) -> List.iter subst_printer rs; print_pf p f) results in
  List.fold_left (fun (ps,fs) (p,f,_) -> (ps+p,fs+f)) (0,0) results;;

(* call-by-value testing functions *)
let cbv_step_tester (m,ts) = ftester m eval_step (output_eq __alpha_eq__) ts;;
let lam_printer r = print_string (report_to_str __tm_to_str__ (output_to_str __tm_to_str__) r);;

let test_cbv_step tests =
  let results = List.map cbv_step_tester tests in
  let () = List.iter (fun (p,f,rs) -> List.iter lam_printer rs; print_pf p f) results in
  List.fold_left (fun (ps,fs) (p,f,_) -> (ps+p,fs+f)) (0,0) results;;

let test_cbv_top_eval tests = 
  let (p,f,rpts) = ftester "cbv top_eval" top_eval (output_eq __alpha_eq__) tests in
  let () = List.iter lam_printer rpts; print_pf p f in
  (p,f);;

(* normal-order testing functions *)
let no_step_tester (m,ts) = ftester m no_step (output_eq __alpha_eq__) ts;;

let test_no_step tests = 
  let results = List.map no_step_tester tests in
  let () = List.iter (fun (p,f,rs) -> List.iter lam_printer rs; print_pf p f) results in
  List.fold_left (fun (ps,fs) (p,f,_) -> (ps+p,fs+f)) (0,0) results;;

let test_no_top_eval tests = 
  let (p,f,rpts) = ftester "no top_eval" top_no_eval (output_eq __alpha_eq__) tests in
  let () = List.iter lam_printer rpts; print_pf p f in
  (p,f);;

(* DeBruijn indices testing functions *)
let test_lam2db tests =
  let (p,f,rpts) = ftester "lam2db" (lam2db []) (output_eq (=)) tests in
  let () = List.iter (fun r -> print_string (report_to_str __tm_to_str__ (output_to_str db_to_str) r)) rpts; print_pf p f in
  (p,f);;

let test_db2lam tests =
  let (p,f,rpts) = ftester "db2lam" (db2lam []) (output_eq __alpha_eq__) tests in
  let () = List.iter (fun r -> print_string (report_to_str db_to_str (output_to_str __tm_to_str__) r)) rpts; print_pf p f in
  (p,f);;


let run_tests () = 
  let (p0,f0) = test_subst subst_tests in
  let (p1,f1) = test_cbv_step cbv_step_tests in
  let (p2,f2) = test_cbv_top_eval cbv_top_tests in
  let (p3,f3) = test_no_step no_step_tests in
  let (p4,f4) = test_no_top_eval no_top_tests in
  let (p5,f5) = test_lam2db lam2db_tests in
  let (p6,f6) = test_db2lam db2lam_tests in
  let (p,f) = (p0+p1+p2+p3+p4+p5+p6,f0+f1+f2+f3+f4+f5+f6) in
  Printf.printf "\x1b[47;30;1mTOTAL: %i PASSED, %i FAILED\x1b[0m\n"  p f; flush stdout;;

run_tests ();;
