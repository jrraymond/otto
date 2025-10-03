(* Solution to hw1.ml*)
exception NO_RULE
type term =
  | TmVar of string
  | TmApp of term * term
  | TmAbs of string * term

type dbterm =
  | DbTmVar of int
  | DbTmApp of dbterm * dbterm
  | DbTmAbs of dbterm

(* used to generate fresh variable name
 : unit -> string = <fun>
 *)
let x = ref 0
let make_fresh_var () =
  x := !x+1;
  "_x"^(string_of_int !x);;



let rec index_of_h i y xs =
  match xs with
  | [] -> raise NO_RULE
  | (x::xs') -> if x = y then i else index_of_h (i+1) y xs'

(* return the index first occurence of an element.
 * the element must be in the list.
 *)
let index_of = index_of_h 0

(* get the index of last occurence of an element.
 * the element must be in the list.
 *)
let last_index_of y xs = let ri = index_of y (List.rev xs) in
                         List.length xs - ri - 1


let rec db2lam ctx tm =
  match tm with
  | DbTmVar x -> TmVar (List.nth ctx x)
  | DbTmAbs t1 -> let x = "x" ^ string_of_int (List.length ctx) in
                  TmAbs (x, db2lam (x::ctx) t1)
  | DbTmApp (t1,t2) -> TmApp (db2lam ctx t1, db2lam ctx t2)

let rec lam2db ctx tm =
  match tm with
  | TmVar x -> DbTmVar (index_of x ctx)
  | TmAbs (x,t1) -> DbTmAbs (lam2db (x::ctx) t1)
  | TmApp (t1,t2) -> DbTmApp (lam2db ctx t1, lam2db ctx t2)

let rec bad_will_loop x = bad_will_loop x

let bad_will_fail _ = raise NO_RULE

let should_fail _ = raise NO_RULE
