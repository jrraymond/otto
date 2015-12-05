(* Solution to hw1.ml*)


(* used to generate fresh variable name
 : unit -> string = <fun>
 *)
let x = ref 0
let make_fresh_var () =
  x := !x+1;
  "_x"^(string_of_int !x);;

(* subst var s term = [var|-->s]term 
 : string -> term -> term -> term
*)

let rec subst var s term =
  match term with
  | TmVar y -> if var = y then s else TmVar y
  | TmAbs (y,t1) -> let y' = make_fresh_var () in
                    let t1' = subst y (TmVar y') t1 in
                    TmAbs (y',subst var s t1')
  | TmApp (t1,t2) -> let t1' = subst var s t1 in
                     let t2' = subst var s t2 in
                     TmApp (t1',t2');;
(* cbv normal forms *)
let is_val t =
  match t with
  | TmAbs _ -> true
  | _ -> false

let rec cbv_step t =
  match t with
  | TmApp (TmAbs (x,t1),t2) when is_val t2 -> subst x t2 t1
  | TmApp (t1,t2) when is_val t1 -> TmApp (t1, eval_step t2)
  | TmApp (t1,t2) -> TmApp (eval_step t1, t2)
  | _ -> raise NO_RULE;;

let rec eval_cbv t = if is_val t then t else eval_cbv (cbv_step t)

let cbv_eval t = 
  x := 0;
  eval_cbv t

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

(* give string representation of \-terms *)
let rec show tm =
  match tm with
  | (TmVar x) -> x
  | (TmApp(t1,t2)) -> "("^(show t1)^" "^(show t2)^")"
  | (TmAbs(x,t)) -> "\\"^x^"."^(show t);;

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
