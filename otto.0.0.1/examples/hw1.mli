(* Slightly more complicated homework *)

(* Untyped lambda terms *)
type term =
  | TmVar of string
  | TmApp of term * term
  | TmAbs of string * term

(* Lambda terms with DeBruijn Indeces *)
type dbterm =
  | TmVar of int
  | TmApp of dbterm * dbterm
  | TmAbs of dbterm


(* expection to raise if stuck *)
exception NO_RULE

(* single step call-by-value evaluation for lambda terms *)
val cbv_step : term -> term

(* full call-by-value evaluation for lambda terms *)
val cbv_eval : term -> term

(* Convert from lambda terms to DeBruijn terms *)
val lam2db : term -> dbterm

(* Convert from DeBruijn term to lambda term *)
val db2lam : dbterm -> term
