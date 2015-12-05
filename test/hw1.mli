exception NO_RULE
type term = TmVar of string | TmApp of term * term | TmAbs of string * term
type dbterm = DbTmVar of int | DbTmApp of dbterm * dbterm | DbTmAbs of dbterm
val x : int ref
val make_fresh_var : unit -> string
val index_of_h : int -> 'a -> 'a list -> int
val index_of : string -> string list -> int
val last_index_of : string -> string list -> int
val db2lam : string list -> dbterm -> term
val lam2db : string list -> term -> dbterm
val bad_will_loop : 'a -> 'b
val bad_will_fail : 'a -> 'b
val should_fail : 'a -> 'b
