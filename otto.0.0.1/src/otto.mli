type output = Pass of string | Fail of string | Group of string * output list
type ('a, 'b) either = Left of 'a | Right of 'b
val kill : int -> int -> unit
val timeout : ('a -> 'c) -> 'a -> int -> 'd -> 'b option
val catcher : ('a -> 'b) -> 'a -> (exn, 'b) either
val otest :
  ?msg:string ->
  ?eq_fun:('a -> 'a -> bool) ->
  ?timeout:int -> ('b -> 'c) -> 'b -> 'a -> unit -> unit
val tgroup :
  ?msg:string ->
  ?eq_fun:('a -> 'a -> bool) ->
  ?timeout:int -> (unit -> 'b) list -> unit -> 'b list
val run_tests : (unit -> 'a) -> 'a
val test_list : unit -> unit list
val print_pf : int -> int -> unit
