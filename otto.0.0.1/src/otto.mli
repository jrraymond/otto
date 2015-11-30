type 'a output = OK of 'a | Exn of exn
type ('a, 'b, 'c) report = {
  msg : string;
  inp : 'a;
  outp : 'b;
  ans : 'c;
  pass : bool;
}
exception TIMEOUT
val kill : int -> int -> unit
val timeout : ('a -> 'c) -> 'a -> int -> 'd -> 'b option
val otest_unit :
  string ->
  ('a -> 'b) ->
  ('c output -> 'd -> bool) -> 'a * 'd -> ('a, 'e output, 'd) report
val otester :
  string ->
  ('a -> 'b) ->
  ('c output -> 'd -> bool) ->
  ('a * 'd) list -> int * int * ('a, 'e output, 'd) report list
val report_to_str :
  ('a -> string) -> ('b -> string) -> ('a, 'b, 'b) report -> string
val output_eq : ('a -> 'b -> bool) -> 'a output -> 'b output -> bool
val print_pf : int -> int -> unit
