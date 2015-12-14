val const : 'a -> 'b -> 'a
val logger_fun : (string -> unit) ref
val default_logger : string -> unit
val pass_fail_count : (int * int) list ref
val pop_pass_fail_count : unit -> int * int
val pop_and_add_pass_fail_count : unit -> int * int
val push_pass_fail_count : int * int -> unit
val inc_pass_count : unit -> unit
val inc_fail_count : unit -> unit
val print_counts : unit -> unit
type ('a, 'b) either = Left of 'a | Right of 'b
val from_either : ('a -> 'b) -> ('c -> 'b) -> ('a, 'c) either -> 'b
val kill : int -> int -> unit
val timeout : ('a -> 'c) -> 'a -> int -> 'd -> 'b option
val catcher :
  ('a -> 'b) -> ((exn, 'b) either -> 'c) -> 'a -> (exn, 'b) either * 'c
val test :
  ?msg:string ->
  ?timeout:int ->
  ('a -> 'b) ->
  'a ->
  (exn, 'c) either ->
  ((exn, 'b) either -> 'd) ->
  ('a -> string) -> ('c -> string) -> unit -> unit
val otest :
  ?msg:string ->
  ?timeout:int ->
  ('a -> 'b) ->
  'a ->
  'b ->
  ('b -> 'b -> bool) -> ('a -> string) -> ('b -> string) -> unit -> unit
val ftest :
  ?msg:string ->
  ?timeout:int ->
  ('a -> 'b) ->
  'a ->
  exn ->
  (exn -> exn -> bool) -> ('a -> string) -> ('b -> string) -> unit -> unit
val tgroup : ?msg:string -> (unit -> 'a) list -> unit -> unit
val tgroup_h :
  ?msg:string ->
  'a -> 'b -> 'c -> ('a -> 'b -> 'c -> unit -> 'd) list -> unit -> unit
val run_tests : (unit -> 'a) -> (string -> unit) -> 'a
