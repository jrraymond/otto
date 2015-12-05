(* Example 0: Homework 0 tests *)
open Otto

let suite = tgroup 
  ~msg:"Homework 0"
  [ tgroup_h ~msg:"fibonacci" (=) string_of_int string_of_int
    [ otest Hw0.fibonacci 0 0;
      otest Hw0.fibonacci 1 1;
      otest Hw0.fibonacci 2 1;
      otest Hw0.fibonacci 3 2;
      otest Hw0.fibonacci 4 3;
      otest Hw0.fibonacci 5 5;
      otest Hw0.fibonacci 6 8
    ];
    tgroup_h ~msg:"factorial" (=) string_of_int string_of_int
    [ otest Hw0.factorial 0 1;
      otest Hw0.factorial 1 1;
      otest Hw0.factorial 2 2;
      otest Hw0.factorial 3 6;
      otest Hw0.factorial 4 24;
      otest Hw0.factorial 5 120
    ]
  ]

let _ = run_tests suite default_logger
