open OUnit2
open OUnitTest
open Pa_ppx_testutils

open Lazy_reclist
open Jqtypes
open Jqparse0
open Jqinterp

Pa_ppx_base.Pp_MLast.Ploc.pp_loc_verbose := true ;;

let warning s = Fmt.(pf stderr "%s\n%!" s)

let matches ~pattern text =
  match Str.search_forward (Str.regexp (Str.quote pattern)) text 0 with
    _ -> true
  | exception Not_found -> false

let assert_raises_exn_pattern ?msg pattern f =
  Testutil.assert_raises_exn_pred ?msg
    (function
        Failure msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Stdlib.Stream.Error msg) when matches ~pattern msg -> true
      | Stdlib.Stream.Error msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Failure msg) when matches ~pattern msg -> true
      | Invalid_argument msg when matches ~pattern msg -> true
      | JQException msg when matches ~pattern msg -> true
      | _ -> false
    )
    f

let of_string_exn s = s |> parse_string parse_exp_eoi
let exec s js =
  let e = of_string_exn s in
  js
  |> List.map Yojson.Basic.from_string
  |> of_list
  |> map (interp e)
  |> to_list
  |> List.map Yojson.Basic.to_string

type json =
    [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Assoc of (string * json) list
    | `List of json list
    ] [@@deriving show,eq]

type json_list = json list [@@deriving show,eq]

let printer = show_exp
let cmp = equal_exp

let success (expect, arg) =
  let msg = Fmt.(str "parsing test for code << %s >>" arg) in
  assert_equal ~msg ~printer ~cmp expect (of_string_exn arg)

let parsing = "parsing" >::: [
    "simple" >:: (fun ctxt -> List.iter success [
        (ExpDot, ".")
      ; ((ExpInt 0), "0")
      ; ((ExpBrackets ExpDot), ".[]")
      ; ((ExpDeref (ExpDot, (ExpInt 0))), ".[0]")
      ; ((ExpDotField "a"), ".a")
      ; ((ExpField ((ExpDotField "a"), "b")), ".a.b")
      ; ((ExpDict []), "{}")
      ; ((ExpDict [((ExpString "a"), (ExpDotField "a"));
                   ((ExpString "b"), (ExpDotField "b"))]), "{a: .a, b: .b}")
      ; ((ExpBrackets (ExpDotField "a")), ".a[]")
      ; ((ExpSeq ((ExpDotField "a"), (ExpDotField "b"))), ".a | .b")
      ; ((ExpCollect (ExpDotField "a")), "[.a]")
      ; ((ExpConcat ((ExpDotField "a"), (ExpDotField "b"))), "(.a,.b)")
      ; ((ExpDeref (ExpDot, (ExpString "a"))), {|.["a"]|})
      ; ((ExpTryCatch ((ExpDotField "a"), ExpEmpty)), {|.a?|})
      ; ((ExpAlt ((ExpTryCatch ((ExpDotField "a"), ExpEmpty)), (ExpInt 1))), {|.a? // 1|})
      ; ((ExpSlice (ExpDot, (Some (ExpInt 1)), (Some (ExpInt 3)))), {|.[1:3]|})
      ; ((ExpSlice (ExpDot, (Some (ExpInt 1)), None)), {|.[1:]|})
      ; ((ExpSlice (ExpDot, None, (Some (ExpInt 3)))), {|.[:3]|})
      ; (ExpRecurse, {|..|})
      ; ((ExpAdd ((ExpDotField "a"), (ExpDotField "b"))), {|.a + .b|})
      ; ((ExpFuncall ("now", [])), {|now|})
      ; ((ExpFuncall ("length", [(ExpString "foo")])), {|length("foo")|})
      ; ((ExpFuncall ("length", [(ExpDot)])), {|length(.)|})
      ; ((ExpFuncDef (
          ("map", ["f"],
           (ExpCollect (ExpSeq ((ExpBrackets ExpDot), (ExpFuncall ("f", [])))))),
          ExpDot)), {|def map(f): [.[] | f]; .|})
      ; ((ExpFuncDef (
          ("map", ["f"],
           (ExpCollect (ExpSeq ((ExpBrackets ExpDot), (ExpFuncall ("f", [])))))),
          (ExpFuncall ("map", [(ExpAdd (ExpDot, (ExpInt 1)))])))), {|def map(f): [.[] | f]; map(. + 1)|})
      ; ((ExpFuncall ("select", [(ExpEq ((ExpDotField "i"), (ExpInt 1)))])), {|select(.i==1)|})
      ; ((ExpEq ((ExpDotField "i"), (ExpInt 1))), {|.i==1|})
      ; ((ExpDataBind ((ExpString "foo"), "x")), {| "foo" as $x |})
      ; ((ExpReduce ((ExpConcat ((ExpInt 1), (ExpInt 2))), "n", (ExpInt 0),
                     (ExpAdd (ExpDot, (ExpDataVar "n"))))), {| reduce (1,2) as $n ( 0; . + $n) |})
      ; ((ExpForeach ((ExpConcat ((ExpInt 1), (ExpInt 2))), "n", (ExpInt 0),
                      (ExpAdd (ExpDot, (ExpDataVar "n"))),
                      (ExpCollect (ExpConcat ((ExpDataVar "n"), ExpDot))))), {| foreach (1,2) as $n ( 0; . + $n; [$n, .]) |})
      ; ((ExpCond (
          [((ExpGt (ExpDot, (ExpInt 10))), (ExpString "a"));
           ((ExpGt (ExpDot, (ExpInt 5))), (ExpString "b"))],
          (ExpString "c"))), {| if . > 10 then "a" elif . > 5 then "b" else "c" end |})
      ; (ExpEmpty, {| empty |})
      ; ((ExpTryCatch ((ExpDotField "a"), (ExpDotField "b"))), {| try .a catch .b |})
      ; ((ExpFuncall ("path", [ExpDot])), {| path(.) |})
      ]
      )
  ]

type string_list = string list [@@deriving show,eq]

let printer = show_string_list

let success (output, exp, input) =
  let msg = Fmt.(str "exec test for code << %s >>" exp) in
  assert_equal ~msg ~printer output (exec exp input)

let success_canon (output, exp, input) =
  let msg = Fmt.(str "canonicalizing exec test for code << %s >>" exp) in
  assert_equal ~msg ~printer
    (List.sort Stdlib.compare output)
    (List.sort Stdlib.compare (exec exp input))

let failure_pattern (pattern, code, input) =
  let msg = Fmt.(str "failure exec test for code << %s >>" code) in
    assert_raises_exn_pattern ~msg
    pattern
    (fun () -> exec code input)

let execute = "execute" >::: [
    "ok" >:: (fun ctxt -> List.iter success [
        ([], ".", [])
      ; (["0"], "0", ["null"])
      ; ([{|"b"|}], ".a", [{| {"a":"b"} |}])
      ; (["null"], ".b", [{| {"a":"b"} |}])
      ; (["null"], ".b", [{| null |}])
      ; ([{|{}|}], "{}", [{| {} |}])
      ; ([{|{"a":"d"}|}], "{a: .b}", [{| {"b":"d"} |}])
      ; ([{|{"a":"d","b":"c"}|}], "{a: .b, b: .a}", [{| {"a":"c", "b":"d"} |}])
      ; ([{|{"a":"d","b":"c"}|}], "{a: .b.a, b: .a.b}", [{| {"a":{"b": "c"}, "b":{"a": "d"}} |}])
      ; (["1"], ".[0]", [{| [1,2,3] |}])
      ; (["null"], ".[0]", [{| null |}])
      ; (["3"], ".[-1]", [{| [1,2,3] |}])
      ; (["null"], ".[-10]", [{| [1,2,3] |}])
      ; (["null"], ".[10]", [{| [1,2,3] |}])
      ; (["1";"2";"3"], ".a[]", [{| {"a":[1,2,3]} |}])
      ; (["1";"2"], ".[]", [{| {"a":1,"b":2} |}])
      ; ([{|"c"|}], ".a | .b", [{| {"a":{"b":"c"}} |}])
      ; (["1";"2"], ".[] | .a", [{| [{"a":1},{"a":2}] |}])
      ; ([{|"c"|}], ".a | .b", [{| {"a":{"b":"c"}} |}])
      ; ([{|["c"]|}], "[.a]", [{| {"a":"c"} |}])
      ; (["1"; "2"], "(.a,.b)", [{| {"a":1, "b":2} |}])
      ; (["1"], {|.["a"]|}, [{| {"a":1, "b":2} |}])
      ; (["1"; "null"], {|(1,.a)|}, [{| {"b": 1} |}])
      ; (["null"], {|.a|}, [{| {"b":2} |}])
      ; (["1"], {|(1,.a?)|}, [{| [] |}])
      ; (["2"], {|.a // 2|}, [{| {"b": 1} |}])
      ; (["2"], {|.a // 2|}, [{| {"a": false} |}])
      ; (["[2,3,4]"], ".[1:]", [{| [1,2,3,4] |}])
      ; (["[1,2,3]"], ".[:3]", [{| [1,2,3,4] |}])
      ; (["[2,3]"], ".[1:3]", [{| [1,2,3,4] |}])
      ; (["3"], {|.a + .b|}, [{| {"a": 1, "b":2} |}])
      ; (["[1,2]"], {|.a + .b|}, [{| {"a": [1], "b":[2]} |}])
      ; ([{|"12"|}], {|.a + .b|}, [{| {"a": "1", "b":"2"} |}])
      ; ([{|{"c":1,"d":2}|}], {|.a + .b|}, [{| {"a": {"c":1}, "b": {"d":2}} |}])
      ; ([{|{"d":2}|}], {|.a + .b|}, [{| {"a": {"d":1}, "b": {"d":2}} |}])
      ; (["3.0"], {|.a + .b|}, [{| {"a": 1, "b":2.0} |}])
      ; (["4"; "5"; "5"; "6"], {|(.a,.b) + (.c,.d)|}, [{| {"a": 1, "b":2, "c":3, "d":4} |}])
      ; (["-1"], {|.a - .b|}, [{| {"a": 1, "b":2} |}])
      ; (["1.0"], {|.a / .b|}, [{| {"a": 1, "b":1} |}])
      ; (["0.5"], {|.a / .b|}, [{| {"a": 1, "b":2} |}])
      ; (["0"], {|.a % .b|}, [{| {"a": 1, "b":1} |}])
      ; (["0.0"], {|.a % .b|}, [{| {"a": 1, "b":1.0} |}])
      ; (["[3]"], {|.a - .b|}, [{| {"a": [1,2,3], "b":[1,2]} |}])
      ; (["[]"], {|.a - .b|}, [{| {"a": [1,2,3], "b":[1,2,3]} |}])
      ; ([{|"xx"|}], {|.a * .b|}, [{| {"a": "x", "b":2.99} |}])
      ; ([{|null|}], {|.a * .b|}, [{| {"a": "x", "b":-1} |}])
      ; ([{|{"k":{"a":0,"b":2,"c":3}}|}],
          {|{"k": {"a": 1, "b": 2}} * {"k": {"a": 0,"c": 3}}|}, [{| null |}])
      ; ([{|["x","z"]|}], {|.a / .b|}, [{| {"a": "xyz", "b":"y"} |}])
      ; ([{|{"a":"y","b":"xyz"}|}], {|{b: .a, a: .b}|}, [{| {"a": "xyz", "b":"y"} |}])
      ; (["3"], {|. | length|}, [{| "abc" |}])

      ; (["4"], {|. | length|}, [{| "abc\u263A" |}])
      ; (["3"], {|. | length|}, [{| [1,2,3] |}])
      ; (["3"], {|. | length|}, [{| {"a":1, "b":2, "c":4} |}])
      ; (["0"], {|. | length|}, [{| null |}])
      ; (["2"], {|. | utf8bytelength|}, [{| "\u03bc" |}])
      ; ([{|["a","b","c"]|}], {|. | keys|}, [{| {"a":1, "b":2, "c":4} |}])
      ; ([{|[0,1,2]|}], {|. | keys|}, [{| [1,2,3] |}])
      ; ([{|["a","b","c"]|}], {|. | keys_unsorted|}, [{| {"a":1, "b":2, "c":4} |}])
      ; ([{|[0,1,2]|}], {|. | keys_unsorted|}, [{| [1,2,3] |}])
      ; (["true"; "false"], {|has(.a,.c)|}, [{| {"a":"b", "c":"e", "b":1} |}])
      ; (["false"], {|has(1)|}, [{| [] |}])
      ; (["true"; "false"; "false"; "false"], {|has(1,-1,-10,10)|}, [{| [1,2] |}])
      ; (["true"; "false"], {|.[] | in({"foo": 42})|}, [{| ["foo", "bar"] |}])
      ; (["false"; "true"], {|.[] | in([0,1])|}, [{| [2, 0] |}])
      ; (["[1,2]"], {|def map(f): [.[] | f]; map(. + 1)|}, [{| [0, 1] |}])
      ; (["true"; "false"; "false"], {|.i==1|}, [{| {"i": 1} |}; {| {"i": 2} |}; {| {"i": 3} |}])
      ; ([{|{"i":1}|}], {|select(.i==1)|}, [{| {"i": 1} |}; {| {"i": 2} |}; {| {"i": 3} |}])
      ; (["true"; "true"], {|true|}, [{| {"i": 1} |}; {| {"i": 2} |}])
      ; ([{|{"i":1}|}; {|{"i":1}|}; {|{"i":2}|}; {|{"i":3}|}], {|select(true, .i==1)|}, [{| {"i": 1} |}; {| {"i": 2} |}; {| {"i": 3} |}])
      ; ([{|{"ab":59}|}], {|{("a"+"b"): 59}|}, ["null"])
      ; ([{|{"foo":42}|}], {|{foo: .bar}|}, [{|{"bar":42, "baz":43}|}])
      ; ([{|{"title":"Boss","user":"Joe"}|}], {|{user, title}|}, [{|{user: "Joe", title: "Boss", id: 32}|}])
      ; ([{|"foo"|}], {|"foo" as $x | $x|}, [{|0|}])
      ; (["1"; "2"; "2"; "3"; "3"; "4"],
          {|(.i,.i+1) as $x | $x|}, [{| {"i": 1} |}; {| {"i": 2} |}; {| {"i": 3} |}])
      ; (["3"], {|reduce (1,2) as $n ( 0; . + $n)|}, [{|null|}])
      ; (["[1,1]"; "[2,3]"; "[3,6]"], {| foreach (1,2,3) as $n ( 0; . + $n; [$n, .]) |}, [{|0|}])
      ; ([{|"c"|}; {|"c"|} ;
                      {|"a"|}; {|"b"|} ;
                      {|"b"|}; {|"a"|}],
          {| if (. == 6, . > 10) then "a" elif . > 5 then "b" else "c" end |}, ["1"; "6"; "11"])
      ; ([{|"c"|}; {|"b"|}; {|"a"|}],
          {| if . > 10 then "a" elif . > 5 then "b" else "c" end |}, ["1"; "6"; "11"])
      ; (["15"], {|label $here | reduce (1,2,3,4,5) as $n ( 0; if $n >= 10 then break $here else . + $n end)|}, [{|0|}])
      ; ([], {|label $here | reduce (1,2,3,4,5,10) as $n ( 0; if $n >= 10 then break $here else . + $n end)|}, [{|0|}])
      ; (["[1,1]"; "[2,3]"; "[3,6]"; "[4,10]"; "[5,15]"], {|label $here | foreach (1,2,3,4,5) as $n ( 0; if $n >= 10 then break $here else . + $n end; [$n, .])|}, [{|0|}])
      ; (["[1,1]"; "[2,3]"; "[3,6]"; "[4,10]"; "[5,15]"], {|label $here | foreach (1,2,3,4,5,10) as $n ( 0; if $n >= 10 then break $here else . + $n end; [$n, .])|}, [{|0|}])
      ; (["[1,1]"; "[2,3]"; "[3,6]"; "[4,10]"; "[5,15]"], {|label $here | foreach (1,2,3,4,5) as $n ( 0; . + $n; if $n >= 10 then break $here else [$n, .] end)|}, [{|0|}])
      ; (["[1,1]"; "[2,3]"; "[3,6]"; "[4,10]"; "[5,15]"], {|label $here | foreach (1,2,3,4,5,10) as $n ( 0; . + $n; if $n >= 10 then break $here else [$n, .] end)|}, [{|0|}])
      ; ([], {|empty|}, [{|0|}; {|0|}])
      ; ([{|"object_field: not an object"|}], {|try .a catch .|}, [{|[0]|}])
      ; ([], {|try .a|}, [{|[0]|}])
      ]
      )
  ; "ok-canon" >:: (fun ctxt -> List.iter success_canon [
        ([{|{"a":{"b":1},"c":"d","e":[2,3]}|}; {|"d"|}; {|{"b":1}|}; "1"; "[2,3]"; "2"; "3"],
         "..", [{| {"a":{"b":1}, "c":"d", "e":[2,3]} |}])
      ]
      )
  ; "errors" >:: (fun ctxt -> List.iter failure_pattern [
      ("interp0: cannot deref object with non-string",
       ".[0]", ["{}"])
    ; ("interp0: cannot deref array with non-int",
       {|.["a"]|}, ["[]"])
    ; ("array_list: not an array",
       {|.[]|}, ["1"])
    ; ("floating-point division produce non-numeric result",
       {|.a / .b|}, [{| {"a": 1, "b":0} |}])
    ; ("arguments to addition were wrong types",
       {|.a + .b|}, [{| {"a": 1, "b":"0"} |}])
    ; ("interp0: exp (ExpDataBind ((ExpString \"foo\"), \"x\")) MUST be part of a sequence of filters",
       {|"foo" as $x|}, [{| null |}])
    ; ("foo",
       {|error(.)|}, [{|"foo"|}])
    ]
      )
  ; "path" >:: (fun ctxt -> List.iter success [
        ([], ".", [])
      ; ([{|[]|}], {|path(.)|}, [{|[]|}])
      ; ([{|["a"]|}], {|path(.a)|}, [{|null|}])
      ; ([{|[]|}], {|path(.)|}, [{|[]|}])
      ; ([{|["a"]|}], {|path(.a)|}, [{|{}|}])
      ; ([{|["a"]|}], {|path(.a)|}, [{|{"b":1}|}])
      ; ([{|["a"]|}], {|path(.a)|}, [{|{"b":1}|}])
      ]
      )
  ; "errors-path" >:: (fun ctxt -> List.iter (fun (c,i) -> failure_pattern ("path: invalid path", c, i)) [
      ({|path(path(.))|}, ["[]"])
    ; ({|path(1)|}, [{| [] |}])
    ; ({|path(.a)|}, [{|[]|}])
    ; ({|path(.a)|}, [{|1|}])
    ]
      )
  ; "ok-2" >:: (fun ctxt ->
      ()
    ; success ([{|[]|}; {|["a"]|}; {|["a",0]|}; {|["a",1]|}; {|["a",2]|}], {|path(..)|}, [{|{"a":[0,1,2]}|}])
    ; success (["[]"; "[\"a\"]"; "[\"b\"]"], {|path(..)|}, [{|{"a":1,"b":2}|}])
    ; assert_equal [{|0|}] (exec {|0|} [{|0|}])
    )
  ; "errors-2" >:: (fun ctxt ->
        ()
      ; failure_pattern
          ("invalid path expression",
           {|path(1)|}, [{| [] |}])
      ; failure_pattern
          ("arguments to addition were wrong types",
           {|.a + .b|}, [{| {"a": 1, "b":"0"} |}])
      )

  ; "." >:: (fun ctxt ->
        success
          (["null"], ".", ["null"])
      )
  ; ".[0]" >:: (fun ctxt ->
        success
          (["0"], ".[0]", ["[0]"])
      )
  ]



let tests = "all" >::: [
    parsing
  ; execute
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
