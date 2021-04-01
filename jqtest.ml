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

let assert_raises_exn_pattern pattern f =
  Testutil.assert_raises_exn_pred
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

let parsing = "parsing" >:::
              let assert_equal = assert_equal ~printer ~cmp in [
    "simple" >:: (fun ctxt ->
        assert_equal ExpDot (of_string_exn ".")
      ; assert_equal (ExpInt 0) (of_string_exn "0")
      ; assert_equal (ExpBrackets ExpDot) (of_string_exn ".[]")
      ; assert_equal (ExpDeref (ExpDot, (ExpInt 0))) (of_string_exn ".[0]")
      ; assert_equal (ExpDotField "a") (of_string_exn ".a")
      ; assert_equal (ExpField ((ExpDotField "a"), "b")) (of_string_exn ".a.b")
      ; assert_equal (ExpDict [])
          (of_string_exn "{}")
      ; assert_equal
          (ExpDict [((ExpString "a"), (ExpDotField "a"));
                    ((ExpString "b"), (ExpDotField "b"))])
          (of_string_exn "{a: .a, b: .b}")
      ; assert_equal (ExpBrackets (ExpDotField "a"))
          (of_string_exn ".a[]")
      ; assert_equal (ExpSeq ((ExpDotField "a"), (ExpDotField "b")))
          (of_string_exn ".a | .b")
      ; assert_equal (ExpCollect (ExpDotField "a"))
          (of_string_exn "[.a]")
      ; assert_equal (ExpConcat ((ExpDotField "a"), (ExpDotField "b")))
          (of_string_exn "(.a,.b)")
      ; assert_equal (ExpDeref (ExpDot, (ExpString "a")))
          (of_string_exn {|.["a"]|})
      ; assert_equal (ExpQuestion (ExpDotField "a"))
          (of_string_exn {|.a?|})
      ; assert_equal (ExpAlt ((ExpQuestion (ExpDotField "a")), (ExpInt 1)))
          (of_string_exn {|.a? // 1|})
      ; assert_equal (ExpSlice (ExpDot, (Some (ExpInt 1)), (Some (ExpInt 3))))
          (of_string_exn {|.[1:3]|})
      ; assert_equal (ExpSlice (ExpDot, (Some (ExpInt 1)), None))
          (of_string_exn {|.[1:]|})
      ; assert_equal (ExpSlice (ExpDot, None, (Some (ExpInt 3))))
          (of_string_exn {|.[:3]|})
      ; assert_equal ExpRecurse
          (of_string_exn {|..|})
      ; assert_equal (ExpAdd ((ExpDotField "a"), (ExpDotField "b")))
          (of_string_exn {|.a + .b|})
      ; assert_equal (ExpFuncall ("now", []))
          (of_string_exn {|now|})
      ; assert_equal (ExpFuncall ("length", [(ExpString "foo")]))
          (of_string_exn {|length("foo")|})
      ; assert_equal (ExpFuncall ("length", [(ExpDot)]))
          (of_string_exn {|length(.)|})
      ; assert_equal
          (ExpFuncDef (
              ("map", ["f"],
               (ExpCollect (ExpSeq ((ExpBrackets ExpDot), (ExpFuncall ("f", [])))))),
              ExpDot))
          (of_string_exn {|def map(f): [.[] | f]; .|})
      ; assert_equal
          (ExpFuncDef (
              ("map", ["f"],
               (ExpCollect (ExpSeq ((ExpBrackets ExpDot), (ExpFuncall ("f", [])))))),
              (ExpFuncall ("map", [(ExpAdd (ExpDot, (ExpInt 1)))]))))
          (of_string_exn {|def map(f): [.[] | f]; map(. + 1)|})
      ; assert_equal
          (ExpFuncall ("select", [(ExpEq ((ExpDotField "i"), (ExpInt 1)))]))
          (of_string_exn {|select(.i==1)|})
      ; assert_equal
          (ExpEq ((ExpDotField "i"), (ExpInt 1)))
          (of_string_exn {|.i==1|})
      ; assert_equal
          (ExpDataBind ((ExpString "foo"), "x"))
          (of_string_exn {| "foo" as $x |})
      )
  ]

type string_list = string list [@@deriving show,eq]

let printer = show_string_list

let execute = "execute" >:::
              let assert_equal = assert_equal ~printer in [
    "ok" >:: (fun ctxt ->
        assert_equal [] (exec "." [])
      ; assert_equal ["0"] (exec "0" ["null"])
      ; assert_equal [{|"b"|}] (exec ".a" [{| {"a":"b"} |}])
      ; assert_equal ["null"] (exec ".b" [{| {"a":"b"} |}])
      ; assert_equal [{|{}|}] (exec "{}" [{| {} |}])
      ; assert_equal [{|{"a":"d"}|}] (exec "{a: .b}" [{| {"b":"d"} |}])
      ; assert_equal [{|{"a":"d","b":"c"}|}] (exec "{a: .b, b: .a}" [{| {"a":"c", "b":"d"} |}])
      ; assert_equal [{|{"a":"d","b":"c"}|}] (exec "{a: .b.a, b: .a.b}" [{| {"a":{"b": "c"}, "b":{"a": "d"}} |}])
      ; assert_equal ["1"] (exec ".[0]" [{| [1,2,3] |}])
      ; assert_equal ["3"] (exec ".[-1]" [{| [1,2,3] |}])
      ; assert_equal ["null"] (exec ".[-10]" [{| [1,2,3] |}])
      ; assert_equal ["null"] (exec ".[10]" [{| [1,2,3] |}])
      ; assert_equal ["1";"2";"3"] (exec ".a[]" [{| {"a":[1,2,3]} |}])
      ; assert_equal ["1";"2"] (exec ".[]" [{| {"a":1,"b":2} |}])
      ; assert_equal [{|"c"|}] (exec ".a | .b" [{| {"a":{"b":"c"}} |}])
      ; assert_equal ["1";"2"] (exec ".[] | .a" [{| [{"a":1},{"a":2}] |}])
      ; assert_equal [{|"c"|}] (exec ".a | .b" [{| {"a":{"b":"c"}} |}])
      ; assert_equal [{|["c"]|}] (exec "[.a]" [{| {"a":"c"} |}])
      ; assert_equal ["1"; "2"] (exec "(.a,.b)" [{| {"a":1, "b":2} |}])
      ; assert_equal ["1"] (exec {|.["a"]|} [{| {"a":1, "b":2} |}])
      ; assert_equal ["1"; "null"] (exec {|(1,.a)|} [{| {"b": 1} |}])
      ; assert_equal ["null"] (exec {|.a|} [{| {"b":2} |}])
      ; assert_equal ["1"] (exec {|(1,.a?)|} [{| [] |}])
      ; assert_equal ["2"] (exec {|.a // 2|} [{| {"b": 1} |}])
      ; assert_equal ["2"] (exec {|.a // 2|} [{| {"a": false} |}])
      ; assert_equal ["[2,3,4]"] (exec ".[1:]" [{| [1,2,3,4] |}])
      ; assert_equal ["[1,2,3]"] (exec ".[:3]" [{| [1,2,3,4] |}])
      ; assert_equal ["[2,3]"] (exec ".[1:3]" [{| [1,2,3,4] |}])
      ; assert_equal
          (List.sort Stdlib.compare [{|{"a":{"b":1},"c":"d","e":[2,3]}|}; {|"d"|}; {|{"b":1}|}; "1"; "[2,3]"; "2"; "3"])
          (List.sort Stdlib.compare (exec ".." [{| {"a":{"b":1}, "c":"d", "e":[2,3]} |}]))
      ; assert_equal ["3"] (exec {|.a + .b|} [{| {"a": 1, "b":2} |}])
      ; assert_equal ["[1,2]"] (exec {|.a + .b|} [{| {"a": [1], "b":[2]} |}])
      ; assert_equal [{|"12"|}] (exec {|.a + .b|} [{| {"a": "1", "b":"2"} |}])
      ; assert_equal [{|{"c":1,"d":2}|}] (exec {|.a + .b|} [{| {"a": {"c":1}, "b": {"d":2}} |}])
      ; assert_equal [{|{"d":2}|}] (exec {|.a + .b|} [{| {"a": {"d":1}, "b": {"d":2}} |}])
      ; assert_equal ["3.0"] (exec {|.a + .b|} [{| {"a": 1, "b":2.0} |}])
      ; assert_equal ["4"; "5"; "5"; "6"] (exec {|(.a,.b) + (.c,.d)|} [{| {"a": 1, "b":2, "c":3, "d":4} |}])
      ; assert_equal ["-1"] (exec {|.a - .b|} [{| {"a": 1, "b":2} |}])
      ; assert_equal ["1.0"] (exec {|.a / .b|} [{| {"a": 1, "b":1} |}])
      ; assert_equal ["0.5"] (exec {|.a / .b|} [{| {"a": 1, "b":2} |}])
      ; assert_equal ["0"] (exec {|.a % .b|} [{| {"a": 1, "b":1} |}])
      ; assert_equal ["0.0"] (exec {|.a % .b|} [{| {"a": 1, "b":1.0} |}])
      ; assert_equal ["[3]"] (exec {|.a - .b|} [{| {"a": [1,2,3], "b":[1,2]} |}])
      ; assert_equal ["[]"] (exec {|.a - .b|} [{| {"a": [1,2,3], "b":[1,2,3]} |}])
      ; assert_equal [{|"xx"|}] (exec {|.a * .b|} [{| {"a": "x", "b":2.99} |}])
      ; assert_equal [{|null|}] (exec {|.a * .b|} [{| {"a": "x", "b":-1} |}])
      ; assert_equal [{|{"k":{"a":0,"b":2,"c":3}}|}]
          (exec {|{"k": {"a": 1, "b": 2}} * {"k": {"a": 0,"c": 3}}|} [{| null |}])
      ; assert_equal [{|["x","z"]|}] (exec {|.a / .b|} [{| {"a": "xyz", "b":"y"} |}])
      ; assert_equal [{|{"a":"y","b":"xyz"}|}] (exec {|{b: .a, a: .b}|} [{| {"a": "xyz", "b":"y"} |}])
      ; assert_equal ["3"] (exec {|. | length|} [{| "abc" |}])

      ; assert_equal ["4"] (exec {|. | length|} [{| "abc\u263A" |}])
      ; assert_equal ["3"] (exec {|. | length|} [{| [1,2,3] |}])
      ; assert_equal ["3"] (exec {|. | length|} [{| {"a":1, "b":2, "c":4} |}])
      ; assert_equal ["0"] (exec {|. | length|} [{| null |}])
      ; assert_equal ["2"] (exec {|. | utf8bytelength|} [{| "\u03bc" |}])
      ; assert_equal [{|["a","b","c"]|}] (exec {|. | keys|} [{| {"a":1, "b":2, "c":4} |}])
      ; assert_equal [{|[0,1,2]|}] (exec {|. | keys|} [{| [1,2,3] |}])
      ; assert_equal [{|["a","b","c"]|}] (exec {|. | keys_unsorted|} [{| {"a":1, "b":2, "c":4} |}])
      ; assert_equal [{|[0,1,2]|}] (exec {|. | keys_unsorted|} [{| [1,2,3] |}])
      ; assert_equal ["true"; "false"] (exec {|has(.a,.c)|} [{| {"a":"b", "c":"e", "b":1} |}])
      ; assert_equal ["false"] (exec {|has(1)|} [{| [] |}])
      ; assert_equal ["true"; "false"; "false"; "false"] (exec {|has(1,-1,-10,10)|} [{| [1,2] |}])
      ; assert_equal ["true"; "false"] (exec {|.[] | in({"foo": 42})|} [{| ["foo", "bar"] |}])
      ; assert_equal ["false"; "true"] (exec {|.[] | in([0,1])|} [{| [2, 0] |}])
      ; assert_equal ["[1,2]"] (exec {|def map(f): [.[] | f]; map(. + 1)|} [{| [0, 1] |}])
      ; assert_equal ["true"; "false"; "false"] (exec {|.i==1|} [{| {"i": 1} |}; {| {"i": 2} |}; {| {"i": 3} |}])
      ; assert_equal [{|{"i":1}|}] (exec {|select(.i==1)|} [{| {"i": 1} |}; {| {"i": 2} |}; {| {"i": 3} |}])
      ; assert_equal ["true"; "true"] (exec {|true|} [{| {"i": 1} |}; {| {"i": 2} |}])
      ; assert_equal [{|{"i":1}|}; {|{"i":1}|}; {|{"i":2}|}; {|{"i":3}|}] (exec {|select(true, .i==1)|} [{| {"i": 1} |}; {| {"i": 2} |}; {| {"i": 3} |}])
      ; assert_equal [{|{"ab":59}|}] (exec {|{("a"+"b"): 59}|} ["null"])
      ; assert_equal [{|{"foo":42}|}] (exec {|{foo: .bar}|} [{|{"bar":42, "baz":43}|}])
      ; assert_equal [{|{"title":"Boss","user":"Joe"}|}] (exec {|{user, title}|} [{|{user: "Joe", title: "Boss", id: 32}|}])
      ; assert_equal [{|"foo"|}] (exec {|"foo" as $x | $x|} [{|0|}])
      ; assert_equal ["1"; "2"; "2"; "3"; "3"; "4"]
          (exec {|(.i,.i+1) as $x | $x|} [{| {"i": 1} |}; {| {"i": 2} |}; {| {"i": 3} |}])
      )
  ; "simplest-2" >:: (fun ctxt ->
        ()
      ; assert_equal [{|0|}] (exec {|0|} [{|0|}])
      ; assert_equal [{|0|}] (exec {|0|} [{|0|}])
      )
  ; "errors" >:: (fun ctxt ->
        assert_raises_exn_pattern
          "interp0: cannot deref object with non-string"
          (fun () -> exec ".[0]" ["{}"])
      ; assert_raises_exn_pattern
          "interp0: cannot deref array with non-int"
          (fun () -> exec {|.["a"]|} ["[]"])
      ; assert_raises_exn_pattern
          "array_list: not an array"
          (fun () -> exec {|.[]|} ["1"])
      ; assert_raises_exn_pattern
        "floating-point division produce non-numeric result"
        (fun () -> exec {|.a / .b|} [{| {"a": 1, "b":0} |}])
      ; assert_raises_exn_pattern
        "arguments to addition were wrong types"
        (fun () -> exec {|.a + .b|} [{| {"a": 1, "b":"0"} |}])
      ; assert_raises_exn_pattern
          "interp0: exp (ExpDataBind ((ExpString \"foo\"), \"x\")) MUST be part of a sequence of filters"
          (fun () -> exec {|"foo" as $x|} [{| null |}])
      )
  ; "errors-2" >:: (fun ctxt ->
        ()

      ; assert_raises_exn_pattern
        "arguments to addition were wrong types"
        (fun () -> exec {|.a + .b|} [{| {"a": 1, "b":"0"} |}])
      )

  ; "." >:: (fun ctxt ->
        assert_equal
          ["null"] (exec "." ["null"])
      )
  ; ".[0]" >:: (fun ctxt ->
        assert_equal
          ["0"] (exec ".[0]" ["[0]"])
      )
  ]



let tests = "all" >::: [
    parsing
  ; execute
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
