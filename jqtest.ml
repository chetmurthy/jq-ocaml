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
      )
  ]

type string_list = string list [@@deriving show,eq]

let printer = show_string_list

let execute = "execute" >:::
              let assert_equal = assert_equal ~printer in [
    "simplest" >:: (fun ctxt ->
        assert_equal [] (exec "." [])
      ; assert_equal ["0"] (exec "0" ["null"])
      ; assert_equal [{|"b"|}] (exec ".a" [{| {"a":"b"} |}])
      ; assert_equal [{|{}|}] (exec "{}" [{| {} |}])
      ; assert_equal [{|{"a":"d"}|}] (exec "{a: .b}" [{| {"b":"d"} |}])
      ; assert_equal [{|{"a":"d","b":"c"}|}] (exec "{a: .b, b: .a}" [{| {"a":"c", "b":"d"} |}])
      ; assert_equal [{|{"a":"d","b":"c"}|}] (exec "{a: .b.a, b: .a.b}" [{| {"a":{"b": "c"}, "b":{"a": "d"}} |}])
      ; assert_equal ["1";"2";"3"] (exec ".a[]" [{| {"a":[1,2,3]} |}])
      ; assert_equal ["1";"2"] (exec ".[]" [{| {"a":1,"b":2} |}])
      ; assert_equal [{|"c"|}] (exec ".a | .b" [{| {"a":{"b":"c"}} |}])
      ; assert_equal ["1";"2"] (exec ".[] | .a" [{| [{"a":1},{"a":2}] |}])
      ; assert_equal [{|"c"|}] (exec ".a | .b" [{| {"a":{"b":"c"}} |}])
      ; assert_equal [{|["c"]|}] (exec "[.a]" [{| {"a":"c"} |}])
      ; assert_equal ["1"; "2"] (exec "(.a,.b)" [{| {"a":1, "b":2} |}])
      ; assert_equal ["1"] (exec {|.["a"]|} [{| {"a":1, "b":2} |}])
      ; assert_equal ["null"] (exec {|.a?|} [{| {"b":2} |}])
      ; assert_equal ["1"; "null"] (exec {|(1,.a)?|} [{| {"b": 1} |}])
      )
  ; "simplest-2" >:: (fun ctxt ->
        ()
      ; assert_equal ["1"; "null"] (exec {|(1,.a)?|} [{| {"b": 1} |}])
      )
  ; "errors" >:: (fun ctxt ->
        assert_raises_exn_pattern
          "interp0: cannot deref object with non-string"
          (fun () -> exec ".[0]" ["{}"])
      ; assert_raises_exn_pattern
          "interp0: cannot deref array with non-int"
          (fun () -> exec {|.["a"]|} ["[]"])
      ; assert_raises_exn_pattern
          "object_field: field a not found"
          (fun () -> exec {|.["a"]|} [{|{"b":1}|}])
      ; assert_raises_exn_pattern
          "array_list: not an array"
          (fun () -> exec {|.[]|} ["1"])
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
