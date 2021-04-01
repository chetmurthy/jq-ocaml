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
      )
  ]

let execute = "execute" >::: [
    "simplest" >:: (fun ctxt ->
        assert_equal [] (exec "." [])
      ; assert_equal ["0"] (exec "0" ["null"])
      ; assert_equal [{|"b"|}] (exec ".a" [{| {"a":"b"} |}])
      ; assert_equal [{|"c"|}] (exec ".a.b" [{| {"a":{"b":"c"}} |}])
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
