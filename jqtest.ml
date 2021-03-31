open OUnit2
open OUnitTest
open Pa_ppx_testutils

open Jqtypes
open Jqparse0

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




let simple = "simple" >::: [
    "simple" >:: (fun ctxt ->
        ()
      )
  ]

let of_string_exn s = s |> parse_string parse_exp_eoi



let tests = "all" >::: [
    simple
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
