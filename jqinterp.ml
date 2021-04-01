open Asttools
open Yojson.Basic

open Lazy_reclist
open Jqutil
open Jqtypes

let rec interp e (j : t) : (t, t ll_t) choice =
  match e with
    ExpDot -> Right (of_list [j])
  | ExpInt n -> Right (of_list [`Int n])

  | ExpDeref (e1, e2) ->
    j
    |> interp e1
    |> of_choice
    |> map (function (j' : t) ->
        j
        |> interp e2 |> of_choice
        |> map (function (`Int n : t) -> Left (array_deref n j'))
        |> (fun ll -> Right ll)
      )
  |> (fun ll -> Right ll)
