open Asttools
open Yojson.Basic

open Lazy_reclist
open Jqutil
open Jqtypes

let inRight ll = Right ll
let inLeft v = Left v

let rec interp e (j : t) : (t, t ll_t) choice =
  match e with
    ExpDot -> Right (of_list [j])
  | ExpInt n -> Right (of_list [`Int n])
  | ExpString s -> Right (of_list [`String s])

  | ExpDeref (e1, e2) ->
    j
    |> interp e1
    |> of_choice
    |> map (function (j' : t) ->
        j
        |> interp e2 |> of_choice
        |> map (function (`Int n : t) -> Left (array_deref n j'))
        |> inRight
      )
  |> inRight

  | ExpDotField f ->
    j |> object_field f |> inLeft

  | ExpField (e,f) ->
    j |> interp e |> of_choice |> map (fun j -> j |> object_field f |> inLeft) |> inRight

  | ExpDict [] ->
     j |> (fun j -> inLeft (`Assoc []))

  | ExpDict ((ke,ve)::l) ->
    j
    |> interp ke
    |> of_choice
    |> map (fun (`String k : t) -> j |> interp ve |> of_choice |> map (fun v ->
        j
        |> interp (ExpDict l)
        |> of_choice
        |> map (fun (`Assoc l : t) ->
            Left(`Assoc ((k,v)::l)))
        |> inRight
      )
                                   |> inRight
      )
    |> inRight


  | e -> failwith Fmt.(str "interp: unrecognized exp %a" pp_exp e)