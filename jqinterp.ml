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

  | ExpBrackets e ->
    j
    |> interp e
    |> of_choice
    |> map (function `List l -> Right (of_list l))
    |> inRight

  | ExpSeq(e1,e2) ->
    j
    |> interp e1
    |> of_choice
    |> map (interp e2)
    |> inRight

  | ExpCollect e ->
    j
    |> interp e
    |> of_choice
    |> to_list
    |> (fun l -> Left(`List l))

  | ExpConcat(e1,e2) ->
    j
    |> interp e1
    |> of_choice
    |> (fun ll1 -> j
                   |> interp e2
                   |> of_choice
                   |> (fun ll2 -> Right(cons_ll ll1 (cons_ll ll2 nil))))

  | ExpDeref(e1, e2) ->
    j
    |> interp e1
    |> of_choice
    |> map (fun j1 ->
        j
        |> interp e2
        |> of_choice
        |> map (fun j2 ->
            (match (j1, j2) with
               (`Assoc _, `String s) -> object_field s j1
             | (`List _, `Int n) -> array_deref n j1
             | (`Assoc _, _) ->
               raise (JQException "interp: cannot deref object with non-string")
             | (`List _, _) ->
               raise (JQException "interp: cannot deref array with non-int")
            )
            |> inLeft)
        |> inRight)
    |> inRight


  | e -> failwith Fmt.(str "interp: unrecognized exp %a" pp_exp e)
