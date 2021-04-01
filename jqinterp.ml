open Asttools
open Yojson.Basic

open Lazy_reclist
open Jqutil
open Jqtypes

let inRight ll = Right ll
let inLeft v = Left v

let rec interp0  e (j : t) : (t, t ll_t) choice =
  match e with
    ExpDot -> Right (of_list [j])
  | ExpInt n -> Right (of_list [`Int n])
  | ExpString s -> Right (of_list [`String s])

  | ExpDotField f ->
    j |> object_field  f |> inLeft

  | ExpField (e,f) ->
    j |> interp0  e |> of_choice |> map (fun j -> j |> object_field  f |> inLeft) |> inRight

  | ExpDict [] ->
     j |> (fun j -> inLeft (`Assoc []))

  | ExpDict ((ke,ve)::l) ->
    j
    |> interp0  ke
    |> of_choice
    |> map (fun (`String k : t) -> j |> interp0  ve |> of_choice |> map (fun v ->
        j
        |> interp0  (ExpDict l)
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
    |> interp0  e
    |> of_choice
    |> map (fun j -> j |> array_list  |> inRight)
    |> inRight

  | ExpSeq(e1,e2) ->
    j
    |> interp0  e1
    |> of_choice
    |> map (interp0  e2)
    |> inRight

  | ExpCollect e ->
    j
    |> interp0  e
    |> of_choice
    |> to_list
    |> (fun l -> Left(`List l))

  | ExpConcat(e1,e2) ->
    j
    |> interp0  e1
    |> of_choice
    |> (fun ll1 -> j
                   |> interp0  e2
                   |> of_choice
                   |> (fun ll2 -> Right(cons_ll ll1 (cons_ll ll2 nil))))

  | ExpDeref(e1, e2) ->
    j
    |> interp0  e1
    |> of_choice
    |> map (fun j1 ->
        j
        |> interp0  e2
        |> of_choice
        |> map (fun j2 ->
            (match (j1, j2) with
               (`Assoc _, `String s) -> object_field  s j1
             | (`List _, `Int n) -> array_deref  n j1
             | (`Assoc _, _) ->
               raise (JQException "interp0: cannot deref object with non-string")
             | (`List _, _) ->
               raise (JQException "interp0: cannot deref array with non-int")
            )
            |> inLeft)
        |> inRight)
    |> inRight

  | ExpQuestion e -> begin
    try
      let l = j
              |> interp0 e
              |> of_choice
              |> to_list in
      l |> of_list |> inRight
    with JQException _ -> nil |> inRight
  end

  | ExpAlt (e1, e2) -> begin
      let l = j
              |> interp0 e1
              |> of_choice
              |> to_list in
      if List.for_all (fun x -> x = `Null || x = `Bool false) l then
        j |> interp0 e2
      else l |> of_list |> inRight
  end

  | ExpNeg e ->
    j
    |> interp0 e
    |> of_choice
    |> map (function `Int n -> Left(`Int (- n)))
    |> inRight

  | ExpSlice(e, Some e1, None) ->
    j
    |> interp0 e
    |> of_choice
    |> map (function `List l ->
        j
        |> interp0 e1
        |> of_choice
        |> map (function `Int n -> Left (`List (slice (Some n) None l)))
        |> inRight)
    |> inRight

  | ExpSlice(e, None, Some e2) ->
    j
    |> interp0 e
    |> of_choice
    |> map (function `List l ->
        j
        |> interp0 e2
        |> of_choice
        |> map (function `Int m -> Left (`List (slice None (Some m) l)))
        |> inRight)
    |> inRight

  | ExpSlice(e, Some e1, Some e2) ->
    j
    |> interp0 e
    |> of_choice
    |> map (function `List l ->
        j
        |> interp0 e1
        |> of_choice
        |> map (function `Int n ->
            j
            |> interp0 e2
            |> of_choice
            |> map (function `Int m -> Left (`List (slice (Some n) (Some m) l)))
            |> inRight)
        |> inRight)
    |> inRight

  | ExpRecurse ->
    let rec rrec j (acc : t list) =
      let acc = j::acc in
      match j with
        `List l -> List.fold_right rrec l acc
      | `Assoc l -> List.fold_right rrec (List.map snd l) acc
      | _ -> acc in
    rrec j [] |> List.rev |> of_list |> inRight

  | e -> failwith Fmt.(str "interp0: unrecognized exp %a" pp_exp e)

let interp e j = interp0 e j
