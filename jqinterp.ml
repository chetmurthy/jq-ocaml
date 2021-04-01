open Asttools
open Yojson.Basic

open Lazy_reclist
open Jqutil
open Jqtypes

let inRight ll = Right ll
let inLeft v = Left v

let functions = ref ([] : (string * (t list -> t ->  (t, t ll_t) choice)) list)

let rec interp0  e (j : t) : (t, t ll_t) choice =
  match e with
    ExpDot -> Right (of_list [j])
  | ExpInt n -> Right (of_list [`Int n])
  | ExpString s -> Right (of_list [`String s])

  | ExpDotField f ->
    j |> object_field  f |> inLeft

  | ExpField (e,f) ->
    j |> interp0  e |> of_choice |> map (fun j -> j |> object_field  f |> inLeft) |> inRight

  | ExpDict l ->
    let rec edrec l j = match l with
        [] ->
        j |> (fun j -> inLeft (`Assoc []))

      | ((ke,ve)::l) ->
        j
        |> interp0  ke
        |> of_choice
        |> map (fun (`String k : t) -> j |> interp0  ve |> of_choice |> map (fun v ->
            j
            |> edrec l
            |> of_choice
            |> map (fun (`Assoc l : t) ->
                Left(`Assoc ((k,v)::l)))
            |> inRight
          )
                                       |> inRight
          )
        |> inRight
    in j
       |> edrec l
       |> of_choice
       |> map (function `Assoc l -> Left(`Assoc (sort_object_keys l)))
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

  | ExpAdd (e1,e2) ->
    binop (function ((j1 : t) , (j2 : t)) -> match (j1, j2) with
          (`Int n, `Int m) -> `Int(n+m)
        | (`Float n, `Int m) -> `Float(n +. float_of_int m)
        | (`Int n, `Float m) -> `Float(float_of_int n +. m)
        | (`Float n, `Float m) -> `Float(n +. m)
        | (`List l1, `List l2) -> `List(l1@l2)
        | (`String l1, `String l2) -> `String(l1^l2)
        | (`Assoc l1, `Assoc l2) ->
          `Assoc(List.fold_left (fun acc (k,v) ->
              if List.mem_assoc k acc then acc else (k,v)::acc)
              l2 l1)
        | (`Null, v) -> v
        | (v, `Null) -> v
        | _ -> raise (JQException "arguments to addition were wrong types")
      )
      e1 e2 j

  | ExpSub (e1,e2) ->
    binop (function ((j1 : t) , (j2 : t)) -> match (j1, j2) with
          (`Int n, `Int m) -> `Int(n-m)
        | (`Float n, `Int m) -> `Float(n -. float_of_int m)
        | (`Int n, `Float m) -> `Float(float_of_int n -. m)
        | (`Float n, `Float m) -> `Float(n -. m)
        | (`List l1, `List l2) -> `List (array_sub l1 l2)
      )
      e1 e2 j

  | ExpMul (e1,e2) ->
    binop (function ((j1 : t) , (j2 : t)) -> match (j1, j2) with
          (`Int n, `Int m) -> `Int(n*m)
        | (`Float n, `Int m) -> `Float(n *. float_of_int m)
        | (`Int n, `Float m) -> `Float(float_of_int n *. m)
        | (`Float n, `Float m) -> `Float(n *. m)
        | (`String s, `Int n) -> string_mul s n
        | (`String s, `Float n) -> string_mul s (Float.to_int n)
        | (`Assoc l1, `Assoc l2) -> `Assoc (object_mul l1 l2)
      )
      e1 e2 j

  | ExpDiv (e1,e2) ->
    let div_float n m =
      let r = n /. m in
      if Float.is_finite r then r
      else raise (JQException "floating-point division produce non-numeric result") in
    binop (function ((j1 : t) , (j2 : t)) -> match (j1, j2) with
          (`Int n, `Int m) -> `Float(div_float (float_of_int n) (float_of_int m))
        | (`Float n, `Int m) -> `Float(div_float n (float_of_int m))
        | (`Int n, `Float m) -> `Float(div_float (float_of_int n) m)
        | (`Float n, `Float m) -> `Float(div_float n m)
        | (`String s1, `String s2) -> `List(Str.(split (regexp s2) s1) |> List.map (fun s -> `String s))
      )
      e1 e2 j

  | ExpMod (e1,e2) ->
    let mod_float n m = fst(modf(n /. m)) in
    binop (function ((j1 : t) , (j2 : t)) -> match (j1, j2) with
          (`Int n, `Int m) -> `Int(n mod m)
        | (`Float n, `Int m) -> `Float(mod_float n (float_of_int m))
        | (`Int n, `Float m) -> `Float(mod_float (float_of_int n) m)
        | (`Float n, `Float m) -> `Float(mod_float n m)
      )
      e1 e2 j

  | ExpFuncall(f, l) ->
    let rec edrec l j = match l with
        [] ->
        j |> (fun j -> inLeft (`List []))

      | (ve::l) ->
        j
        |> interp0 ve |> of_choice |> map (fun v ->
            j
            |> edrec l
            |> of_choice
            |> map (fun (`List l : t) ->
                Left(`List (v::l)))
            |> inRight
          )
        |> inRight
    in
    let code = List.assoc f !functions in
    j
    |> edrec l
    |> of_choice
    |> map (function `List l -> j |> code l)
    |> inRight


  | e -> failwith Fmt.(str "interp0: unrecognized exp %a" pp_exp e)

and binop f e1 e2 j =
  j
  |> interp0 e1
  |> of_choice
  |> map (fun j1 ->
      j
      |> interp0 e2
      |> of_choice
      |> map (fun j2 ->
          let jr = f (j1, j2) in
          jr |> inLeft)
      |> inRight)
  |> inRight


let interp e j = interp0 e j

let add_function fname code =
  functions := (fname, code):: !functions
;;

add_function "length"
  (function [] -> function
        `String s -> Left (`Int(utf8_length s))
      | `List l -> Left (`Int(List.length l))
      | `Assoc l -> Left (`Int(List.length l))
      | `Null -> Left (`Int 0)
  )
;;

add_function "utf8bytelength"
  (function [] -> function
        `String s -> Left (`Int(String.length s))
  )
;;

add_function "keys"
  (function [] -> function
        `Assoc l -> Left (`List(List.sort Stdlib.compare (List.map (fun (k,_) -> `String k) l)))
      | `List l -> Left (`List(List.mapi (fun i _ -> `Int i) l))
  )
;;

add_function "keys_unsorted"
  (function [] -> function
        `Assoc l -> Left (`List(List.map (fun (k,_) -> `String k) l))
      | `List l -> Left (`List(List.mapi (fun i _ -> `Int i) l))
  )
;;

add_function "has"
  (function
      [`String k] ->
      (function
          `Assoc l ->
          Left(`Bool (List.mem_assoc k l))
      )
    | [`Int n] ->
      (function
          `List l ->
          Left (`Bool (n >= 0 && n < List.length l))
      )
  )
;;

add_function "in"
  (function
      [`Assoc l] ->
      (function
          `String k ->
          Left(`Bool (List.mem_assoc k l))
      )
    | [`List l] ->
      (function
          `Int n ->
          Left (`Bool (n >= 0 && n < List.length l))
      )
  )
;;
