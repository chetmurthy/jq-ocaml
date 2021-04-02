open Asttools
open Yojson.Basic

open Lazy_reclist
open Jqutil
open Jqtypes

let inRight ll = Right ll
let inLeft v = Left v

type closure_t = path:bool -> t -> (t,t ll_t) choice
type fenv_t = (string * (closure_t list -> closure_t)) list

let functions = ref ([] : fenv_t)

let rec interp0 path (fenv : fenv_t) denv benv e (j : t) : (t, t ll_t) choice =
  match e with
    ExpDot when path -> Left (`List [])
  | ExpDot -> Right (of_list [j])

  | ExpInt _ when path -> Fmt.(jqexceptionf "path: invalid path expression %a" pp_exp e)
  | ExpInt n -> Right (of_list [`Int n])

  | ExpBool b -> Right (of_list [`Bool b])
  | ExpString s -> Right (of_list [`String s])

  | ExpEmpty -> Right nil

  | ExpDotField f ->
    j |> object_field  f |> inLeft

  | ExpField (e,f) ->
    j |> interp0 path fenv denv benv e |> of_choice |> map (fun j -> j |> object_field  f |> inLeft) |> inRight

  | ExpDict l ->
    let rec edrec l j = match l with
        [] ->
        j |> (fun j -> inLeft (`Assoc []))

      | ((ke,ve)::l) ->
        j
        |> interp0 path fenv denv benv ke
        |> of_choice
        |> map (fun (`String k : t) -> j |> interp0 path fenv denv benv ve |> of_choice |> map (fun v ->
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
    |> interp0 path fenv denv benv e
    |> of_choice
    |> map (fun j -> j |> array_list  |> inRight)
    |> inRight

  | ExpSeq(ExpDataBind(e1, id),e2) ->
    j
    |> interp0 path fenv denv benv e1
    |> of_choice
    |> map (fun j' ->
        j
        |> interp0 path fenv ((id, j')::denv) benv e2
      )
    |> inRight

  | ExpDataBind (_, _) as e ->
    Fmt.(failwithf "interp0: exp %a MUST be part of a sequence of filters" pp_exp e)

  | ExpDataVar s -> begin match List.assoc s denv with
        v -> Left v
      | exception Not_found -> Fmt.(jqexceptionf "variable $%s not found in data-environment" s)
    end

  | ExpSeq(ExpLabel id,e2) ->
    j |> interp0 path fenv denv (id::benv) e2

  | ExpLabel _ as e ->
    Fmt.(failwithf "interp0: exp %a MUST be part of a sequence of filters" pp_exp e)

  | ExpBreak s ->
    if List.mem s benv then  jqbreak s
    else
      Fmt.(failwithf "interp0: label %s was not lexically outer from break" s)

  | ExpReduce(e, id, init, step) ->
    j
    |> interp0 path fenv denv benv init
    |> of_choice
    |> map (fun jinit ->
        j
        |> interp0 path fenv denv benv e
        |> of_choice
        |> reduce benv (fun jv j' ->
            jv
            |> interp0 path fenv ((id, j')::denv) benv step
            |> of_choice)
          jinit
        |> inRight
      )
    |> inRight

  | ExpForeach(e, id, init, step, update) ->
    j
    |> interp0 path fenv denv benv init
    |> of_choice
    |> map (fun jinit ->
        j
        |> interp0 path fenv denv benv e
        |> of_choice
        |> foreach benv (fun jv j' ->
            jv
            |> interp0 path fenv ((id, j')::denv) benv step
            |> of_choice)
          (fun jv j' ->
             jv
             |> interp0 path fenv ((id, j')::denv) benv update
             |> of_choice)
          jinit
        |> inRight
      )
    |> inRight
    

  | ExpSeq(e1,e2) ->
    j
    |> interp0 path fenv denv benv e1
    |> of_choice
    |> map (interp0 path fenv denv benv e2)
    |> inRight

  | ExpCollect e ->
    j
    |> interp0 path fenv denv benv e
    |> of_choice
    |> to_list
    |> (fun l -> Left(`List l))

  | ExpConcat(e1,e2) ->
    j
    |> interp0 path fenv denv benv e1
    |> of_choice
    |> (fun ll1 -> j
                   |> interp0 path fenv denv benv e2
                   |> of_choice
                   |> (fun ll2 -> Right(cons_ll ll1 (cons_ll ll2 nil))))

  | ExpDeref(e1, e2) ->
    j
    |> interp0 path fenv denv benv e1
    |> of_choice
    |> map (fun j1 ->
        j
        |> interp0 path fenv denv benv e2
        |> of_choice
        |> map (fun j2 ->
            (match (j1, j2) with
               (`Assoc _, `String s) -> object_field  s j1
             | (`Null, `String _) -> `Null
             | (`List _, `Int n) -> array_deref  n j1
             | (`Null, `Int n) -> `Null
             | (`Assoc _, _) ->
               jqexception "interp0: cannot deref object with non-string"
             | (`List _, _) ->
               jqexception "interp0: cannot deref array with non-int"
            )
            |> inLeft)
        |> inRight)
    |> inRight

  | ExpAlt (e1, e2) -> begin
      let l = j
              |> interp0 path fenv denv benv e1
              |> of_choice
              |> to_list in
      if List.for_all (fun x -> x = `Null || x = `Bool false) l then
        j |> interp0 path fenv denv benv e2
      else l |> of_list |> inRight
  end

  | ExpNeg e ->
    j
    |> interp0 path fenv denv benv e
    |> of_choice
    |> map (function `Int n -> Left(`Int (- n)))
    |> inRight

  | ExpSlice(e, Some e1, None) ->
    j
    |> interp0 path fenv denv benv e
    |> of_choice
    |> map (function `List l ->
        j
        |> interp0 path fenv denv benv e1
        |> of_choice
        |> map (function `Int n -> Left (`List (slice (Some n) None l)))
        |> inRight)
    |> inRight

  | ExpSlice(e, None, Some e2) ->
    j
    |> interp0 path fenv denv benv e
    |> of_choice
    |> map (function `List l ->
        j
        |> interp0 path fenv denv benv e2
        |> of_choice
        |> map (function `Int m -> Left (`List (slice None (Some m) l)))
        |> inRight)
    |> inRight

  | ExpSlice(e, Some e1, Some e2) ->
    j
    |> interp0 path fenv denv benv e
    |> of_choice
    |> map (function `List l ->
        j
        |> interp0 path fenv denv benv e1
        |> of_choice
        |> map (function `Int n ->
            j
            |> interp0 path fenv denv benv e2
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
    binop path fenv denv benv (function ((j1 : t) , (j2 : t)) -> match (j1, j2) with
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
        | _ -> jqexception "arguments to addition were wrong types"
      )
      e1 e2 j

  | ExpSub (e1,e2) ->
    binop path fenv denv benv (function ((j1 : t) , (j2 : t)) -> match (j1, j2) with
          (`Int n, `Int m) -> `Int(n-m)
        | (`Float n, `Int m) -> `Float(n -. float_of_int m)
        | (`Int n, `Float m) -> `Float(float_of_int n -. m)
        | (`Float n, `Float m) -> `Float(n -. m)
        | (`List l1, `List l2) -> `List (array_sub l1 l2)
      )
      e1 e2 j

  | ExpMul (e1,e2) ->
    binop path fenv denv benv (function ((j1 : t) , (j2 : t)) -> match (j1, j2) with
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
      else jqexception "floating-point division produce non-numeric result" in
    binop path fenv denv benv (function ((j1 : t) , (j2 : t)) -> match (j1, j2) with
          (`Int n, `Int m) -> `Float(div_float (float_of_int n) (float_of_int m))
        | (`Float n, `Int m) -> `Float(div_float n (float_of_int m))
        | (`Int n, `Float m) -> `Float(div_float (float_of_int n) m)
        | (`Float n, `Float m) -> `Float(div_float n m)
        | (`String s1, `String s2) -> `List(Str.(split (regexp s2) s1) |> List.map (fun s -> `String s))
      )
      e1 e2 j

  | ExpMod (e1,e2) ->
    let mod_float n m = fst(modf(n /. m)) in
    binop path fenv denv benv (function ((j1 : t) , (j2 : t)) -> match (j1, j2) with
          (`Int n, `Int m) -> `Int(n mod m)
        | (`Float n, `Int m) -> `Float(mod_float n (float_of_int m))
        | (`Int n, `Float m) -> `Float(mod_float (float_of_int n) m)
        | (`Float n, `Float m) -> `Float(mod_float n m)
      )
      e1 e2 j

  | ExpFuncall("path", _) when path -> Fmt.(jqexceptionf "path: invalid path expression %a" pp_exp e)
  | ExpFuncall("path", [e1]) ->
    j
    |> interp0 true fenv denv benv e1

  | ExpFuncall(f, l) ->
    let argcl = List.map (fun e ~path -> interp0 path fenv denv benv e) l in
    let code =
      match List.assoc f fenv with
        f -> f
      | exception Not_found -> Fmt.(failwithf "interp: function %a not found" Dump.string f)
    in
    j
    |> (code ~path) argcl

  | ExpFuncDef((fname, formals, body), e) ->
    let fcode actuals ~path j =
      if List.length formals <> List.length actuals then
        Fmt.(failwithf "function %a: formal-actual length mismatch" Dump.string fname) ;
      let newenv = List.map2 (fun f a ->
          (f, fun [] -> a)) formals actuals in
      j |> interp0 path (newenv@fenv) denv benv body in
    j |> interp0 path ((fname, fcode)::fenv) denv benv e

  | ExpEq (e1, e2) ->
    j
    |> interp0 path fenv denv benv e1
    |> of_choice
    |> map (fun j1 ->
        j
        |> interp0 path fenv denv benv e2
        |> of_choice
        |> map (fun j2 -> Left (`Bool (j1 = j2)))
        |> inRight)
    |> inRight

  | ExpNe (e1, e2) ->
    j
    |> interp0 path fenv denv benv e1
    |> of_choice
    |> map (fun j1 ->
        j
        |> interp0 path fenv denv benv e2
        |> of_choice
        |> map (fun j2 -> Left (`Bool (j1 <> j2)))
        |> inRight)
    |> inRight
         
  | ExpLt (e1, e2) ->
    j
    |> interp0 path fenv denv benv e1
    |> of_choice
    |> map (fun j1 ->
        j
        |> interp0 path fenv denv benv e2
        |> of_choice
        |> map (fun j2 -> Left (`Bool (j1 < j2)))
        |> inRight)
    |> inRight
         
  | ExpGt (e1, e2) ->
    j
    |> interp0 path fenv denv benv e1
    |> of_choice
    |> map (fun j1 ->
        j
        |> interp0 path fenv denv benv e2
        |> of_choice
        |> map (fun j2 -> Left (`Bool (j1 > j2)))
        |> inRight)
    |> inRight
         
  | ExpLe (e1, e2) ->
    j
    |> interp0 path fenv denv benv e1
    |> of_choice
    |> map (fun j1 ->
        j
        |> interp0 path fenv denv benv e2
        |> of_choice
        |> map (fun j2 -> Left (`Bool (j1 <= j2)))
        |> inRight)
    |> inRight
         
  | ExpGe (e1, e2) ->
    j
    |> interp0 path fenv denv benv e1
    |> of_choice
    |> map (fun j1 ->
        j
        |> interp0 path fenv denv benv e2
        |> of_choice
        |> map (fun j2 -> Left (`Bool (j1 >= j2)))
        |> inRight)
    |> inRight
         
  | ExpCond([], e) ->
    j |> interp0 path fenv denv benv e

  | ExpCond((e1,e2)::l, e) ->
    j
    |> interp0 path fenv denv benv e1
    |> of_choice
    |> map (function
          `Bool false ->
          j
          |> interp0 path fenv denv benv (ExpCond(l, e))
        | _ ->
          j
          |> interp0 path fenv denv benv e2
      )
    |> inRight

  | ExpTryCatch (e1, e2) -> begin
      try
        j
        |> interp0 path fenv denv benv e1
        |> of_choice
        |> to_list
        |> of_list
        |> inRight
      with JQException msg ->
        (`String msg)
        |> interp0 path fenv denv benv e2
    end

  | e -> Fmt.(failwithf "interp0: unrecognized exp %a" pp_exp e)

and binop path fenv denv benv f e1 e2 j =
  j
  |> interp0 path fenv denv benv e1
  |> of_choice
  |> map (fun j1 ->
      j
      |> interp0 path fenv denv benv e2
      |> of_choice
      |> map (fun j2 ->
          let jr = f (j1, j2) in
          jr |> inLeft)
      |> inRight)
  |> inRight


let interp e j = interp0 false !functions [] [] e j
let path e j = interp0 true !functions [] [] e j

let add_function fname code =
  functions := (fname, code):: !functions
;;

let interp_tuple l j : (t, t ll_t) choice =
  let rec edrec l (j : t) = match l with
      [] ->
      j |> (fun j -> inLeft (`List []))

    | (f::l) ->
      j
      |> f |> of_choice |> map (fun v ->
          j
          |> edrec l
          |> of_choice
          |> map (fun (`List l : t) ->
              Left(`List (v::l)))
          |> inRight
        )
      |> inRight
  in
  j
  |> edrec l
;;

add_function "length"
  (function [] -> fun ~path -> function
        `String s -> Left (`Int(utf8_length s))
      | `List l -> Left (`Int(List.length l))
      | `Assoc l -> Left (`Int(List.length l))
      | `Null -> Left (`Int 0)
  )
;;

add_function "utf8bytelength"
  (function [] -> fun ~path -> function
        `String s -> Left (`Int(String.length s))
  )
;;

add_function "keys"
  (function [] -> fun ~path -> function
        `Assoc l -> Left (`List(List.sort Stdlib.compare (List.map (fun (k,_) -> `String k) l)))
      | `List l -> Left (`List(List.mapi (fun i _ -> `Int i) l))
  )
;;

add_function "keys_unsorted"
  (function [] -> fun ~path -> function
        `Assoc l -> Left (`List(List.map (fun (k,_) -> `String k) l))
      | `List l -> Left (`List(List.mapi (fun i _ -> `Int i) l))
  )
;;

add_function "has"
  (function
      [f0] ->
      fun ~path ->
      (function
          (`Assoc l as j) ->
          j
          |> f0 ~path
          |> of_choice
          |> map (function `String k ->
              Left(`Bool (List.mem_assoc k l)))
          |> inRight
        | (`List l as j) ->
          j
          |> f0 ~path
          |> of_choice
          |> map (function `Int n ->
              Left (`Bool (n >= 0 && n < List.length l)))
          |> inRight
      )
  )
;;

add_function "in"
  (function
      [f0] ->
      fun ~path ->
      (function
          (`String k as j) ->
          j
          |> f0 ~path
          |> of_choice
          |> map (function `Assoc l ->
              Left(`Bool (List.mem_assoc k l)))
          |> inRight
        | (`Int n as j) ->
          j
          |> f0 ~path
          |> of_choice
          |> map (function `List l ->
              Left (`Bool (n >= 0 && n < List.length l)))
          |> inRight
      )
  )
;;

add_function "select"
  (function
      [f0] ->
      fun ~path ->
      (function j ->
         j
         |> f0 ~path
         |> of_choice
         |> map (function `Bool false -> Right(of_list []) | _ -> Left j)
         |> inRight))
;;


add_function "error"
  (function
      [f0] ->
      fun ~path ->
      (function j ->
         j
         |> f0 ~path
         |> of_choice
         |> map (function `String msg -> jqexception msg)
         |> inRight
      )
  )
;;
