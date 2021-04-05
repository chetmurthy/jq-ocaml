open Asttools
open Yojson.Basic

open Lazy_reclist
open Jqutil
open Jqtypes

let inRight ll = Right ll
let inLeft v = Left v

module type CARRIER = sig
  type t
  val to_json : t -> Yojson.Basic.t
  val from_json : Yojson.Basic.t -> t
  val object_field : string -> t -> t
  val array_list : t -> t ll_t
  val array_deref : int -> t -> t
end

module type INTERP = sig
  module C : CARRIER
  type t = C.t
  type closure_t = t -> (t, t ll_t) choice
  type fenv_t = ((string * int) * (closure_t list -> closure_t)) list
  type env_t = fenv_t * (string * t) list * string list
  val map_to_json :
    (Yojson.Basic.t -> (t, t ll_t) choice) -> t ll_t -> t ll_t
  val predefined_functions : fenv_t ref
  val interp0 : env_t -> exp -> closure_t
  val interp0_funcdefs : env_t -> funcdef list -> env_t
  val binop :
    env_t -> (Yojson.Basic.t * Yojson.Basic.t -> Yojson.Basic.t) ->
    exp ->
    exp -> closure_t
  val interp : ?fenv:fenv_t -> exp -> closure_t
  val add_function : string * int -> (closure_t list -> closure_t) -> unit
  val interp_tuple : (closure_t) list -> closure_t
  val exec0 : ?fenv:fenv_t -> exp -> Yojson.Basic.t list -> t ll_t
  val exec : ?fenv:fenv_t -> exp -> Yojson.Basic.t list -> t list
end


module Gen(C : CARRIER) : (INTERP with module C = C) = struct
  module C = C
type t = C.t
type closure_t = t -> (t,t ll_t) choice
type fenv_t = ((string * int) * (closure_t list -> closure_t)) list
type env_t = fenv_t * (string * t) list * string list

let map_to_json f (ll : t ll_t) : t ll_t =
  map (fun j -> j |> C.to_json |> f) ll

let predefined_functions = ref ([] : fenv_t)
let rec interp0 env e (j : t) : (t, t ll_t) choice =
  match e with
    ExpDot -> Right (of_list [j])

  | ExpInt n -> Right (of_list [C.from_json (`Int n)])
  | ExpFloat n -> Right (of_list [C.from_json (`Float n)])

  | ExpNull -> Right (of_list [C.from_json `Null])

  | ExpBool b -> Right (of_list [C.from_json (`Bool b)])

  | ExpString s -> Right (of_list [C.from_json (`String s)])

  | ExpEmpty -> Right nil

  | ExpDotField f ->
    j |> C.object_field f |> inLeft

  | ExpField (e,f) ->
    j |> interp0 env e |> of_choice |> map (fun j -> j |> C.object_field f |> inLeft) |> inRight

  | ExpDict l ->
    let rec edrec l j = match l with
        [] ->
        j |> (fun j -> inLeft (C.from_json (`Assoc [])))

      | ((ke,ve)::l) ->
        j
        |> interp0 env ke
        |> of_choice
        |> map_to_json (fun (`String k) -> j |> interp0 env ve |> of_choice |> map_to_json (fun v ->
            j
            |> edrec l
            |> of_choice
            |> map_to_json (fun (`Assoc l) ->
                Left(C.from_json (`Assoc ((k,v)::l))))
            |> inRight
          )
                                       |> inRight
          )
        |> inRight
    in j
       |> edrec l
       |> of_choice
       |> map_to_json (function `Assoc l -> Left(C.from_json (`Assoc (sort_object_keys l))))
       |> inRight

  | ExpBrackets e ->
    j
    |> interp0 env e
    |> of_choice
    |> map (fun j -> j |> C.array_list  |> inRight)
    |> inRight

  | ExpSeq(ExpDataBind(e1, id),e2) ->
    let (fenv, denv, benv) = env in
    j
    |> interp0 env e1
    |> of_choice
    |> map (fun j' ->
        j
        |> interp0 (fenv, (id, j')::denv, benv) e2
      )
    |> inRight

  | ExpDataBind (_, _) as e ->
    Fmt.(failwithf "interp0: exp %a MUST be part of a sequence of filters" pp_exp e)

  | ExpDataVar s ->
    let (fenv, denv, benv) = env in
    begin match List.assoc s denv with
        v -> Left v
      | exception Not_found -> Fmt.(jqexceptionf "variable $%s not found in data-environment" s)
    end

  | ExpSeq(ExpLabel id,e2) ->
    let (fenv, denv, benv) = env in
    let benv = id::benv in
    j
    |> interp0 (fenv, denv, benv) e2
    |> of_choice
    |> (fun ll ->
        let rec truncate ll =
          match match_ll ll with
            None -> nil
          | Some(v,ll) -> lazy (Lazy.force (cons_it v (truncate ll)))
          | exception JQBreak s when List.mem s benv ->
            nil
        in truncate ll)
    |> inRight

  | ExpLabel _ as e ->
    Fmt.(failwithf "interp0: exp %a MUST be part of a sequence of filters" pp_exp e)

  | ExpBreak s ->
    let (fenv, denv, benv) = env in
    if List.mem s benv then Right (lazy (Lazy.force (jqbreak s)))
    else
      Fmt.(failwithf "interp0: label %s was not lexically outer from break" s)

  | ExpReduce(e, id, init, step) ->
    let (fenv, denv, benv) = env in
    lazy (Lazy.force (j
                      |> interp0 env init
                      |> of_choice
                      |> map (fun jinit ->
                          j
                          |> interp0 env e
                          |> of_choice
                          |> reduce (fun jv j' ->
                              jv
                              |> interp0 (fenv, ((id, j')::denv), benv) step
                              |> of_choice)
                            jinit
                          |> inRight
                        )))
    |> inRight

  | ExpForeach(e, id, init, step, update) ->
    let (fenv, denv, benv) = env in
    j
    |> interp0 env init
    |> of_choice
    |> map (fun jinit ->
        j
        |> interp0 env e
        |> of_choice
        |> foreach (fun jv j' ->
            jv
            |> interp0 (fenv, ((id, j')::denv), benv) step
            |> of_choice)
          (fun jv j' ->
             jv
             |> interp0 (fenv, ((id, j')::denv), benv) update
             |> of_choice)
          jinit
        |> inRight
      )
    |> inRight
    

  | ExpSeq(e1,e2) ->
    j
    |> interp0 env e1
    |> of_choice
    |> map (interp0 env e2)
    |> inRight

  | ExpCollect e ->
    j
    |> interp0 env e
    |> of_choice
    |> to_list
    |> (fun l -> Left(C.from_json (`List (List.map C.to_json l))))

  | ExpConcat(e1,e2) ->
    j
    |> interp0 env e1
    |> of_choice
    |> (fun ll1 -> j
                   |> interp0 env e2
                   |> of_choice
                   |> (fun ll2 -> Right(cons_ll ll1 (cons_ll ll2 nil))))

  | ExpDeref(e1, e2) ->
    j
    |> interp0 env e1
    |> of_choice
    |> map (fun j1 ->
        j
        |> interp0 env e2
        |> of_choice
        |> map (fun j2 ->
            (match (C.to_json j1, C.to_json j2) with
               ((`Assoc _|`Null), `String s) -> C.object_field  s j1
             | ((`List _|`Null), `Int n) -> C.array_deref n j1
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
              |> interp0 env e1
              |> of_choice
              |> to_list in
      if List.for_all (fun x -> C.to_json x = `Null || C.to_json x = `Bool false) l then
        j |> interp0 env e2
      else l |> of_list |> inRight
  end

  | ExpNeg e ->
    j
    |> interp0 env e
    |> of_choice
    |> map_to_json (function `Int n -> Left(C.from_json (`Int (- n))))
    |> inRight

  | ExpSlice(e, Some e1, None) ->
    j
    |> interp0 env e
    |> of_choice
    |> map_to_json (function `List l ->
        j
        |> interp0 env e1
        |> of_choice
        |> map_to_json (function `Int n -> Left (C.from_json (`List (slice (Some n) None l))))
        |> inRight)
    |> inRight

  | ExpSlice(e, None, Some e2) ->
    j
    |> interp0 env e
    |> of_choice
    |> map_to_json (function `List l ->
        j
        |> interp0 env e2
        |> of_choice
        |> map_to_json (function `Int m -> Left (C.from_json (`List (slice None (Some m) l))))
        |> inRight)
    |> inRight

  | ExpSlice(e, Some e1, Some e2) ->
    j
    |> interp0 env e
    |> of_choice
    |> map_to_json (function `List l ->
        j
        |> interp0 env e1
        |> of_choice
        |> map_to_json (function `Int n ->
            j
            |> interp0 env e2
            |> of_choice
            |> map_to_json (function `Int m -> Left (C.from_json (`List (slice (Some n) (Some m) l))))
            |> inRight)
        |> inRight)
    |> inRight

  | ExpRecurse ->
    let rec rrec j =
      match C.to_json j with
        `List _ | `Assoc _ ->
        Right (cons_it j
                 (j
                  |> C.array_list
                  |> map rrec))
      | _ -> Left j in
    rrec j

  | ExpAdd (e1,e2) ->
    binop env (function ((j1 : Yojson.Basic.t) , (j2 : Yojson.Basic.t)) -> match (j1, j2) with
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
    binop env (function (j1 , j2) -> match (j1, j2) with
          (`Int n, `Int m) -> `Int(n-m)
        | (`Float n, `Int m) -> `Float(n -. float_of_int m)
        | (`Int n, `Float m) -> `Float(float_of_int n -. m)
        | (`Float n, `Float m) -> `Float(n -. m)
        | (`List l1, `List l2) -> `List (array_sub l1 l2)
      )
      e1 e2 j

  | ExpMul (e1,e2) ->
    binop env (function (j1 , j2) -> match (j1, j2) with
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
    binop env (function (j1 , j2) -> match (j1, j2) with
          (`Int n, `Int m) -> `Float(div_float (float_of_int n) (float_of_int m))
        | (`Float n, `Int m) -> `Float(div_float n (float_of_int m))
        | (`Int n, `Float m) -> `Float(div_float (float_of_int n) m)
        | (`Float n, `Float m) -> `Float(div_float n m)
        | (`String s1, `String s2) -> `List(Str.(split (regexp s2) s1) |> List.map (fun s -> `String s))
      )
      e1 e2 j

  | ExpMod (e1,e2) ->
    let mod_float n m = fst(modf(n /. m)) in
    binop env (function (j1 , j2) -> match (j1, j2) with
          (`Int n, `Int m) -> `Int(n mod m)
        | (`Float n, `Int m) -> `Float(mod_float n (float_of_int m))
        | (`Int n, `Float m) -> `Float(mod_float (float_of_int n) m)
        | (`Float n, `Float m) -> `Float(mod_float n m)
      )
      e1 e2 j

  | ExpFuncall(f, l) ->
    let (fenv, denv, benv) = env in
    let argcl = List.map (fun e -> interp0 env e) l in
    let code =
      let nargs = List.length l in
      match List.assoc (f, nargs) fenv with
        f -> f
      | exception Not_found -> Fmt.(failwithf "interp: function %a/%n not found" Dump.string f nargs)
    in
    j
    |> code argcl

  | ExpFuncDef(fd, e) ->
    let env = interp0_funcdef env fd in
    j |> interp0 env e

  | ExpEq (e1, e2) ->
    j
    |> interp0 env e1
    |> of_choice
    |> map_to_json (fun j1 ->
        j
        |> interp0 env e2
        |> of_choice
        |> map_to_json (fun j2 -> Left (C.from_json (`Bool (j1 = j2))))
        |> inRight)
    |> inRight

  | ExpNe (e1, e2) ->
    j
    |> interp0 env e1
    |> of_choice
    |> map_to_json (fun j1 ->
        j
        |> interp0 env e2
        |> of_choice
        |> map_to_json (fun j2 -> Left (C.from_json (`Bool (j1 <> j2))))
        |> inRight)
    |> inRight
         
  | ExpLt (e1, e2) ->
    j
    |> interp0 env e1
    |> of_choice
    |> map_to_json (fun j1 ->
        j
        |> interp0 env e2
        |> of_choice
        |> map_to_json (fun j2 -> Left (C.from_json (`Bool (j1 < j2))))
        |> inRight)
    |> inRight
         
  | ExpGt (e1, e2) ->
    j
    |> interp0 env e1
    |> of_choice
    |> map_to_json (fun j1 ->
        j
        |> interp0 env e2
        |> of_choice
        |> map_to_json (fun j2 -> Left (C.from_json (`Bool (j1 > j2))))
        |> inRight)
    |> inRight
         
  | ExpLe (e1, e2) ->
    j
    |> interp0 env e1
    |> of_choice
    |> map_to_json (fun j1 ->
        j
        |> interp0 env e2
        |> of_choice
        |> map_to_json (fun j2 -> Left (C.from_json (`Bool (j1 <= j2))))
        |> inRight)
    |> inRight
         
  | ExpGe (e1, e2) ->
    j
    |> interp0 env e1
    |> of_choice
    |> map_to_json (fun j1 ->
        j
        |> interp0 env e2
        |> of_choice
        |> map_to_json (fun j2 -> Left (C.from_json (`Bool (j1 >= j2))))
        |> inRight)
    |> inRight
         
  | ExpCond([], e) ->
    j |> interp0 env e

  | ExpCond((e1,e2)::l, e) ->
    j
    |> interp0 env e1
    |> of_choice
    |> map_to_json (function
          `Bool false ->
          j
          |> interp0 env (ExpCond(l, e))
        | _ ->
          j
          |> interp0 env e2
      )
    |> inRight

  | ExpTryCatch (e1, e2) -> begin
      try
        j
        |> interp0 env e1
        |> of_choice
        |> to_list
        |> of_list
        |> inRight
      with JQException msg ->
        (`String msg) |> C.from_json
        |> interp0 env e2
    end

  | e -> Fmt.(failwithf "interp0: unrecognized exp %a" pp_exp e)

and binop env f e1 e2 j =
  j
  |> interp0 env e1
  |> of_choice
  |> map_to_json (fun j1 ->
      j
      |> interp0 env e2
      |> of_choice
      |> map_to_json (fun j2 ->
          let jr = C.from_json (f (j1, j2)) in
          jr |> inLeft)
      |> inRight)
  |> inRight

and interp0_funcdef (fenv, denv, benv) (fname, formals, body) =
  let fcode actuals j =
    if List.length formals <> List.length actuals then
      Fmt.(failwithf "function %a: formal-actual length mismatch" Dump.string fname) ;
    let newenv = List.map2 (fun f a ->
        ((f,0), fun [] -> a)) formals actuals in
    j |> interp0 ((newenv@fenv), denv, benv) body in
  ((((fname, List.length formals), fcode)::fenv), denv, benv)

and interp0_funcdefs env l =
  List.fold_left interp0_funcdef env l

let interp ?(fenv=[]) e j =
  let fenv = if fenv = [] then !predefined_functions else fenv in
  interp0 (fenv, [], []) e j

let add_function fname code =
  predefined_functions := (fname, code):: !predefined_functions

let interp_tuple l j : (t, t ll_t) choice =
  let rec edrec l (j : t) = match l with
      [] ->
      j |> (fun j -> inLeft (C.from_json (`List [])))

    | (f::l) ->
      j
      |> f |> of_choice |> map_to_json (fun v ->
          j
          |> edrec l
          |> of_choice
          |> map_to_json (fun (`List l) ->
              Left (C.from_json (`List (v::l))))
          |> inRight
        )
      |> inRight
  in
  j
  |> edrec l

let exec0 ?(fenv=[]) e l =
  l
  |> List.map C.from_json
  |> of_list
  |> map (interp ~fenv e)

let exec ?(fenv=[]) e l =
  l
  |> exec0 ~fenv e
  |> to_list

end

module JsonCarrier : (CARRIER with type t = Yojson.Basic.t) = struct
  type t = Yojson.Basic.t
  let to_json x = x
  let from_json x = x
  let object_field = Jqutil.object_field
  let array_list = Jqutil.array_list
  let array_deref = Jqutil.array_deref
end

type json_path_t = Yojson.Basic.t * Yojson.Basic.t list option
module PathCarrier : (CARRIER with type t = json_path_t) = struct
  type t = json_path_t
  let to_json x = fst x
  let from_json = function
      (`Assoc _ | `List _|`Null) as x -> (x, Some [])
    | x -> (x, None)

  let add_int n = function
      None -> None
    | Some p -> Some ((`Int n)::p)

  let add_string n = function
      None -> None
    | Some p -> Some ((`String n)::p)

let object_field fname : t -> t = function
    (`Assoc l, p) -> begin match List.assoc fname l with
      v -> (v, add_string fname p)
      | exception Not_found -> (`Null, add_string fname p)
    end

  | (`Null, p) -> (`Null, add_string fname p)
  | _ -> jqexception "object_field: not an object"

let array_deref n : t -> t = function
    (`List l, p) ->
    let alen = List.length l in
    let n = if n < 0 then alen + n else n in
    if n < 0 || n >= alen then (`Null, add_int n p)
    else (List.nth l n, add_int n p)

  | (`Null, p) -> (`Null, add_int n p)

  | _ -> jqexception "array_deref: not an array"

let array_list : t -> t ll_t = function
    (`List l, p) -> of_list (List.mapi (fun i v -> (v,add_int i p)) l)
  | (`Assoc l, p) -> of_list (List.mapi (fun i (k,v) -> (v, add_string k p)) l)
  | _ -> jqexception "array_list: not an array or object"

end

module IPJ = Gen(PathCarrier)
module IJ = Gen(JsonCarrier)

module I = IPJ

I.add_function ("length",0)
  (function [] ->  function j -> match I.C.to_json j with
        `String s -> Left (I.C.from_json (`Int(utf8_length s)))
      | `List l -> Left (I.C.from_json (`Int(List.length l)))
      | `Assoc l -> Left (I.C.from_json (`Int(List.length l)))
      | `Null -> Left (I.C.from_json (`Int 0))
  )
;;

I.add_function ("utf8bytelength",0)
  (function [] -> function j -> match I.C.to_json j with
        `String s -> Left (I.C.from_json (`Int(String.length s)))
  )
;;

I.add_function ("keys",0)
  (function [] -> function j -> match I.C.to_json j with
        `Assoc l -> Left (I.C.from_json (`List(List.sort Stdlib.compare (List.map (fun (k,_) -> `String k) l))))
      | `List l -> Left (I.C.from_json (`List(List.mapi (fun i _ -> `Int i) l)))
  )
;;

I.add_function ("keys_unsorted",0)
  (function [] -> function j -> match I.C.to_json j with
        `Assoc l -> Left (I.C.from_json (`List(List.map (fun (k,_) -> `String k) l)))
      | `List l -> Left (I.C.from_json (`List(List.mapi (fun i _ -> `Int i) l)))
  )
;;

I.add_function ("has",1)
  (function
      [f0] ->
      (function j -> match I.C.to_json j with
          (`Assoc l) ->
          j
          |> f0
          |> of_choice
          |> I.map_to_json (function (`String k) ->
              Left(I.C.from_json (`Bool (List.mem_assoc k l))))
          |> inRight
        | (`List l) ->
          j
          |> f0
          |> of_choice
          |> I.map_to_json (function (`Int n) ->
              Left (I.C.from_json (`Bool (n >= 0 && n < List.length l))))
          |> inRight
      )
  )
;;

I.add_function ("in",1)
  (function
      [f0] ->
      (function j -> match I.C.to_json j with
          (`String k) ->
          j
          |> f0
          |> of_choice
          |> I.map_to_json (function (`Assoc l) ->
              Left(I.C.from_json (`Bool (List.mem_assoc k l))))
          |> inRight
        | (`Int n) ->
          j
          |> f0
          |> of_choice
          |> I.map_to_json (function (`List l) ->
              Left (I.C.from_json (`Bool (n >= 0 && n < List.length l))))
          |> inRight
      )
  )
;;
(*
I.add_function ("select",1)
  (function
      [f0] ->
      (function j ->
         j
         |> f0
         |> of_choice
         |> I.map_to_json (function `Bool false -> Right(of_list []) | _ -> Left j)
         |> inRight))
;;
*)

I.add_function ("error",0)
  (function
      [] ->
      (function j ->
         j
         |> I.C.to_json
         |> (function (`String msg) -> jqexception msg)
         |> inRight
      )
  )
;;

I.add_function ("getpath",1)
  (function
      [f0] ->
      (function j ->
          j
          |> f0
          |> of_choice
          |> I.map_to_json (fun jp ->
              let rec traverse = function
                  (j, []) -> j
                | (`List l, (`Int n)::tl) ->
                  if 0 <= n && n < List.length l then
                    traverse (List.nth l n, tl)
                  else `Null
                | (`Assoc l, (`String k)::tl) -> begin match List.assoc k l with
                      v -> traverse (v, tl)
                    | exception Not_found -> `Null
                  end
                  | (`Null, _) -> `Null
                  | _ -> jqexception "getpath: can only index array & object"
              in match jp with
              `List l -> Left (I.C.from_json (traverse (I.C.to_json j, l)))
              | _ -> jqexception "getpath: path must be specified as an array"
            )
          |> inRight
      )
  )
;;

let set_nth l n newv =
  let rec setrec = function
      ([], n) when n > 0 -> `Null::(setrec ([], n-1))
    | ([], 0) -> []
    | (h::t, n) when n > 0 -> h::(setrec (t, n))
    | (h::t, 0) -> newv::t
  in if n < 0 then jqexception "setpath: numeric index must be non-negative"
  else setrec (l,n)

let get_nth l n =
  if n < 0 then jqexception "setpath: numeric index must be non-negative"
  else List.nth l n

let remove_nth l n =
  if n < 0 then jqexception "delpaths: numeric index must be non-negative"
  else
    let rec rrec = function
        (h::t, 0) -> t
      | (h::t, n) -> h::(rrec (t,n-1))
      | ([], _) -> []
    in rrec (l, n)

let set_key l k newv =
  let l = if List.mem_assoc k l then
      List.remove_assoc k l
    else l in
  sort_object_keys ((k,newv)::l)

let get_key l k =
  match List.assoc k l with
    v -> v
  | exception Not_found -> `Null
;;
I.add_function ("setpath",2)
  (function
      [f0;f1] ->
      (function j ->
          j
          |> f0
          |> of_choice
          |> I.map_to_json (fun jp ->
              let pl = match jp with
                  `List l -> l
                | _ -> jqexception "setpath: first arg must be list of string/int" in
              j
              |> f1
              |> of_choice
              |> I.map_to_json (fun jv ->
                  let rec update = function
                      (j, []) -> jv
                    | (`List l, (`Int n)::tl) ->
                      let newv = update (get_nth l n, tl) in
                      `List(set_nth l n newv)

                    | (`Null, (`Int n::tl)) ->
                       let newv = update (`Null, tl) in
                       `List(set_nth [] n newv)

                    | (`Assoc l, (`String k::tl)) ->
                       let newv = update (get_key l k, tl) in
                       `Assoc (set_key l k newv)

                    | (`Null, (`String k::tl)) ->
                       let newv = update (`Null, tl) in
                       `Assoc (set_key [] k newv)
                    | _ -> jqexception "setpath: first arg must be list/array/null, second must be array of string/int"
                  in Left (I.C.from_json (update (I.C.to_json j, pl)))
                )
              |> inRight
            )
          |> inRight
      )
  )
;;

I.add_function ("delpaths",1)
  (function
      [f0] ->
      (function j ->
          j
          |> f0
          |> of_choice
          |> I.map_to_json (fun jp ->
              let pl = match jp with
                  `List l -> l
                | _ -> jqexception "delpaths: first arg must be list of path (path = list of string/int)" in
              let rec update1 = function
                  (j, []) -> `Null

                | (`List l, (`Int n)::[]) -> `List(remove_nth l n)
                      
                | (`List l, (`Int n)::tl) ->
                  if n < 0 then jqexception "delpaths: numeric indexes must be non-negative"
                  else if n >= List.length l then `List l
                  else
                    let newv = update1 (get_nth l n, tl) in
                    `List(set_nth l n newv)

                | (`Null, (`Int n::tl)) -> `Null
                  
                | (`Assoc [], (`String k::tl)) -> `Assoc []
                                                    
                | (`Assoc l, (`String k::tl)) -> 
                  if List.mem_assoc k l then
                    `Assoc (List.remove_assoc k l)
                  else
                    let newv = update1 (get_key l k, tl) in
                    `Assoc (set_key l k newv)
                      
                | (`Null, (`String k::tl)) -> `Null
                  
                | _ -> jqexception "delpaths: first arg must be list/array/null, second must be array of array of string/int"
              in
              let rec update j = function
                  `List l -> update1 (j,l)
                | _ -> jqexception "delpaths: first arg must be list/array/null, second must be array of array of string/int"
              in
              Left (I.C.from_json (List.fold_left update (I.C.to_json j) pl))
            )
          |> inRight
      )
  )
;;

I.add_function ("path",1)
  (function
      [f0] ->
      (function j ->
          j
          |> f0
          |> of_choice
          |> map (function
                (_, Some pl) -> Left (`List (List.rev pl), None)
              | (_, None) -> jqexception "path: invalid path-expression"
              | _ -> Right nil
            )
          |> inRight
      )
  )
;;

I.add_function ("halt_error",1)
  (function
      [f0] ->
      (function j ->
          j
          |> f0
          |> of_choice
          |> I.map_to_json (function `Int n ->
              Stdlib.exit n)
          |> inRight
      )
  )
;;

I.add_function ("halt",0)
  (function
      [] ->
      (function j ->
         Stdlib.exit 0
      )
  )
;;

I.add_function ("_sort_by_impl",1)
  (function
      [f0] ->
      (function j ->
         j
         |> I.C.to_json
         |>  (function (`List vl) as vj ->
             j
             |> f0
             |> of_choice
             |> I.map_to_json (function (`List kl) as kj ->
                 if List.length vl <> List.length kl then
                   jqexception "_sort_by_impl: Internal error: value and key lists of differing length" ;
                 let l = List.map2 (fun k v -> (k,v)) kl vl in
                 let l = List.sort (fun (k1, _) (k2,_) -> Stdlib.compare k1 k2) l in
                 let l = List.map snd l in
                 Left(I.C.from_json (`List l))
               )
           )
         |>
         inRight
      )
  )
;;
