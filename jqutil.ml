open Asttools
open Yojson.Basic

open Jqtypes

open Lazy_reclist

(* borrowed from ounit *)
let failwithf fmt =
  Fmt.kstrf failwith fmt

let jqexception s = raise (JQException s)
let jqexceptionf fmt = Fmt.kstrf jqexception fmt
let jqbreak s = raise (JQBreak s)


(* shadow "failwith" in the Fmt module, so that the form

  Fmt.(failwith ....)

will be a type error (since it almost certainly should have been
   "failwithf"

*)
module Fmt = struct
  include Fmt
  let failwith = ()
end


let sort_object_keys l1 =
  List.sort (fun (a,_) (b,_) -> Stdlib.compare a b) l1

let canon_json j =
  let rec crec = function
      `List l -> `List (List.map crec l)
    | `Assoc l -> `Assoc (sort_object_keys (List.map (fun (k,v) -> (k, crec v)) l))
    | v -> v
  in crec j

let object_field fname : Yojson.Basic.t -> Yojson.Basic.t = function
    `Assoc l -> begin match List.assoc fname l with
      v -> v
      | exception Not_found -> `Null
    end

  | `Null -> `Null
  | _ -> jqexception "object_field: not an object"

let array_deref n : Yojson.Basic.t -> Yojson.Basic.t = function
    `List l ->
    let alen = List.length l in
    let n = if n < 0 then alen + n else n in
    if n < 0 || n >= alen then `Null
    else List.nth l n

  | `Null -> `Null

  | _ -> jqexception "array_deref: not an array"

let array_list : Yojson.Basic.t -> Yojson.Basic.t ll_t = function
    `List l -> of_list l
  | `Assoc l -> of_list (List.map snd l)
  | _ -> jqexception "array_list: not an array or object"

let gather_to_list
    (f : Yojson.Basic.t -> Yojson.Basic.t ll_t)
    (j : Yojson.Basic.t) : Yojson.Basic.t list =
  j |> f |> to_list
;;

let gather_to_array f ll : Yojson.Basic.t =
  ll |> gather_to_list f |> (fun l -> `List l)

let firstn n l =
  let rec aux acc = function
      (0, l) -> List.rev acc
    | (n, (h::t)) -> aux (h::acc) (pred n, t)
    | _ -> failwith "firstn"
  in aux [] (n,l)

let rec nthtail l = function
	0 -> l
  | n -> nthtail (List.tl l) (n-1)

let slice n m (l : t list) =
  let alen = List.length l in
  let n = match n with
      None -> 0
    | Some n ->
      if n < 0 then
        if -n > alen then 0 else alen - n
      else
        if n > alen then alen else n in
  let m = match m with
      None -> alen
    | Some m ->
      if m < 0 then
        if -m > alen then alen else alen - m
      else
      if m > alen then alen else m in
  let l = nthtail l n in
  firstn (m-n) l

let array_sub l1 l2 =
  List.filter (fun x -> not (List.mem x l2)) l1

let string_mul s n =
  let rec mrec acc = function
    0 -> acc
    | n -> mrec (s::acc) (n-1)
  in if n <= 0 then `Null
  else `String(String.concat "" (mrec [] n))

let object_mul l1 l2 =
  let rec mrec = function
      ([], []) -> []
    | (l, []) -> l
    | ([], l) -> l
    | (((h1, v1)::t1 as l1), ((h2, v2)::t2 as l2)) ->
      if h1 = h2 then
        match (v1,v2) with
          (`Assoc v1, `Assoc v2) ->
          (h1,`Assoc (mrec (v1, v2)))::(mrec (t1,t2))
        | _ -> (h2,v2)::(mrec (t1, t2))
      else if h1 < h2 then
        (h1, v1)::(mrec (t1, l2))
      else
        (h2, v2)::(mrec (l1, t2))
  in mrec (l1,l2)

let utf8_length s =
  let open Uutf in
  String.fold_utf_8 (fun n _ -> function
        `Uchar _ -> n+1
      | `Malformed _ -> jqexception "length: malformed UTF8 string")
    0 s
