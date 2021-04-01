open Asttools
open Yojson.Basic

open Jqtypes

open Lazy_reclist

let object_field fname : Yojson.Basic.t -> Yojson.Basic.t = function
    `Assoc l -> begin match List.assoc fname l with
      v -> v
      | exception Not_found -> raise (JQException Fmt.(str "object_field: field %s not found" fname))
    end
  | _ -> raise (JQException "object_field: not an object")

let array_deref n : Yojson.Basic.t -> Yojson.Basic.t = function
    `List l -> begin match List.nth l n with
        v -> v
      | exception Failure _ -> raise (JQException Fmt.(str "array_deref: element %d not found (array too short)" n))
    end
  | _ -> raise (JQException "array_deref: not an array")

let array_list : Yojson.Basic.t -> Yojson.Basic.t ll_t = function
    `List l -> of_list l
  | _ -> raise (JQException Fmt.(str "array_list: not an array"))

let gather_to_list
    (f : Yojson.Basic.t -> Yojson.Basic.t ll_t)
    (j : Yojson.Basic.t) : Yojson.Basic.t list =
  j |> f |> to_list
;;

let gather_to_array f ll : Yojson.Basic.t =
  ll |> gather_to_list f |> (fun l -> `List l)

let map (f : t -> (t, t ll_t) choice) ll : t ll_t =
  Lazy_reclist.map f ll
