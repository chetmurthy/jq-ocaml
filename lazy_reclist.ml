open Asttools
open Jqtypes

module LazyList = struct
type 'a ll_elem_t =
    IT of 'a
  | LL of 'a ll_t

and 'a ll_node_t =
    NIL
  | CONS of 'a ll_elem_t * 'a ll_t

and 'a ll_t = 'a ll_node_t lazy_t

let nil = lazy NIL

let cons_it v ll =
  lazy (CONS (IT v, ll))

let cons_ll ll1 ll2 =
  lazy (CONS (LL ll1, ll2))

let match_ll ll =
  let rec mrec ll =
    match ll with
      lazy NIL -> None
    | lazy (CONS (IT v, ll)) -> Some (v,ll)
    | lazy (CONS (LL (lazy NIL), ll)) -> mrec ll
    | lazy (CONS (LL (lazy (CONS (IT v, ll1))), ll2)) -> Some(v, cons_ll ll1 ll2)
    | lazy (CONS (LL (lazy (CONS (LL ll1, ll2))), ll3)) ->
      mrec (cons_ll ll1 (cons_ll ll2 ll3))

  in mrec ll

let map f ll =
  let rec maprec ll =
    match match_ll ll with
      None -> nil
    | Some (v, ll) ->
      match f v with
        Left v -> cons_it v (maprec ll)
      | Right ll' -> cons_ll ll' (maprec ll)
  in maprec ll

let of_choice = function
    Left v -> cons_it v nil
  | Right ll -> ll

let of_list l =
  let rec orec = function
      [] -> nil
    | h::t -> cons_it h (orec t)
  in orec l

let to_list ll =
  let rec orec ll =
    match match_ll ll with
      None -> []
    | Some(v,ll) -> v::(orec ll)
  in orec ll

let singleton v = cons_it v nil

let last ll =
  match List.rev (to_list ll) with
    [] -> failwith "LazyList.last: list was empty -- must be nonempty"
  | h::_ -> h


  let reduce (f : 'a -> 'a -> 'a ll_t) (jinit : 'a) (ll : 'a ll_t) : 'a ll_t =
    let rec rrec (jv, ll) =
      match match_ll ll with
        None -> singleton jv
      | Some (j, ll) -> begin
          match last (f jv j) with
            newjv -> rrec (newjv, ll)
        end
    in rrec (jinit, ll)

  let foreach (f : 'a -> 'a -> 'a ll_t) update (jinit : 'a) (ll : 'a ll_t) =
    let rec frec (jv, ll) =
      match match_ll ll with
        None -> nil
      | Some(j, ll) -> begin match last (f jv j) with
            newjv -> begin match  update newjv j with
               updv ->
               cons_ll updv (lazy (Lazy.force (frec (newjv, ll))))
            end
        end
    in lazy (Lazy.force (frec (jinit, ll)))
end

module EagerList = struct
  type 'a ll_t = 'a list

  let nil = []
  let cons_it v l = v::l
  let cons_ll ll1 ll2 = ll1@ll2
  let match_ll = function
      [] -> None
    | h::t -> Some(h,t)
  let map f ll =
    List.fold_right (fun v acc ->
        match f v with
          Left v -> v::acc
        | Right ll -> ll@acc)
      ll []
  let of_choice = function
      Left v -> [v]
    | Right ll -> ll
  let of_list l = l
  let to_list l = l
  let singleton v = [v]

  let last ll =
    match List.rev ll with
      [] -> failwith "EagerList.last: list was empty -- must be nonempty"
    | h::_ -> h

  let reduce (f : 'a -> 'a -> 'a ll_t) (jinit : 'a) (ll : 'a ll_t) : 'a ll_t =
    let rec rrec (jv, ll) =
      match match_ll ll with
        None -> singleton jv
      | Some (j, ll) -> begin
          match last (f jv j) with
            newjv -> rrec (newjv, ll)
        end
    in rrec (jinit, ll)

  let foreach (f : 'a -> 'a -> 'a ll_t) update (jinit : 'a) (ll : 'a ll_t) =
    let rec frec (jv, ll) =
      match match_ll ll with
        None -> nil
      | Some(j, ll) -> begin match last (f jv j) with
            newjv -> begin match  update newjv j with
               updv ->
               cons_ll updv (frec (newjv, ll))
            end
        end
    in (frec (jinit, ll))

end
(*
include EagerList
*)
include LazyList

