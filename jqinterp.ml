open Asttools ;;

open Lazy_reclist ;;
open Jqutil ;;
open Jqtypes

let rec interp e j =
  match e with
    ExpDot -> j |> map (function v -> Left v)
