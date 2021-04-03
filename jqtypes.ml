
exception JQException of string
exception JQBreak of string

type exp =
    ExpDot
  | ExpRecurse
  | ExpDotField of string
  | ExpDataVar of string
  | ExpBreak of string
  | ExpField of exp * string
  | ExpString of string
  | ExpInt of int
  | ExpNull
  | ExpBool of bool
  | ExpFloat of float
  | ExpDeref of exp * exp
  | ExpSlice of exp * exp option * exp option
  | ExpBrackets of exp
  | ExpDict of (exp * exp) list
  | ExpArray
  | ExpCollect of exp
  | ExpFormat of string
  | ExpSeq of exp * exp
  | ExpConcat of exp * exp
  | ExpAlt of exp * exp
  | ExpAssign of exp * exp
  | ExpAssignOr of exp * exp
  | ExpAssignAdd of exp * exp
  | ExpAssignSub of exp * exp
  | ExpAssignMul of exp * exp
  | ExpAssignDiv of exp * exp
  | ExpAssignUpd of exp * exp
  | ExpAssignMod of exp * exp
  | ExpAssignDes of exp * exp
  | ExpAnd of exp * exp
  | ExpOr of exp * exp
  | ExpEq of exp * exp
  | ExpNe of exp * exp
  | ExpGt of exp * exp
  | ExpLt of exp * exp
  | ExpGe of exp * exp
  | ExpLe of exp * exp
  | ExpAdd of exp * exp
  | ExpSub of exp * exp
  | ExpMul of exp * exp
  | ExpDiv of exp * exp
  | ExpMod of exp * exp
  | ExpNeg of exp
  | ExpDesAlt of exp * exp
  | ExpFuncall of string * exp list
  | ExpFuncDef of funcdef * exp
  | ExpDataBind of exp * string
  | ExpReduce of exp * string * exp * exp
  | ExpForeach of exp * string * exp * exp * exp
  | ExpLabel of string
  | ExpCond of (exp * exp) list * exp
  | ExpEmpty
  | ExpTryCatch of exp * exp

and funcdef = string * string list * exp
 [@@deriving show { with_path = false },eq]
