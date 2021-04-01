open Asttools ;
open Pa_ppx_base.Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;
open Jqtypes ;

type t += [
    Exc of Ploc.t and t[@rebind_to Ploc.Exc;][@name "Ploc.Exc";]
] [@@deriving show;]
;

value print_exn exn = Some (show exn) ;
Printexc.register_printer print_exn ;

value input_file = ref "" ;

value lexer = do {
  Plexer.dollar_for_antiquotation.val := False ;
  let rv = Plexer.gmake() in
  Plexer.dollar_for_antiquotation.val := True ;
  rv
} ;
value g = Grammar.gcreate lexer;
value (exp : Grammar.Entry.e exp) = Grammar.Entry.create g "exp";
value (exp_eoi : Grammar.Entry.e exp) = Grammar.Entry.create g "exp_eoi";

EXTEND
  GLOBAL:
    exp exp_eoi
    ;

    dict_pair:
      [ [ id = LIDENT ; ":" ; e = exp -> (ExpString id, e)
        | e1 = exp ; ":" ; e2 = exp -> (e1,e2)
      ] ]
    ;

    exp: [
      "|" LEFTA [
        e1 = exp ; "|" ; e2 = exp -> ExpSeq e1 e2
      ]
    | "," LEFTA [
        e1 = exp ; "," ; e2 = exp -> ExpConcat e1 e2
      ]
    | "//" RIGHTA [
        e1 = exp ; "//" ; e2 = exp -> ExpAlt e1 e2
      ]
    | "=" NONA [
        e1 = exp ; "=" ; e2 = exp -> ExpAssign e1 e2
      | e1 = exp ; "+=" ; e2 = exp -> ExpAssignAdd e1 e2
      | e1 = exp ; "-=" ; e2 = exp -> ExpAssignSub e1 e2
      | e1 = exp ; "*=" ; e2 = exp -> ExpAssignMul e1 e2
      | e1 = exp ; "/=" ; e2 = exp -> ExpAssignDiv e1 e2
      | e1 = exp ; "%=" ; e2 = exp -> ExpAssignMod e1 e2
      | e1 = exp ; "//=" ; e2 = exp -> ExpAssignDes e1 e2
      | e1 = exp ; "|=" ; e2 = exp -> ExpAssignUpd e1 e2
      ]
    | "or" NONA [
        e1 = exp ; "or" ; e2 = exp -> ExpAnd e1 e2
      ]
    | "and" NONA [
        e1 = exp ; "and" ; e2 = exp -> ExpAnd e1 e2
      ]
    | "==" NONA [
        e1 = exp ; "==" ; e2 = exp -> ExpEq e1 e2
      | e1 = exp ; "!=" ; e2 = exp -> ExpNe e1 e2
      | e1 = exp ; "<" ; e2 = exp -> ExpLt e1 e2
      | e1 = exp ; ">" ; e2 = exp -> ExpGt e1 e2
      | e1 = exp ; "<=" ; e2 = exp -> ExpLe e1 e2
      | e1 = exp ; ">=" ; e2 = exp -> ExpGe e1 e2
      ]
    | "+" LEFTA [
        e1 = exp ; "+" ; e2 = exp -> ExpAdd e1 e2
      | e1 = exp ; "-" ; e2 = exp -> ExpSub e1 e2
      ]
    | "*" LEFTA [
        e1 = exp ; "*" ; e2 = exp -> ExpMul e1 e2
      | e1 = exp ; "/" ; e2 = exp -> ExpDiv e1 e2
      | e1 = exp ; "%" ; e2 = exp -> ExpMod e1 e2
      ]
    | "-" [
        "-" ; e = exp -> ExpNeg e
      ]
    | "?//" NONA [
        e1 = exp ; "?//" ; e2 = exp -> ExpDesAlt e1 e2
      ]
    | "." LEFTA [
        e = exp ; "." ; f=LIDENT ->
        match e with [
          ExpDot -> failwith "..fldname is a syntax error"
        | _ -> ExpField e f
        ]
        | e=exp ; "?" -> ExpQuestion e
        | e = exp ; "[" ; e2 = exp ; "]" -> ExpDeref e e2
        | e = exp ; "[" ; "]" -> ExpBrackets e
        | e = exp ; "[" ; e1 = exp ; ":" ; e2 = OPT exp ; "]" -> ExpSlice e e1 e2
        | e = exp ; "." ; "[" ; e2 = exp ; "]" -> ExpDeref e e2
        | n = INT -> ExpInt (int_of_string n)
        | n = FLOAT -> ExpFloat (float_of_string n)
      ]
    | "simple" [
        "." -> ExpDot
      | "." ; f=LIDENT -> ExpDotField f
      | "." ; f=STRING -> ExpDotField f
      | ".." -> ExpDotDot
      | "$" ; id = LIDENT -> ExpDataVar id
      | "break" ; "$" ; l=LIDENT -> ExpBreak l
      | s = STRING -> ExpString s
      | "@" ; id = LIDENT -> ExpFormat id
      | "(" ; e = exp ; ")" -> e
      | "[" ; e = exp ; "]" -> ExpCollect e
      | "[" ; "]" -> ExpArray
      | "{" ; l = LIST0 dict_pair SEP "," ; "}" -> ExpDict l
    ] ]
  ;
  exp_eoi : [ [ e = exp ; EOI -> e ] ] ;
END;

value parse_exp = Grammar.Entry.parse exp ;
value parse_exp_eoi = Grammar.Entry.parse exp_eoi ;

value parse_string pf s =
  pf (Stream.of_string s)
;

value parse_channel pf ic =
  pf (Stream.of_channel ic)
;

value parse_file pf fname =
  let ic = open_in fname in
  let rv = pf (Stream.of_channel ic) in 
  do { close_in ic ; rv }
;
