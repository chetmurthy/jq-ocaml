open Asttools ;
open Pa_ppx_base.Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;
open Jqtypes ;
open Jqlexing ;

type t += [
    Exc of Ploc.t and t[@rebind_to Ploc.Exc;][@name "Ploc.Exc";]
  | JQException of string[@rebind_to Jqtypes.JQException;][@name "Jqtypes.JQException";]
  | JQBreak of string[@rebind_to Jqtypes.JQBreak;][@name "Jqtypes.JQBreak";]

] [@@deriving show;]
;

value print_exn exn = Some (show exn) ;
Printexc.register_printer print_exn ;

value positions_to_loc ?{comments=""} (spos, epos) =
  let open Lexing in
  Ploc.make_loc spos.pos_fname spos.pos_lnum spos.pos_bol (spos.pos_cnum, epos.pos_cnum) comments
;

value convert_token (tok, pos) =
  let pos = positions_to_loc pos in
  let tok = match tok with [
    Spcl s -> ("",s)
  | Keyw s -> ("",s)
  | Ident s -> ("IDENT",s)
  | Integer n -> ("INT",n)
  | Float n -> ("FLOAT",n)
  | String s -> ("STRING", Unescape.jsonstring s)
  | EOF -> ("EOI","")
  ]
  in
  (tok, pos)
;

value lex_string s =
  let lexbuf = Sedlexing.Latin1.from_gen (gen_of_string s) in
  let rec lexrec acc =
    match convert_token (rawtoken lexbuf) with [
      (("EOI",_),_) as t -> List.rev [t::acc]
    | t -> lexrec [t::acc]
    ] in lexrec []
;

value input_file = Plexing.input_file ;
value lexer_func_of_sedlex_located lexfun cs =
  let read1 () =
    try Some (Stream.next cs) with [ Stream.Failure -> None ] in
  let lexbuf = Sedlexing.Latin1.from_gen read1
  in
  let next_token_func () = convert_token (lexfun lexbuf) in do {
    Sedlexing.set_filename lexbuf input_file.val ;
    Plexing.make_stream_and_location next_token_func
  }
;


value lexer = lexer_func_of_sedlex_located rawtoken ;
value lexer = {Plexing.tok_func = lexer;
 Plexing.tok_using _ = (); Plexing.tok_removing _ = ();
 Plexing.tok_match = Plexing.default_match;
 Plexing.tok_text = Plexing.lexer_text;
 Plexing.tok_comm = None} ;

value g = Grammar.gcreate lexer;
value (exp : Grammar.Entry.e exp) = Grammar.Entry.create g "exp";
value (exp_eoi : Grammar.Entry.e exp) = Grammar.Entry.create g "exp_eoi";
value (funcdefs : Grammar.Entry.e (list (string * list string * exp))) = Grammar.Entry.create g "funcdefs";
value (funcdefs_eoi : Grammar.Entry.e (list (string * list string * exp))) = Grammar.Entry.create g "funcdefs_eoi";

EXTEND
  GLOBAL:
    exp exp_eoi
    funcdefs funcdefs_eoi
    ;

    dict_pair:
      [ [ id = IDENT ; ":" ; e = exp LEVEL "//" -> (ExpString id, e)
        | id = IDENT -> (ExpString id, ExpDotField id)
        | e1 = exp ; ":" ; e2 = exp LEVEL "//" -> (e1,e2)
      ] ]
    ;

    ident: [ [ id = IDENT -> id | id = UIDENT -> id ] ] ;
    funcdef: [ [
        "def" ; id=ident ; ":" ; e = exp ; ";" -> (id,[],e)
      | "def" ; id=ident ; "(" ; l = LIST1 ident SEP ";" ; ")" ; ":" ; e = exp ; ";" -> (id, l, e)
      ] ]
    ;
    funcdefs: [ [ l = LIST1 funcdef -> l ] ] ;
    exp: [
      "def" NONA [
        fd=funcdef ; e = exp -> ExpFuncDef fd e
      ]
    | "|" RIGHTA [
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
    | "as" [
        e = exp ; "as" ; "$" ; id = ident -> ExpDataBind e id
      ]
    | "reduce" [
        "reduce" ; e = exp LEVEL "." ; "as" ; "$" ; id = ident ;
        "(" ; e1 = exp ; ";" ; e2 = exp ; ")" -> ExpReduce e id e1 e2
      | "foreach" ; e = exp LEVEL "." ; "as" ; "$" ; id = ident ;
        "(" ; e1 = exp ; ";" ; e2 = exp ; ";" ; e3 = exp ; ")" -> ExpForeach e id e1 e2 e3
      | "foreach" ; e = exp LEVEL "." ; "as" ; "$" ; id = ident ;
        "(" ; e1 = exp ; ";" ; e2 = exp ; ")" -> ExpForeach e id e1 e2 ExpDot
      ]
    | "." LEFTA [
        e = exp ; "." ; f=ident ->
        match e with [
          ExpDot -> failwith "..fldname is a syntax error"
        | _ -> ExpField e f
        ]
        | e=exp ; "?" -> ExpTryCatch e ExpEmpty
        | e = exp ; "[" ; e1 = exp ; "]" -> ExpDeref e e1
        | e = exp ; "[" ; e1 = exp ; ":" ; e2 = exp ; "]" -> ExpSlice e (Some e1) (Some e2)

        | e = exp ; "[" ; e1 = exp ; ":]" -> ExpSlice e (Some e1) None
        | e = exp ; "[" ; e1 = exp ; ":" ; "]" -> ExpSlice e (Some e1) None

        | e = exp ; "[" ; ":" ; e2 = exp ; "]" -> ExpSlice e None (Some e2)

        | e = exp ; "[" ; "]" -> ExpBrackets e
        | e = exp ; "[:" ; e2 = exp ; "]" -> ExpSlice e None (Some e2)
        | e = exp ; "." ; "[" ; e2 = exp ; "]" -> ExpDeref e e2
      ]
    | "simple" [
        "." -> ExpDot
      | "." ; f=ident -> ExpDotField f
      | "." ; f=STRING -> ExpDotField f
      | ".." -> ExpRecurse
      | "$" ; id = ident -> ExpDataVar id
      | "empty" -> ExpEmpty
      | "break" ; "$" ; l=ident -> ExpBreak l
      | "label" ; "$" ; l=ident -> ExpLabel l
      | s = STRING -> ExpString s
      | "@" ; id = IDENT -> ExpFormat id
      | "(" ; e = exp ; ")" -> e
      | "[" ; e = exp ; "]" -> ExpCollect e
      | "[" ; "]" -> ExpArray
      | "{" ; l = LIST0 dict_pair SEP "," ; "}" -> ExpDict l
      | f=ident ; "(" ; l = LIST1 exp SEP ";" ; ")" -> ExpFuncall f l
      | f=ident -> ExpFuncall f []
      | n = INT -> ExpInt (int_of_string n)
      | "null" -> ExpNull
      | "true" -> ExpBool True
      | "false" -> ExpBool False
      | n = FLOAT -> ExpFloat (float_of_string n)
      | "if" ; e1 = exp ; "then" ; e2 = exp ;
        l = LIST0 [ "elif" ; e3 = exp ; "then"; e4 = exp  -> (e3, e4) ] ;
        "else" ; e = exp ; "end" -> ExpCond [(e1,e2)::l] e
      | "try" ; e1 = exp ; "catch" ; e2 = exp -> ExpTryCatch e1 e2
      | "try" ; e1 = exp -> ExpTryCatch e1 ExpEmpty
    ]
 ]
  ;
  exp_eoi : [ [ e = exp ; EOI -> e ] ] ;
  funcdefs_eoi : [ [ e = funcdefs ; EOI -> e ] ] ;
END;

value parse_exp = Grammar.Entry.parse exp ;
value parse_exp_eoi = Grammar.Entry.parse exp_eoi ;

value parse_funcdefs = Grammar.Entry.parse funcdefs ;
value parse_funcdefs_eoi = Grammar.Entry.parse funcdefs_eoi ;

value parse_string pf s = do {
  input_file.val := s ;
  pf (Stream.of_string s)
}
;

value parse_channel pf ic = do {
  input_file.val := "<channel-input>" ;
  pf (Stream.of_channel ic)
}
;

value parse_file pf fname = do {
  input_file.val := fname ;
  let ic = open_in fname in
  let rv = pf (Stream.of_channel ic) in 
  do { close_in ic ; rv }
}
;
