type token =
  | COH
  | OBJ
  | PIPE
  | MOR
  | PATH
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | COL
  | IDENT of (string)
  | STRING of (string)
  | CHECK
  | EVAL
  | HYP
  | ENV
  | EQUAL
  | LET
  | IN
  | TEST
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Common
    open Command
    open Syntax

# 32 "parser.ml"
let yytransl_const = [|
  257 (* COH *);
  258 (* OBJ *);
  259 (* PIPE *);
  260 (* MOR *);
  261 (* PATH *);
  262 (* LPAR *);
  263 (* RPAR *);
  264 (* LBRA *);
  265 (* RBRA *);
  266 (* COL *);
  269 (* CHECK *);
  270 (* EVAL *);
  271 (* HYP *);
  272 (* ENV *);
  273 (* EQUAL *);
  274 (* LET *);
  275 (* IN *);
  276 (* TEST *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  267 (* IDENT *);
  268 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\006\000\006\000\007\000\007\000\008\000\008\000\
\009\000\009\000\005\000\005\000\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\005\000\006\000\004\000\007\000\005\000\002\000\
\006\000\000\000\002\000\000\000\003\000\001\000\003\000\001\000\
\001\000\002\000\006\000\001\000\006\000\001\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\002\000\024\000\
\000\000\000\000\000\000\000\000\000\000\008\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\
\000\000\000\000\000\000\022\000\000\000\014\000\000\000\005\000\
\000\000\020\000\000\000\000\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\000\000\007\000\
\000\000\000\000\015\000\000\000\004\000\013\000\000\000\011\000\
\000\000\009\000\000\000\000\000\000\000\006\000\023\000\000\000\
\000\000\021\000\019\000"

let yydgoto = "\002\000\
\008\000\009\000\012\000\027\000\032\000\045\000\033\000\028\000\
\034\000"

let yysindex = "\008\000\
\011\000\000\000\011\255\023\255\017\255\023\255\000\000\000\000\
\011\000\023\255\026\255\014\255\023\255\000\000\000\000\032\255\
\034\255\012\255\015\255\022\255\012\255\012\255\000\000\035\255\
\012\255\036\255\033\255\000\000\015\255\000\000\038\255\000\000\
\002\255\000\000\012\255\015\255\000\000\044\255\012\255\045\255\
\037\255\015\255\046\255\039\255\000\000\002\255\040\255\000\000\
\023\255\002\255\000\000\015\255\000\000\000\000\015\255\000\000\
\015\255\000\000\002\255\041\255\042\255\000\000\000\000\012\255\
\015\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\024\255\000\000\015\000\000\000\000\000\
\000\000\048\255\000\000\000\000\024\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\046\000\000\000\250\255\237\255\239\255\013\000\233\255\000\000\
\000\000"

let yytablesize = 291
let yytable = "\014\000\
\012\000\037\000\038\000\016\000\010\000\040\000\020\000\029\000\
\001\000\046\000\007\000\043\000\030\000\023\000\010\000\047\000\
\024\000\025\000\048\000\050\000\029\000\010\000\046\000\018\000\
\053\000\030\000\059\000\013\000\011\000\026\000\019\000\035\000\
\031\000\010\000\060\000\063\000\017\000\061\000\036\000\062\000\
\010\000\021\000\058\000\022\000\066\000\039\000\041\000\067\000\
\044\000\042\000\049\000\051\000\054\000\052\000\015\000\055\000\
\057\000\010\000\056\000\064\000\065\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\000\000\000\000\000\000\010\000\000\000\012\000\
\000\000\000\000\000\000\003\000\000\000\012\000\010\000\010\000\
\000\000\010\000\012\000\012\000\012\000\010\000\010\000\004\000\
\010\000\000\000\000\000\010\000\005\000\000\000\006\000\000\000\
\010\000\000\000\010\000"

let yycheck = "\006\000\
\000\000\021\000\022\000\010\000\000\000\025\000\013\000\006\001\
\001\000\033\000\000\000\029\000\011\001\002\001\000\000\035\000\
\005\001\006\001\036\000\039\000\006\001\011\001\046\000\010\001\
\042\000\011\001\050\000\011\001\006\001\018\001\017\001\010\001\
\018\001\010\001\052\000\059\000\011\001\055\000\017\001\057\000\
\017\001\010\001\049\000\010\001\064\000\011\001\011\001\065\000\
\011\001\017\001\007\001\007\001\007\001\017\001\009\000\017\001\
\017\001\010\001\046\000\019\001\019\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\255\255\255\255\001\001\255\255\007\001\
\255\255\255\255\255\255\001\001\255\255\013\001\010\001\001\001\
\255\255\013\001\018\001\019\001\020\001\017\001\018\001\013\001\
\020\001\255\255\255\255\013\001\018\001\255\255\020\001\255\255\
\018\001\255\255\020\001"

let yynames_const = "\
  COH\000\
  OBJ\000\
  PIPE\000\
  MOR\000\
  PATH\000\
  LPAR\000\
  RPAR\000\
  LBRA\000\
  RBRA\000\
  COL\000\
  CHECK\000\
  EVAL\000\
  HYP\000\
  ENV\000\
  EQUAL\000\
  LET\000\
  IN\000\
  TEST\000\
  EOF\000\
  "

let yynames_block = "\
  IDENT\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Command.prog) in
    Obj.repr(
# 23 "parser.mly"
               ( _1::_2 )
# 225 "parser.ml"
               : Command.prog))
; (fun __caml_parser_env ->
    Obj.repr(
# 24 "parser.mly"
          ( [] )
# 231 "parser.ml"
               : Command.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'tyexpr) in
    Obj.repr(
# 27 "parser.mly"
                                ( Coh (make_var _2,_3,_5) )
# 240 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'tyexpr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'tmexpr) in
    Obj.repr(
# 28 "parser.mly"
                                         ( Check (_2,_6, Some _4) )
# 249 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'tmexpr) in
    Obj.repr(
# 29 "parser.mly"
                              ( Check (_2,_4,None) )
# 257 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'args) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'tyexpr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'tmexpr) in
    Obj.repr(
# 30 "parser.mly"
                                             ( Decl (make_var _2,_3,_7,Some _5) )
# 267 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'tmexpr) in
    Obj.repr(
# 31 "parser.mly"
                                  ( Decl (make_var _2,_3,_5, None) )
# 276 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 32 "parser.mly"
                ( Ctx_test _2 )
# 283 "parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'tyexpr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 35 "parser.mly"
                                      ( (make_var _2, _4)::_6 )
# 292 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
      ( [] )
# 298 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_tmexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sub) in
    Obj.repr(
# 39 "parser.mly"
                        ( _1::_2 )
# 306 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
      ( [] )
# 312 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tmexpr) in
    Obj.repr(
# 43 "parser.mly"
                       ( _2 )
# 319 "parser.ml"
               : 'simple_tmexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
            ( Var (make_var _1) )
# 326 "parser.ml"
               : 'simple_tmexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tyexpr) in
    Obj.repr(
# 47 "parser.mly"
                       ( _2 )
# 333 "parser.ml"
               : 'simple_tyexpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
          ( Obj )
# 339 "parser.ml"
               : 'simple_tyexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_tmexpr) in
    Obj.repr(
# 51 "parser.mly"
                    ( _1 )
# 346 "parser.ml"
               : 'subst_tmexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'simple_tmexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sub) in
    Obj.repr(
# 52 "parser.mly"
                        ( Sub (_1,_2) )
# 354 "parser.ml"
               : 'subst_tmexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'tmexpr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'tmexpr) in
    Obj.repr(
# 55 "parser.mly"
                                       ( Letin_tm (make_var _2, _4, _6) )
# 363 "parser.ml"
               : 'tmexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'subst_tmexpr) in
    Obj.repr(
# 56 "parser.mly"
                   ( _1 )
# 370 "parser.ml"
               : 'tmexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'tmexpr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'tyexpr) in
    Obj.repr(
# 59 "parser.mly"
                                       ( Letin_ty (make_var _2, _4, _6) )
# 379 "parser.ml"
               : 'tyexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_tyexpr) in
    Obj.repr(
# 60 "parser.mly"
                    ( _1 )
# 386 "parser.ml"
               : 'tyexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'tyexpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'simple_tmexpr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'simple_tmexpr) in
    Obj.repr(
# 61 "parser.mly"
                                                    ( Path (make_var _2, _3, _4, _5) )
# 396 "parser.ml"
               : 'tyexpr))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Command.prog)
