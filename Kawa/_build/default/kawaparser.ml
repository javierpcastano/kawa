
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | WHILE
    | VOID
    | VAR
    | T_INT
    | TRUE
    | THIS
    | SUB
    | SEMI
    | RPAR
    | RETURN
    | REM
    | PRINT
    | OR
    | NOT
    | NEW
    | NEQS
    | NEQ
    | MUL
    | METHOD
    | MAIN
    | LT
    | LPAR
    | LE
    | INT of (
# 8 "kawaparser.mly"
       (int)
# 37 "kawaparser.ml"
  )
    | IF
    | IDENT of (
# 9 "kawaparser.mly"
       (string)
# 43 "kawaparser.ml"
  )
    | GT
    | GE
    | FALSE
    | EXTENDS
    | EQS
    | EQ
    | EOF
    | END
    | ELSE
    | DOT
    | DIV
    | COMMA
    | CLASS
    | BOOL
    | BEGIN
    | ATTRIBUTE
    | AS
    | AND
    | ADD
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState146
  | MenhirState142
  | MenhirState137
  | MenhirState136
  | MenhirState134
  | MenhirState130
  | MenhirState122
  | MenhirState119
  | MenhirState113
  | MenhirState111
  | MenhirState108
  | MenhirState103
  | MenhirState99
  | MenhirState98
  | MenhirState89
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState58
  | MenhirState54
  | MenhirState52
  | MenhirState46
  | MenhirState45
  | MenhirState42
  | MenhirState41
  | MenhirState38
  | MenhirState36
  | MenhirState35
  | MenhirState31
  | MenhirState26
  | MenhirState23
  | MenhirState22
  | MenhirState18
  | MenhirState17
  | MenhirState12
  | MenhirState9
  | MenhirState1
  | MenhirState0

# 1 "kawaparser.mly"
  

  open Lexing
  open Kawa


# 133 "kawaparser.ml"

let rec _menhir_goto_class_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.class_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | MAIN ->
        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146

and _menhir_goto_list_method_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.method_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.method_def))), _, (xs : (Kawa.method_def list))) = _menhir_stack in
        let _v : (Kawa.method_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 162 "kawaparser.ml"
         in
        _menhir_goto_list_method_def_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (id : (
# 9 "kawaparser.mly"
       (string)
# 177 "kawaparser.ml"
            ))), (p : (
# 9 "kawaparser.mly"
       (string)
# 181 "kawaparser.ml"
            ))), _, (a : ((string * Kawa.typ) list))), _, (m : (Kawa.method_def list))) = _menhir_stack in
            let _v : (Kawa.class_def) = 
# 56 "kawaparser.mly"
    ( { class_name = id; attributes=a; methods=m; parent = Some p } )
# 186 "kawaparser.ml"
             in
            _menhir_goto_class_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (id : (
# 9 "kawaparser.mly"
       (string)
# 207 "kawaparser.ml"
            ))), _, (a : ((string * Kawa.typ) list))), _, (m : (Kawa.method_def list))) = _menhir_stack in
            let _v : (Kawa.class_def) = 
# 54 "kawaparser.mly"
    ( { class_name = id; attributes=a; methods=m; parent = None } )
# 212 "kawaparser.ml"
             in
            _menhir_goto_class_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.method_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 229 "kawaparser.ml"
     in
    _menhir_goto_list_method_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | T_INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Kawa.expr))), (id : (
# 9 "kawaparser.mly"
       (string)
# 268 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let le = 
# 232 "<standard.mly>"
    ( xs )
# 273 "kawaparser.ml"
             in
            
# 102 "kawaparser.mly"
                                                                          (MethCall(e, id, le))
# 278 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 9 "kawaparser.mly"
       (string)
# 299 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let e = 
# 232 "<standard.mly>"
    ( xs )
# 304 "kawaparser.ml"
             in
            
# 101 "kawaparser.mly"
                                                              (NewCstr(id, e))
# 309 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce8 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.mem_access) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (m : (Kawa.mem_access))) = _menhir_stack in
    let _v : (Kawa.expr) = 
# 96 "kawaparser.mly"
            (Get(m))
# 327 "kawaparser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | IF ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | PRINT ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | RETURN ->
        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | WHILE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | END ->
        _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState45 | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Kawa.expr list)) = _v in
        let _v : (Kawa.expr list) = 
# 144 "<standard.mly>"
    ( x )
# 381 "kawaparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Kawa.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
        let _v : (Kawa.expr list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 392 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | INT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | LPAR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | NEW ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | NOT ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | SUB ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | THIS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | RPAR ->
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
        | ADD | AND | AS | COMMA | DIV | DOT | EQ | EQS | GE | GT | LE | LT | MUL | NEQ | NEQS | OR | REM | RPAR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Kawa.expr))), (id : (
# 9 "kawaparser.mly"
       (string)
# 779 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.mem_access) = 
# 108 "kawaparser.mly"
                           (Field(e, id))
# 784 "kawaparser.ml"
             in
            _menhir_goto_memoire _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_goto_list_attr_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | METHOD ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | END ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Kawa.typ))), _, (xs : ((string * Kawa.typ) list))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 908 "kawaparser.ml"
         in
        _menhir_goto_list_attr_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | METHOD ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | END ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState137
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | FALSE ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                    | IDENT _v ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                    | IF ->
                        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                    | INT _v ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                    | LPAR ->
                        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                    | NEW ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                    | NOT ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                    | PRINT ->
                        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                    | RETURN ->
                        _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                    | SUB ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                    | THIS ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                    | TRUE ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                    | WHILE ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                    | END ->
                        _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))), _, (if_instr : (Kawa.seq))), _, (else_instr : (Kawa.seq))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 114 "kawaparser.mly"
                                                                                                             (If(e, if_instr, else_instr))
# 1014 "kawaparser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.instr))), _, (xs : (Kawa.seq))) = _menhir_stack in
        let _v : (Kawa.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1030 "kawaparser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))), _, (isntr : (Kawa.seq))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 115 "kawaparser.mly"
                                                                  (While (e, isntr))
# 1046 "kawaparser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (t : (Kawa.typ))), (id : (
# 9 "kawaparser.mly"
       (string)
# 1067 "kawaparser.ml"
            ))), _, (xs : ((string * Kawa.typ) list))), _, (loc : ((string * Kawa.typ) list))), _, (c : (Kawa.seq))) = _menhir_stack in
            let _v : (Kawa.method_def) = let p = 
# 232 "<standard.mly>"
    ( xs )
# 1072 "kawaparser.ml"
             in
            
# 81 "kawaparser.mly"
    ( { method_name = id; code = c; params=p; locals=loc; return = t } )
# 1077 "kawaparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | METHOD ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | END ->
                _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, (globals : ((string * Kawa.typ) list))), _, (classes : (Kawa.class_def list))), _, (main : (Kawa.seq))) = _menhir_stack in
                let _v : (Kawa.program) = 
# 50 "kawaparser.mly"
    ( {classes; globals; main} )
# 1115 "kawaparser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_1 : (Kawa.program)) = _v in
                Obj.magic _1
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.expr list) = 
# 142 "<standard.mly>"
    ( [] )
# 1141 "kawaparser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_memoire : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.mem_access) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState113 | MenhirState108 | MenhirState103 | MenhirState99 | MenhirState38 | MenhirState41 | MenhirState42 | MenhirState45 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState58 | MenhirState54 | MenhirState52 | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState142 | MenhirState36 | MenhirState98 | MenhirState122 | MenhirState119 | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
            | INT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
            | LPAR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | NEW ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | NOT ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | SUB ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | THIS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState113
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
        | ADD | AND | DIV | DOT | EQ | EQS | GE | GT | LE | LT | MUL | NEQ | NEQS | OR | REM | SEMI | SUB ->
            _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | NEQS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 99 "kawaparser.mly"
                          (e)
# 1242 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | EQ | EQS | GE | GT | LE | LT | NEQ | NEQS | OR | RPAR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 128 "kawaparser.mly"
      ( Sub )
# 1272 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1277 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | DIV | EQ | EQS | GE | GT | LE | LT | MUL | NEQ | NEQS | OR | REM | RPAR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 131 "kawaparser.mly"
      ( Rem )
# 1299 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1304 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 | MenhirState89 | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | INT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | LPAR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | NEW ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | NOT ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | SUB ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | THIS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | NEQS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1383 "kawaparser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | EQ | EQS | GE | GT | LE | LT | NEQ | NEQS | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 139 "kawaparser.mly"
     ( Or )
# 1417 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1422 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | DIV | EQ | EQS | GE | GT | LE | LT | MUL | NEQ | NEQS | OR | REM | RPAR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 129 "kawaparser.mly"
      ( Mul )
# 1444 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1449 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | DIV | EQ | EQS | GE | GT | LE | LT | MUL | NEQ | NEQS | OR | REM | RPAR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 130 "kawaparser.mly"
      ( Div )
# 1471 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1476 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | EQS | GE | GT | LE | LT | NEQ | NEQS | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 138 "kawaparser.mly"
      ( And )
# 1508 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1513 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | EQ | EQS | GE | GT | LE | LT | NEQ | NEQS | OR | RPAR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 127 "kawaparser.mly"
      ( Add )
# 1541 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1546 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 142 "kawaparser.mly"
       ( Neqs )
# 1582 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1587 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 133 "kawaparser.mly"
      ( Neq )
# 1623 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1628 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 134 "kawaparser.mly"
     ( Lt )
# 1664 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1669 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 135 "kawaparser.mly"
     ( Le )
# 1705 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1710 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 136 "kawaparser.mly"
     ( Gt )
# 1746 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1751 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 137 "kawaparser.mly"
     ( Ge )
# 1787 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1792 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 141 "kawaparser.mly"
      ( Eqs )
# 1828 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1833 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let op = 
# 132 "kawaparser.mly"
     ( Eq )
# 1869 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                       ( Binop(op, e1, e2) )
# 1874 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQ | EQS | GE | GT | LE | LT | NEQ | NEQS | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let up = 
# 122 "kawaparser.mly"
      ( Not )
# 1906 "kawaparser.ml"
             in
            
# 97 "kawaparser.mly"
                           (Unop(up , e))
# 1911 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | COMMA | EQ | EQS | GE | GT | LE | LT | NEQ | NEQS | OR | RPAR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let up = 
# 123 "kawaparser.mly"
      ( Opp )
# 1939 "kawaparser.ml"
             in
            
# 97 "kawaparser.mly"
                           (Unop(up , e))
# 1944 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | NEQS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FALSE ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | IDENT _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
                | IF ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | INT _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
                | LPAR ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | NEW ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | NOT ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | PRINT ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | RETURN ->
                    _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | SUB ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | THIS ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | END ->
                    _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState98
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | NEQS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 116 "kawaparser.mly"
                            (Return (e))
# 2087 "kawaparser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | NEQS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
                let _v : (Kawa.instr) = 
# 112 "kawaparser.mly"
                                    ( Print(e) )
# 2146 "kawaparser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | NEQS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FALSE ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | IDENT _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
                | IF ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | INT _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
                | LPAR ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | NEW ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | NOT ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | PRINT ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | RETURN ->
                    _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | SUB ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | THIS ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | END ->
                    _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | NEQS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (m : (Kawa.mem_access))), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 113 "kawaparser.mly"
                                 (Set(m,e))
# 2297 "kawaparser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 | MenhirState36 | MenhirState98 | MenhirState111 | MenhirState119 | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | NEQS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | REM ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 117 "kawaparser.mly"
                    (Expr(e))
# 2351 "kawaparser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_class_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.class_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MAIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FALSE ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | IDENT _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
                | IF ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | INT _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
                | LPAR ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | NEW ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | NOT ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | PRINT ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | RETURN ->
                    _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | SUB ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | THIS ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | END ->
                    _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState142
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.class_def))), _, (xs : (Kawa.class_def list))) = _menhir_stack in
        let _v : (Kawa.class_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2435 "kawaparser.ml"
         in
        _menhir_goto_list_class_def_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_param_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Kawa.typ) list)) = _v in
        let _v : ((string * Kawa.typ) list) = 
# 144 "<standard.mly>"
    ( x )
# 2451 "kawaparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_param__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Kawa.typ) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2462 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_param__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VAR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | END | FALSE | IDENT _ | IF | INT _ | LPAR | NEW | NOT | PRINT | RETURN | SUB | THIS | TRUE | WHILE ->
                _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Kawa.typ) list) = 
# 211 "<standard.mly>"
    ( [] )
# 2511 "kawaparser.ml"
     in
    _menhir_goto_list_attr_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | T_INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 2544 "kawaparser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | INT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | LPAR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | NEW ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | NOT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | THIS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 93 "kawaparser.mly"
       ( Bool(true) )
# 2595 "kawaparser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 95 "kawaparser.mly"
       (This)
# 2606 "kawaparser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run99 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | INT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | LPAR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NEW ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NOT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | THIS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | INT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | LPAR ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | NEW ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | NOT ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | SUB ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | THIS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | RPAR ->
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
        | ADD | AND | COMMA | DIV | DOT | EQ | EQS | GE | GT | LE | LT | MUL | NEQ | NEQS | OR | REM | RPAR | SEMI | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (id : (
# 9 "kawaparser.mly"
       (string)
# 2783 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 100 "kawaparser.mly"
                 (New (id))
# 2788 "kawaparser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | INT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LPAR ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NEW ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | SUB ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (int)
# 2836 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 8 "kawaparser.mly"
       (int)
# 2844 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.expr) = 
# 92 "kawaparser.mly"
        ( Int(n) )
# 2849 "kawaparser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | INT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | LPAR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | NEW ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | NOT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | THIS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "kawaparser.mly"
       (string)
# 2896 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (
# 9 "kawaparser.mly"
       (string)
# 2904 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.mem_access) = 
# 107 "kawaparser.mly"
          (Var(id))
# 2909 "kawaparser.ml"
     in
    _menhir_goto_memoire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 94 "kawaparser.mly"
        ( Bool(false) )
# 2920 "kawaparser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.class_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 2929 "kawaparser.ml"
     in
    _menhir_goto_list_class_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ATTRIBUTE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState136
            | END | METHOD ->
                _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState136
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
        | EXTENDS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ATTRIBUTE ->
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | END | METHOD ->
                        _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_types : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (t : (Kawa.typ))), (id : (
# 9 "kawaparser.mly"
       (string)
# 3029 "kawaparser.ml"
                ))) = _menhir_stack in
                let _v : (string * Kawa.typ) = 
# 61 "kawaparser.mly"
                            ((id, t))
# 3034 "kawaparser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9
                | CLASS | END | FALSE | IDENT _ | IF | INT _ | LPAR | MAIN | NEW | NOT | PRINT | RETURN | SUB | THIS | TRUE | WHILE ->
                    _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState9
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (t : (Kawa.typ))), (id : (
# 9 "kawaparser.mly"
       (string)
# 3079 "kawaparser.ml"
                ))) = _menhir_stack in
                let _v : (string * Kawa.typ) = 
# 68 "kawaparser.mly"
                                   ( (id, t) )
# 3084 "kawaparser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ATTRIBUTE ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState134
                | END | METHOD ->
                    _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState134
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
                | T_INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | RPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState26 in
                    let _v : ((string * Kawa.typ) list) = 
# 142 "<standard.mly>"
    ( [] )
# 3141 "kawaparser.ml"
                     in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_param__ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (id : (
# 9 "kawaparser.mly"
       (string)
# 3172 "kawaparser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (t : (Kawa.typ))) = _menhir_stack in
            let _v : (string * Kawa.typ) = 
# 86 "kawaparser.mly"
                 ((id, t))
# 3178 "kawaparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
                | T_INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
                let _v : ((string * Kawa.typ) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3208 "kawaparser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_param_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_var_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Kawa.typ))), _, (xs : ((string * Kawa.typ) list))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 3237 "kawaparser.ml"
         in
        _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLASS ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | MAIN ->
            _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | IF ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | INT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | LPAR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NEW ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NOT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | PRINT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | RETURN ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | THIS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | WHILE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | END ->
            _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 75 "kawaparser.mly"
       ( TVoid )
# 3300 "kawaparser.ml"
     in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 74 "kawaparser.mly"
        ( TInt )
# 3311 "kawaparser.ml"
     in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "kawaparser.mly"
       (string)
# 3318 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (id : (
# 9 "kawaparser.mly"
       (string)
# 3326 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.typ) = 
# 76 "kawaparser.mly"
           ( TClass(id) )
# 3331 "kawaparser.ml"
     in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 73 "kawaparser.mly"
       ( TBool )
# 3342 "kawaparser.ml"
     in
    _menhir_goto_types _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Kawa.typ) list) = 
# 211 "<standard.mly>"
    ( [] )
# 3546 "kawaparser.ml"
     in
    _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | T_INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Kawa.program) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CLASS | MAIN ->
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3605 "kawaparser.ml"
