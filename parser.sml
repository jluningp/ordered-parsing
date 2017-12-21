 signature PARSER =
sig
  datatype ast = FUN of string * ast
               | PIPE of ast * ast
               | ID of string

  val parse : string -> ast
  val printAst : ast -> string
end

structure Parser : PARSER =
struct
  open Lexer

  datatype typ = EXP
               | EXP'
               | TID
               | TSLASH
               | TDOT
               | TDOLLAR
               | TPIPE
               | TSEMI

  datatype ast = FUN of string * ast
               | PIPE of ast * ast
               | ID of string

  datatype parseTerm = TOKEN of token
                     | AST of ast

  exception ParseError of (parseTerm * typ) list option

  open Lexer

  fun fst (a, b) = a
  fun snd (a, b) = b

  fun printTyp t =
      case t of
          EXP => "exp"
        | EXP' => "exp'"
        | TID => "id"
        | TSLASH => "\\"
        | TDOT =>  "."
        | TDOLLAR => "$"
        | TPIPE => "|>"
        | TSEMI => ";"

  fun printAst t =
      case t of
          FUN (s, e2) => "\\" ^ s ^ "." ^ (printAst e2) ^ ";"
        | PIPE (e1, e2) => (printAst e1) ^ "|>" ^ (printAst e2)
        | ID s => "$" ^ s

  fun printParseTerm t =
      case t of
          TOKEN e => printToken e
        | AST a => printAst a

  (* Assigns a type to each token. *)
  fun lexToType l =
      case l of
          SLASH => (TOKEN SLASH, TSLASH)
        | DOT => (TOKEN DOT, TDOT)
        | IDENT s => (TOKEN (IDENT s), TID)
        | DOLLAR => (TOKEN DOLLAR, TDOLLAR)
        | PIPESYM => (TOKEN PIPESYM, TPIPE)
        | SEMI => (TOKEN SEMI, TSEMI)

  (* The next four functions make up a "Theorem Prover" that also happens to parse. It's a Theorem
     Prover because it takes the rules in a logic and tries to determine if they can be applied in such
     a way that we get a valid judgement (most of the time we want to get to the judgement "e true", but
     in this case we want the judgement e : TEXP). We apply the rules for A / B and A \ B anywhere we can
     find adjacent terms with the right type, until we hopefully get a single AST with type TEXP. *)

  fun func ((_, TSLASH), (TOKEN (IDENT x), TID), (_, TDOT), (AST e, EXP), (_, TSEMI)) = (AST(FUN (x, e)), EXP')
    | func ((_, TSLASH), (TOKEN (IDENT x), TID), (_, TDOT), (AST e, EXP'), (_, TSEMI)) = (AST(FUN (x, e)), EXP')
    | func _ = raise ParseError NONE

  fun var ((_, TDOLLAR), (TOKEN (IDENT x), TID)) = (AST(ID x), EXP')
    | var _ = raise ParseError NONE

  fun pipe ((AST e1, EXP), (_, TPIPE), (AST e2, EXP')) = (AST (PIPE(e1, e2)), EXP)
    | pipe ((AST e1, EXP'), (_, TPIPE), (AST e2, EXP')) = (AST (PIPE(e1, e2)), EXP)
    | pipe _ = raise ParseError NONE

  fun find ([] : (parseTerm * typ) list) = []
    | find [x] = [x]
    | find [x, y] = ([var (x, y)] handle ParseError _ => [x, y])
    | find [x, y, z] =
     ([pipe (x, y, z)]
      handle ParseError _ =>
             (var (x, y))::[z]
             handle ParseError _ =>
                    x::(find [y, z]))
    | find ([w, x, y, z]) =
      (pipe (w, x, y) :: [z]
       handle ParseError _ =>
              (var (w, x)) :: (find (y::[z]))
              handle ParseError _ =>
                     w::(find (x::y::[z])))
    | find (v::w::x::y::z::xs) =
      (func (v, w, x, y, z)) :: (find xs)
      handle ParseError _ =>
             pipe (v, w, x) :: (find (y::z::xs))
             handle ParseError _ =>
                    (var (v, w)) :: (find (x::y::z::xs))
                     handle ParseError _ =>
                            v::(find (w::x::y::z::xs))

  fun prove (L : (parseTerm * typ) list) prev =
      if L = prev then raise ParseError (SOME L)
      else case L of
               [(AST x, EXP)] => (x, EXP)
             | [(AST x, EXP')] => (x, EXP)
             | _ => prove (find L) L


  fun parse s =
      let
        val tokens = lex s
        val withTypes = map lexToType tokens
      in
        fst (prove withTypes [])
        handle ParseError (SOME L) =>
               raise Fail ("Parse Error. ASTs do not reduce to exp: " ^
                           ListFormat.listToString printParseTerm (map fst L))
      end
end
