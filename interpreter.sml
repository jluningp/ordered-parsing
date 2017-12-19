signature INTERPRETER =
sig
  datatype exp = LAM of string * exp
               | APP of exp * exp
               | EID of string
  val elaborate : Parser.ast -> exp
  val printElab : exp -> string
  val reduce : exp -> exp
  val repl : unit -> unit
end

structure Interpreter : INTERPRETER =
struct
  open Parser
  open Lexer

  datatype exp = LAM of string * exp
               | APP of exp * exp
               | EID of string

  val tmp = ref 0
  fun freshTemp () = (tmp := !tmp + 1; "t" ^ (Int.toString (!tmp)))

  fun printElab e =
      case e of
          LAM (s, exp) => "\\" ^ s ^ "." ^ "(" ^ (printElab exp) ^ ")"
        | APP (e1, e2) => (printElab e2) ^ " |> " ^ (printElab e1)
        | EID s => "$" ^ s

  fun elaborate e =
      case e of
          APPOVER(APPUNDER(APPOVER (TOKEN SLASH, TOKEN (ID s)), TOKEN DOT), body) => LAM (s, elaborate body)
        | APPUNDER(e2, APPOVER (TOKEN PIPE, e1)) => APP (elaborate e1, elaborate e2)
        | APPOVER(TOKEN DOLLAR, TOKEN (ID s)) => EID s
        | _ => raise Fail ("Invalid AST " ^ (printAst e))

  fun freeVarsInExp e =
      case e of
          LAM (s, e') => List.filter (fn x => x = s) (freeVarsInExp e')
        | APP (e1, e2) => freeVarsInExp e1 @ freeVarsInExp e2
        | EID s => [s]

  fun subst (s, (e, freeVars)) exp =
      case exp of
          EID s' => if s = s' then e else EID s'
        | LAM (s', exp') =>
          if s' = s then LAM (s', exp')
          else
            if List.exists (fn x => s' = x) freeVars
            then
              let
                val fresh = freshTemp ()
                val varied = (subst (s', (EID fresh, [fresh])) exp')
              in
                LAM (fresh, subst (s, (e, freeVars)) varied)
              end
            else LAM (s', subst (s, (e, freeVars)) exp')
        | APP (e1, e2) => APP (subst (s, (e, freeVars)) e1, subst (s, (e, freeVars)) e2)

  fun reduce' e prev =
      if (SOME e) = prev then e else
      case e of
          APP (LAM (s, exp), e') => reduce' (subst (s, (e', freeVarsInExp e')) exp) (SOME e)
        | APP (e1, e2) => reduce' (APP (reduce' e1 NONE, reduce' e2 NONE)) (SOME e)
        | LAM (s, exp) => LAM (s, exp)
        | EID s => EID s

  fun reduce e = reduce' e NONE

  fun repl () = (print "> ";
                 let
                   val expr = TextIO.inputLine TextIO.stdIn
                 in
                   case expr of
                       SOME "exit\n" => ()
                     | NONE => ()
                     | SOME exp' =>
                       (let
                         val exp = Substring.string (Substring.trimr 1 (Substring.full exp'))
                         val ast = parse exp
                         val elab = elaborate ast
                         val tempCtr = !tmp
                       in
                         print (printElab (reduce elab) ^ "\n");
                         if tempCtr < !tmp
                         then (print "Variables alpha-varied to avoid capture ";
                               print "replaced with fresh variables t1, t2, ...\n")
                         else ()
                       end
                        handle Fail s => print (s ^ "\n");
                        repl ())
                 end)
end
