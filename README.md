This is an interpreter for WeirdAf Lambda Calculus, a variation of Lambda Calculus with
weird syntax that's a blend of Bash and OCaml. Now there's a sentence you don't write every day.

The purpose of this language is to demonstrate some very simple parsing techniques. It has delibarately ugly syntax so that students can modify the syntax as an exercise. 

The syntax is as follows:

```
var ::= <anything that doesn't contain a reserved character or spaces>
exp ::=  func | $var | pipe
func ::= \var.exp;
pipe ::= exp |> exp
```

Yes, variables are used with a $ and declared without $, just like in Bash. And yes, that is the
pipe from OCaml.

```
Production rules for parsing:

E'  -->      \id.E;
E'  -->      $id  
E   -->      E |> E'
E   -->      E'
```

Examples of expressions in WeirdAf Lambda Calculus:

```
\x.$x; |> \x.$x;
\x.\y.$x;;
```

To run, load into SML/NJ and start the REPL:
```
$ sml -m sources.cm
Standard ML of New Jersey
- Interpreter.repl()
> \y.$y; |> \x.$x;
\y.$y
> $x |> \y.\x.$y;;
\t1.($x)
Variables alpha-varied to avoid capture replaced with fresh variables t1, t2, ...
> $x |> \x.$x;
$x
> x
Parse Error. ASTs do not reduce to exp: [x]
``` 
