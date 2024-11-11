# A parser for LALR(1) grammars in Haskell

Usage:
```
lr-demo MyGrammar.cf < MyInput.txt
```
Prints the generated LALR(1) parse table for context-free grammar `MyGrammar.cf`
and a trace of shift and reduce actions of the parser when accepting `MyInput.txt`.

The input `.cf` file consists of labelled BNF rules (LBNF) of the form:
```
LABEL "." NONTERMINAL "::=" (NONTERMINAL | TERMINAL)* ";"
```
For example:
```
Par.  S ::= "(" S ")" S ;
Done. S ::= ;
```
This format is compatible with BNFC (but BNFC pragmas are not recognized).

Limitation: terminals can only be single characters.

## Getting started

1. Have Haskell Stack installed.
2. Clone and enter this repository.
3. Execute: `stack run lr-demo -- test/BalancedParentheses.cf < test/BalPar.txt`
```
Using the following grammar:

Done . S ::=;
Par . S ::= "(" S ")" S;

Generated parse table:

State 0

	eof	reduce with rule Done
	'('	shift to state 1

	S 	goto state 2

State 1

	'('	shift to state 1
	')'	reduce with rule Done

	S 	goto state 3

State 2

	eof	reduce with rule %start

State 3

	')'	shift to state 4

State 4

	eof	reduce with rule Done
	'('	shift to state 1
	')'	reduce with rule Done

	S 	goto state 5

State 5

	eof	reduce with rule Par
	')'	reduce with rule Par


Parsing stdin...
            . '(' ')'  -- shift
'('         . ')'      -- reduce with rule Done
'(' S       . ')'      -- shift
'(' S ')'   .          -- reduce with rule Done
'(' S ')' S .          -- reduce with rule Par
S           .          -- halt
```

## Reference

```
stack run -- --help
```

## License

BSD 3-clause.
