# A parser for CFGs in Haskell

- Uses a de Bruijn representation of context-free grammars (CFGs)
- Implements some analyses on grammars using grammar folds

- LR constructs an LALR(1) parse table for a supplied LBNF grammar and executes that parser on some input

- Restrictions: only single character tokens supported for now.
