.PHONY: LRtest CYKtest TAGS

LRfiles = \
  LR.hs \
  CFG.hs \
  CharacterTokenGrammar.hs \
  DebugPrint.hs \
  ParseTable.hs \
  ParseTable/Pretty.hs \
  SetMaybe.hs \
  Saturation.hs \
  Util.hs

default: LR LRtest

cyk : CYK CYKtest

CYKtest : \
  CYKtest/fail/Unguarded \
  CYKtest/BalancedParentheses

LRtest : \
  LRtest/BalancedParentheses \
  LRtest/LogicalExpressions \
  LRtest/RRecLRec

LRtest/% : test/%.cf test/%.txt LR
	./LR $(word 1,$^) < $(word 2,$^)

LRtest/fail/% : test/fail/%.cf test/fail/%.txt LR
	! ./LR $(word 1,$^) < $(word 2,$^)

CYKtest/% : test/%.cf test/%.txt CYK
	./CYK $(word 1,$^) < $(word 2,$^)

CYKtest/fail/% : test/fail/%.cf test/fail/%.txt CYK
	! ./CYK $(word 1,$^) < $(word 2,$^)

CYK : % : %.hs LBNF/Test
	ghc --make $< -o $@

LR : % : %.hs LBNF/Test $(LRfiles)
	ghc --make $< -o $@

LBNF/Test.hs LBNF/Lex.x LBNF/Layout.hs LBNF/Par.y : LBNF.cf
	bnfc --haskell -d $<

%.hs: %.y
	happy -gcai $<

%.hs: %.x
	alex -g $<

LBNF/Test: LBNF/Test.hs LBNF/Par.hs LBNF/Lex.hs
	ghc --make $< -o $@

pack : CYK.tgz

CYK.tgz : LBNF.cf LBNF/*.hs LBNF/*.x LBNF/*.y CYK.hs Makefile
	tar czf $@ $^

TAGS :
	hasktags --etags $(LRfiles)

clean:
	-rm -f LBNF/*.log LBNF/*.aux LBNF/*.hi LBNF/*.o LBNF/*.dvi *.dyn_o *.dyn_hi *.hi *.o *~

distclean: clean
	-rm -f LBNF/Doc.* LBNF/Lex.* LBNF/Par.* LBNF/Layout.* LBNF/Skel.* LBNF/Print.* LBNF/Test.* LBNF/Abs.* LBNF/Test LBNF/ErrM.* LBNF/SharedString.* LBNF/ComposOp.* LBNF/LBNF.dtd LBNF/XML.*
	-rmdir -p LBNF/

# EOF
