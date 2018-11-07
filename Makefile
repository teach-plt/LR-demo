.PHONY: test

all : CYK test

test : test/BalancedParentheses test/Unguarded

test/% : test/%.cf test/%.txt CYK
	./CYK $(word 1,$^) < $(word 2,$^)

CYK : CYK.hs LBNF/Test
	ghc --make $< -o $@

LBNF/Test.hs LBNF/Lex.x LBNF/Layout.hs LBNF/Par.y : LBNF.cf
	bnfc --haskell -d $<

LBNF/Par.hs: LBNF/Par.y
	happy -gcai $<

LBNF/Lex.hs: LBNF/Lex.x
	alex -g $<

LBNF/Test: LBNF/Test.hs LBNF/Par.hs LBNF/Lex.hs
	ghc --make $< -o $@

pack : CYK.tgz

CYK.tgz : LBNF.cf LBNF/*.hs LBNF/*.x LBNF/*.y CYK.hs Makefile
	tar czf $@ $^

clean:
	-rm -f LBNF/*.log LBNF/*.aux LBNF/*.hi LBNF/*.o LBNF/*.dvi

distclean: clean
	-rm -f LBNF/Doc.* LBNF/Lex.* LBNF/Par.* LBNF/Layout.* LBNF/Skel.* LBNF/Print.* LBNF/Test.* LBNF/Abs.* LBNF/Test LBNF/ErrM.* LBNF/SharedString.* LBNF/ComposOp.* LBNF/LBNF.dtd LBNF/XML.*
	-rmdir -p LBNF/

# EOF
