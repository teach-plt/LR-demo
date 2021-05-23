.PHONY: LRtest CYKtest TAGS

LRstems = \
  LR \
  CFG \
  CharacterTokenGrammar \
  DebugPrint \
  ParseTable \
  ParseTable/Pretty \
  SetMaybe \
  Saturation \
  Util

LRfiles = $(patsubst %,src/%.hs,$(LRstems))

default: LR LRtest

.PHONY: build
build: LR-demo.cabal
	cabal build

## Infrastructure

.PRECIOUS: LR-demo.cabal
LR-demo.cabal : package.yaml
	hpack $<

## Testing

cyk : CYK CYKtest

CYKtest : \
  CYKtest/fail/Unguarded \
  CYKtest/BalancedParentheses

LRtest : \
  LRtest/BalancedParentheses \
  LRtest/LogicalExpressions \
  LRtest/RRecLRec \
  LRtest/ExpPlusTimes

LRtest/% : test/%.cf test/%.txt LR
	./LR $(word 1,$^) < $(word 2,$^)

LRtest/fail/% : test/fail/%.cf test/fail/%.txt LR
	! ./LR $(word 1,$^) < $(word 2,$^)

CYKtest/% : test/%.cf test/%.txt CYK
	./CYK $(word 1,$^) < $(word 2,$^)

CYKtest/fail/% : test/fail/%.cf test/fail/%.txt CYK
	! ./CYK $(word 1,$^) < $(word 2,$^)

## Binaries

CYK : main-cyk/CYK.hs src/LBNF/Lex.hs src/LBNF/Par.hs
	ghc -isrc -imain-cyk -main-is CYK --make $< -o $@

LR : lr-demo/Main.hs src/LBNF/Lex.hs src/LBNF/Par.hs $(LRfiles)
	ghc -isrc -ilr-demo --make $< -o $@

pack : CYK.tgz

CYK.tgz : src/LBNF.cf src/LBNF/*.hs src/LBNF/*.x src/LBNF/*.y CYK.hs Makefile
	tar czf $@ $^

## BNFC-generated files

src/LBNF/Abs.hs src/LBNF/Lex.x src/LBNF/Par.y \
                src/LBNF/Lex.hs src/LBNF/Par.hs src/LBNF/Print.hs : src/LBNF.cf
	make -C src

## Misc

TAGS :
	hasktags --etags $(LRfiles)

clean:
	-rm -f src/LBNF/*.log src/LBNF/*.aux src/LBNF/*.hi src/LBNF/*.o src/LBNF/*.dvi *.dyn_o *.dyn_hi *.hi *.o *~

distclean: clean
	-rm -f src/LBNF/Doc.* src/LBNF/Lex.* src/LBNF/Par.* src/LBNF/Layout.* src/LBNF/Skel.* src/LBNF/Print.* src/LBNF/Test.* src/LBNF/Abs.* src/LBNF/Test src/LBNF/ErrM.* src/LBNF/SharedString.* src/LBNF/ComposOp.* src/LBNF/src/LBNF.dtd src/LBNF/XML.*
	-rmdir -p src/LBNF/

# EOF
