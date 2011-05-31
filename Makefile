WARNINGS=-Wall -fno-warn-missing-signatures

all:
	ghc --make -O2 ${WARNINGS} Main.hs -o demonL

doc: YicesDomain.tex
	pdflatex $<

clean:
	@rm *.o *.hi demonL

YicesDomain.tex: YicesDomain.lhs
	lhs2TeX -o YicesDomain.tex YicesDomain.lhs
