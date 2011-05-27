WARNINGS=-Wall -fno-warn-missing-signatures

all:
	ghc --make -O2 ${WARNINGS} Main.hs -o demonL

clean:
	@rm *.o *.hi demonL
