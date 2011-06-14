WARNINGS=-Wall -fno-warn-missing-signatures

demonL: Main.hs TypeCheck.hs YicesDomain.lhs Parser.hs Script.hs
	ghc --make -O2 ${WARNINGS} Main.hs -o demonL

doc: YicesDomain.tex
	pdflatex $<

clean:
	@rm demonL

YicesDomain.tex: YicesDomain.lhs
	lhs2TeX -o YicesDomain.tex YicesDomain.lhs

test: demonL
	./demonL test.dmn goal.ser
	racket pretty.rkt test.dmn.lisp

# %.hi: %.hs
# 	ghc $<

# %.hi: %.lhs
# 	ghc $<

# # DO NOT DELETE: Beginning of Haskell dependencies
# ParserBasic.o : ParserBasic.hs
# Types.o : Types.hs
# Types.o : ParserBasic.hi
# AST.o : AST.hs
# AST.o : Types.hi
# Goal.o : Goal.hs
# Goal.o : AST.hi
# Yices.o : Yices.hs
# Yices.o : AST.hi
# Yices.o : Types.hi
# GoalSerial.o : GoalSerial.hs
# GoalSerial.o : Types.hi
# GoalSerial.o : Goal.hi
# GoalSerial.o : AST.hi
# GoalSerial.o : Yices.hi
# Parser.o : Parser.hs
# Parser.o : Types.hi
# Parser.o : ParserBasic.hi
# Parser.o : Goal.hi
# Parser.o : AST.hi
# YicesDomain.o : YicesDomain.lhs
# YicesDomain.o : Yices.hi
# YicesDomain.o : Types.hi
# YicesDomain.o : Parser.hi
# YicesDomain.o : AST.hi
# Main.o : Main.hs
# Main.o : GoalSerial.hi
# Main.o : Yices.hi
# Main.o : YicesDomain.hi
# Main.o : Parser.hi
# # DO NOT DELETE: End of Haskell dependencies
