#
#	Make instructions for the Oregon APL Compiler
#

# compiler, loader options
CFLAGS =
LFLAGS =

# files used to implement intermediate passes of compiler
PASS = pass.o iotree.o const.o
PASSc = pass.c iotree.c const.c

# files used in code generator
GEN = dcls.o switch.o \
asgn.o axis.o collect.o leaf.o misc.o ops.o trans.o \
ctree.o cutil.o frag.o nutil.o
GENc = dcls.c switch.c \
asgn.c axis.c collect.c leaf.c misc.c ops.c trans.c \
ctree.c cutil.c frag.c nutil.c

# files used in the run-time routines
RUN = runio.o runmem.o runop.o runshape.o runtrans.o
RUNc = runio.c runmem.c runop.c runshape.c runtrans.c

#########################################################
#
#   boot procedure
#
#########################################################

all:	parse prnt trs access resource gen run.o
	touch trs.c
	make intra
	echo done

clean:
	rm *.o

#########################################################
#
#   parser, inference maker, code generator
#
#########################################################

parse:	y.tab.o ptree.o psym.o iotree.o const.o 
	cc $(LFLAGS) -o parse y.tab.o ptree.o psym.o iotree.o const.o -lln

prnt:   prnt.o $(PASS)
	cc $(LFLAGS) -o prnt prnt.o $(PASS)

access:	access.o nutil.o $(PASS)
	cc $(LFLAGS) -o access access.o nutil.o $(PASS)

trs:	trs.o util2.o nutil.o $(PASS)
	cc $(LFLAGS) -o trs trs.o util2.o nutil.o $(PASS)

intra:	itrs.o intra.o util2.o nutil.o $(PASS)
	cc $(LFLAGS) -o intra itrs.o intra.o util2.o nutil.o $(PASS)

resource: resource.o util2.o nutil.o $(PASS)
	cc $(LFLAGS) -o resource resource.o util2.o nutil.o $(PASS)

gen:	$(GEN) $(PASS)
	cc $(LFLAGS) -o gen $(GEN) $(PASS)

run.o:	$(RUN)
	ld -r -o run.o $(RUN) 

#########################################################
#
#   conversion rules
#
#########################################################
y.tab.c: apl.y
	yacc -d apl.y

y.tab.o: lex.yy.c

lex.yy.c: apl.lex
	lex apl.lex

itrs.o: trs.c
	cc -c $(CFLAGS) -DINTRA trs.c
	mv trs.o itrs.o

#########################################################
#
#   getting the lint out
#
#########################################################

parselint:
	lint y.tab.c ptree.c psym.c iotree.c const.c -lln

trslint:
	lint trs.c util2.c nutil.c $(PASSc)

accesslint:
	lint access.c nutil.c $(PASSc)

resourcelint:
	lint resource.c util2.c $(PASSc)

genlint:
	lint $(GENc) $(PASSc)

#########################################################
#
#   listings
#
#########################################################

iolist:
	pr aplc Makefile *.h iotree.c const.c pass.c | lpr

parselist:
	pr apl.y apl.lex ptree.c psym.c | lpr

prntlist:
	pr prnt.c | lpr

trslist:
	pr trs.c intra.c | lpr

accesslist:
	pr access.c | lpr

resourcelist:
	pr resource.c | lpr

genlist:
	pr $(GENc) | lpr

runlist:
	pr $(RUNc) | lpr

#########################################################
#
#   test cases 
#
#########################################################
ulamtest: ulam.apl
	aplc -i1 ulam.apl
	echo type a number, such as 8.  output should be a sprial
	echo of numbers, followed by a sprial of 0/1 with 1 on each
	echo prime location, followed by a pattern of *'s
	a.out

#########################################################
#
#   distribution tapes
#
#########################################################

bundle: 
	packmail -o'apl.source'  Makefile apl.y apl.lex  *.c *.h aplc *.apl
