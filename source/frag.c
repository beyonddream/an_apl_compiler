/*
	APL Compiler

	Very Small, but Commonly Used, Code Fragments
		(putting them together saves data space)
	tim budd
*/

/*
	The APL Compiler is Public Domain
	It may be freely redistributed as long as this notice of authorship
	is retained with the sources.

		Tim Budd
		Oregon State University
		Department of Computer Science
		Corvallis, Oregon 97331 USA

	No guarantees are given as to the suitablity of this code for any
	purposes whatsoever
*/
# include "parse.h"
# include <stdio.h>

commasp() {printf(", ");}	/* comma space */

				/* compare tree to zero */
ctzero(t, i, op) struct codetree *t; int i; char *op;
{ if (i) t = gmon(deref, gsop(PLUS, t, gicn(iptr, i, INT)));
printf("if ("); ctgen(t); printf(" %s 0)\n", op); }

				/* *(mp.ip + i) */
dmppi(m, i) int m, i; {printf("*(mp%d.ip + i%d)", m, i);}

				/* else { */
elsebr() {printf("else {\n");}

				/* i = */
ieq(i) int i; {printf("i%d = ", i);}

				/* i1 = c; */
ieqc(i, c) int i, c; { printf("i%d = %d;\n", i, c); }

				/* i1 = i2; */
ieqi(i1, i2) int i1, i2; {printf("i%d = i%d;\n", i1, i2);}

				/* i = tree */
ieqtree(i, t) int i; struct codetree *t;
{ieq(i); ctgen(t); seminl();}

				/* i++; */
iincr(i) int i; {printf("i%d++;\n", i);}

				/* i--; */
idecr(i) int i; {printf("i%d--;\n", i);}

				/* i = i op i */
iopi(i1, i2, op, i3) int i1, i2, i3; char *op;
{printf("i%d = i%d %s i%d;\n", i1, i2, op, i3);}

ifi(i) int i; { printf("if (i%d)\n", i); }

iflp() {printf("if (");}	/* if ( */

iiloop(i1, i2) int i1, i2; 	/* integer loop */
{printf("for (i%d = 0; i%d < i%d; i%d++) {\n", i1, i1, i2, i1);}

				/* down counting loop , type s */
isloop(i1, i2) int i1, i2;
{printf("for (i%d = i%d; i%d >= 0; i%d--) {\n", i1, i2, i1, i1);}

				/* down counting loop,  minus 1 */
izloop(i1, i2) int i1, i2;
{printf("for (i%d = i%d - 1; i%d >= 0; i%d--) {\n", i1, i2, i1, i1);}

				/* valloc(mp, i, INT) */
impalloc(m, i) int m, i; {printf("valloc(&mp%d, i%d, INT);\n", m, i);}

lp() {printf("(");}		/* left parenthesis */

				/* lrank, lshape, rrank, rshape */
lrnsrrns(child1, child2) struct node *child1, *child2;
{ rns(child1); commasp(); rns(child2);}

				/* mp1 = mp2; */
mpeqmp(m1, m2) int m1, m2; {printf("mp%d = mp%d;\n", m1, m2);}

				/* memfree(mp.ip) */
mpfree(i) int i; {printf("memfree(&mp%d.ip);\n", i);}

				/* *(mp + i) = */
mpipieq(m, i) int m, i; {printf("*(mp%d.ip + i%d) = ", m, i);}

				/* (p / (e * s)) * sp */
poests(p, e, s, sp) int p, e, s, sp;
{printf("(i%d / (i%d * i%d)) * i%d", p, e, s, sp);}

				/* prerror - print error message */
prerror(s) char *s; {printf("error(\"%s\");\n", s);}

rbr() {printf("}\n");}		/* right brace */

				/* res = res */
reqr(r1, r2) int r1, r2; { printf("res%d = res%d;\n", r1, r2);}

				/* i = rankstuff */
rkeq(node, i) struct node *node; int i;
{ if (! (node->info & RANKKNOWN)) { ieq(i); 
node->rank.c = gicn(iptr, i, INT); }}

rns(t) struct node *t;		/* rank, shape */
{ctgen(t->rank.c); commasp(); ctgen(t->shape.c);}

rp() {printf(")");}		/* right parenthesis */

rpseminl() {printf(");\n");}	/* right parenthesis, semicolon, newline */

seminl() {printf(";\n");}	/* semicolon - newline combination */

				/* i = const; set index var to constant */
seticon(i, c) int i, c; {printf("i%d = %d;\n", i, c);}

				/* i = 0;  set index var to zero */
setizero(i) int i; {seticon(i, 0);}

				/* mp.ip = i; */
setmptoi(m, i) int m, i; {printf("*mp%d.ip = i%d;\n", m, i);}

				/* i = rank */
setrank(i, node) int i; struct node *node;
{ ieq(i); ctgen(node->rank.c); seminl(); }

				/* settrs */
settrs(t, n) int t; struct node *n;
{ printf("settrs(&trs%d, ", t); ctgen(n->type.c); commasp(); rns(n); rpseminl();}
				/* *mp.ip = tree */
smpieqt(i, t) int i; struct codetree *t;
{printf("*mp%d.ip = ", i); ctgen(t); seminl();}

				/* allocate a scalar mp vector */
smpalloc(i) int i; {printf("valloc(&mp%d, 1, INT);\n", i);}

				/* test reduction flag */
testflag(i, r1, r2) int i, r1, r2;
{ifi(i); printf("{i%d = 0;\nres%d = res%d;\n} else\n", i, r1, r2);}
				/* *(tree + i) */
trepi(t, i) struct codetree *t; int i;
{ctgen(gmon(deref, gsop(PLUS, t, gicn(iptr, i, INT))));}

				/* trs.type = UKTYPE */
trsuk(i) int i; {printf("trs%d.type = UKTYPE;\n", i);}

/*
	slightly longer than one liners
*/

static char *tfstrs[] = {"uck", "i", "i", "r", "c", "uck", "uck"};

/* smpval - set a values into an mpval */
smpval(node, mpval)
struct node *node;
int mpval;
{	struct codetree *vtree;

	vtree = node->values.c;
	if (vtree->cop == memptr)
		return(vtree->c0.cindex);
	if (node->info & TYPEKNOWN) 
		printf("mp%d.%sp = ", mpval, tfstrs[rtype(node)]);
	else
		printf("mp%d = ", mpval);
	ctgen(vtree);
	seminl();
	return(mpval);
}

filtrs(t, n)
int t;
struct node *n;
{
	settrs(t, n);
	if (n->info & TYPEKNOWN)
		printf("trs%d.value.%sp = ", t, tfstrs[rtype(n)]);
	else
		printf("trs%d.value = ", t);
	ctgen(n->values.c);
	seminl();
}

/* rkloop - rank loop */
rkloop(node, loopvar, rankvar)
struct node *node;
int loopvar, rankvar;
{	int v;
	struct codetree *t;

	if (node->info & RANKKNOWN) {
		v = rankvalue(node);
		if (v <= 1) {
			setizero(loopvar);
			printf("{\n");
			return;
			}
		t = gicn(icnst, v-1, INT);
		}
	else {
		setrank(rankvar, node);
		t = gsop(MINUS, gicn(iptr, rankvar, INT),
			gicn(icnst, 1, INT));
		}
	printf("for (i%d = ", loopvar);
	ctgen(t);
	printf("; i%d >= 0; i%d--) {\n", loopvar, loopvar);
}

divmod(top, bottom, div, mod)
int top, bottom, mod, div;
{
	printf("i%d = i%d - i%d * (i%d = i%d / i%d);\n",
		mod, top, bottom, div, top, bottom);
}

esubtract(node, index, e)
struct node *node;
int index, e;
{
	if (node->info & LASTAXIS)
		printf("i%d--;\n", index);
	else
		printf("i%d -= i%d;\n", index, e);
}
