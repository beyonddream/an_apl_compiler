/*
	apl compiler
		code generation - print out declarations,
		call routines to generate code
		timothy a. budd
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
/*
	This pass is in charge of code generation.
*/

# include "parse.h"
# include "y.tab.h"
# include <stdio.h>


extern int *iconsts, ictop, rctop, sctop, lctop;
extern double *rconsts;
extern char *sconsts;
extern union label_struct lconsts[];
extern char *funname;

struct node *zeronode;		/* a constant zero - used several places */

# define nl() printf("\n")

/* passinit - initialize this pass (do nothing) */
passinit()
{
	printf("# include \"aplc.h\"\n");
	zeronode = structalloc(node);
}

/* glbvar - process a global variable name */
glbvar(name)
char *name;
{
	printf("extern struct trs_struct %s;\n", name);
}

/* gcdcls - generate constant declarations */
static gcdcls()
{	int i;

	if (ictop) {
		printf("int i_%s[%d] = {", funname, ictop);
		for (i = 0; i < ictop; i++) {
			printf("%d%c ", iconsts[i], ((i+1)<ictop?',':'}'));
			if (i % 10 == 9) nl();
			}
		seminl();
		}
	if (rctop) {
		printf("double r_%s[%d] = {", funname, rctop);
		for (i = 0; i < rctop; i++) {
			printf("%g%c ", rconsts[i], ((i+1)<rctop?',':'}'));
			if (i % 10 == 9) nl();
			}
		seminl();
		}
	if (sctop) {
		printf("char c_%s[] = \"%s\";\n", funname, sconsts);
		}
	if (lctop) {
		printf("int l_%s[%d] = {", funname, lctop);
		for (i = 0; i < lctop; i++) {
			printf("%d%c ", lconsts[i].label_num, 
				((i+1)<rctop?',':'}'));
			if (i % 10 == 9) nl();
			}
		seminl();
		}
}

/* doprog - process function */
doprog(head, syms, code)
struct headnode *head;
struct symnode *syms;
struct statenode *code;
{  
	struct statenode *p;
	struct symnode *sy;
	int i;

	/* generate function declaration */
	if (head == NILHEAD || head->fname == NILCHAR) {
		/* give the real (not forward) declarations for
		global variables */
		for (sy = syms; sy != NILSYM; sy = sy->next) {
			printf("struct trs_struct %s;\n", sy->name);
			}
		gcdcls();
		printf("main() {\n");
		}
	else {
		gcdcls();
		if (head->asvar == NILCHAR)
			head->asvar = "_no1";
		if (head->parm1 == NILCHAR)
			head->parm1 = "_no2";
		if (head->parm2 == NILCHAR)
			head->parm2 = "_no3";
		printf("%s(%s, %s, %s)\n", head->fname, head->asvar,
		head->parm1, head->parm2);
		printf("struct trs_struct *%s, *%s, *%s;\n",
		head->asvar, head->parm1, head->parm2);
		printf("{\n");

		/* give declarations for local variables */
		for (sy = syms; sy != NILSYM; sy = sy->next)
			if (! is_parm(sy->name))
				printf("struct trs_struct %s;\n",
				sy->name);
		}

	/* generate declarations for registers */
	if (head->maxtrs > 0) {
		printf("struct trs_struct ");
		for (i = 1; i <= head->maxtrs; i++) {
			printf("trs%d%c ", i, (i==head->maxtrs)?';':',');
			if (i % 10 == 9) nl();
			}
		nl();
		}
	if (head->maxmp > 0) {
		printf("union mp_struct ");
		for (i = 1; i <= head->maxmp; i++) {
			printf("mp%d%c ", i, (i==head->maxmp)?';':',');
			if (i % 10 == 9) nl();
			}
		nl();
		}
	if (head->maxres > 0) {
		printf("union res_struct res0, ");
		for (i = 1; i <= head->maxres; i++) {
			printf("res%d%c ", i, (i==head->maxres)?';':',');
			if (i % 10 == 9) nl();
			}
		nl();
		}
	printf("int i0%c ", (head->maxi)?',':';');
	for (i = 1; i <= head->maxi; i++) {
		printf("i%d%c ", i, (i==head->maxi)?';':',');
		if (i % 10 == 9) nl();
		}
	printf("\n\n");

	/* give initial values to variables */
	for (sy = syms; sy != NILSYM; sy = sy->next) {
		if (! is_parm(sy->name))
			printf("%s.type = UKTYPE;\n", sy->name);
		}

	/* generate switch statement surrounding code */
	printf("\nstmtno = 1;\n");
	printf("while (stmtno)\n switch(stmtno) {\n");
	printf("default: stmtno = 0; break;\n");

	/* generate code for each statement */
	stmtno = 0;
	for (p = code ; p != NILSTATE; p = p->nextstate) {
		stmtno++;
		printf("case %d: stmtno = %d;\n", stmtno, stmtno);
		printf("trace(\"%s\", %d);\n", funname, stmtno);
		switchbox(p->code, SHAPE, 1);
		switchbox(p->code, FINISH, 1);
	}

	/* generate ending stuff */
	printf("stmtno = 0;\n");
	rbr(); 
	rbr();

	fresyms(syms);
	frecode(code);
}

