/*
	APL compiler
		parse tree i/o utilities
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
# include "parse.h"
# include "y.tab.h"
# include <stdio.h>

extern int    *iconsts, ictop, rctop, sctop, lctop;
extern double *rconsts;
extern char   *sconsts;
extern union label_struct lconsts[];

int    stmtno = 0;		/* the current statement number */
char   *funname = "main";	/* the current function name */


/* error - print error message and quit */
error(c)
char *c;
{  
	fprintf(stderr,"%s:%d:%s\n", funname, stmtno, c);
	exit(1);
}

/* putname - print a name */
putname(c)
char *c;
{
	if (c != NILCHAR)
		fputs(c, stdout);
	putchar('\n');
}

/* put function header */
putheader(head)
struct headnode *head;
{
	(void) fwrite((char *) head, sizeof(struct headnode), 1, stdout);
	putname(head->fname);
	putname(head->asvar);
	putname(head->parm1);
	putname(head->parm2);
}

/* getfname - get function name field */
char *getfname(c)
char *c;
{
	c[0] = 0;
	(void) gets(c);
	if (c[0] == '\0')
		return(NILCHAR);
	else
		return(c);
}

/* get function header */
gethead(head, name, asvar, parm1, parm2)
struct headnode *head;
char *name, *asvar, *parm1, *parm2;
{
	(void) fread((char *) head, sizeof(struct headnode), 1, stdin);
	head->fname = getfname(name);
	head->asvar = getfname(asvar);
	head->parm1 = getfname(parm1);
	head->parm2 = getfname(parm2);
}

/* put symbol table */
putsyms(syms)            
struct symnode *syms;     
{  
	struct symnode *p;       

	for (p = syms; p != 0; p = p->next) {
		if (p->name) {
			putchar(SYMS);
			(void) fwrite((char *) p, sizeof(struct symnode), 1, stdout);
			putname(p->name);
		}
	}
	putchar(ESYMS);
}

/* read symbol table */
struct symnode *getsyms()
{  
	struct symnode *fs, *os, *cs;
	char c, iname[120];

	fs = 0;
	while ((c = getchar()) == SYMS) {
		cs = structalloc(symnode);
		(void) fread((char *) cs, sizeof(struct symnode), 1, stdin);
		(void) gets(iname);
		cs->name = (char *) malloc((unsigned) (1 + strlen(iname)));
		cs->next = NILSYM;
		(void) strcpy(cs->name, iname);
		if (fs == NILSYM)
			fs = os = cs;
		else
			os->next = cs;
		os = cs;
	}
	if (c != ESYMS) {
		fprintf(stderr,"char is %d\n",c);
		error("bad symbol table format");
	}
	return(fs);
}

/* free symbol table */
fresyms(syms)
struct symnode *syms;
{
	if (syms != (struct symnode *) 0) {
		if (syms->next != (struct symnode *) 0)
			fresyms(syms->next);
		free((char *) syms);
	}
}

/* put constants */
putconsts()
{
	int a[4], i;

	putchar(CONSTS);

	a[0] = strlen(sconsts);
	a[1] = ictop;
	a[2] = rctop;
	a[3] = lctop;
	(void) fwrite((char *) a, sizeof(int), 4, stdout);
	for(i=0; i<a[0]; i++)
		putchar(sconsts[i]);
	if (a[1])
		(void) fwrite((char *) iconsts, sizeof(int), ictop, stdout);
	if (a[2])
		(void) fwrite((char *) rconsts, sizeof(double), rctop, stdout);
	if (a[3])
		(void) fwrite((char *) lconsts, 
			sizeof(union label_struct), lctop, stdout);
}

/* read constants */
rdconsts() {
	int a[4], i;

	if (getchar() != CONSTS)
		error("bad format reading constants");
	(void) fread((char *) a, sizeof(int), 4, stdin);
	sctop = a[0];
	ictop = a[1];
	rctop = a[2];
	lctop = a[3];
	for(i=0; i<sctop; i++)
		sconsts[i] = getchar();
	sconsts[i] = 0;
	if (ictop)
		(void) fread((char *) iconsts, sizeof(int), a[1], stdin);
	if (rctop)
		(void) fread((char *) rconsts, sizeof(double), a[2], stdin);
	if (lctop)
		(void) fread((char *) lconsts, 
			sizeof(union label_struct), a[3], stdin);
}

/* put out code */
putcode(code)
struct statenode *code;
{  
	struct statenode *p;

	for (p = code; p != NILSTATE; p = p->nextstate) {
		putchar(STMT);
		(void) fwrite((char *) &p->label, sizeof(int), 1, stdout);
		putnode(p->code);
	}
	putchar(ESTMT);
}

/* free storage used by code */
frecode(code)
struct statenode *code;
{
	if (code->nextstate != NILSTATE)
		frecode(code->nextstate);
	frenode(code->code);
	free((char *) code);
}

/* read code */
struct statenode *rdcode()
{
	struct statenode *sp, *op, *fp;
	struct node *innode();
	char c;

	op = NILSTATE;
	while ((c = getchar()) == STMT) {
		sp = structalloc(statenode);
		(void) fread((char *) &sp->label, sizeof(int), 1, stdin);
		sp->code = innode();
		sp->nextstate = NILSTATE;
		sp->list = NILSYM;
		if (op == NILSTATE)
			op = fp = sp;
		else
			op->nextstate = sp;
		op = sp;
	}
	if (c != ESTMT) {
		fprintf(stderr,"last character is %d\n",c);
		error("bad format");
	}
	return(fp);
}

/* put out node */
putnode(node)
struct node *node;
{
	(void) fwrite((char *) node, sizeof(struct node), 1, stdout);

	switch(node->nodetype) {
	default: 
		fprintf(stderr,"node type %d\n",node->nodetype);
		error("illegal node type output");

	case IDENT:
		puts(node->a.namep);
		break;

	case BCON: 
	case ICON: 
	case RCON: 
	case LCON: 
	case SCON:
	case AVEC: 
	case QQUAD: 
	case BOX: 
	case EMPTSEMI: 
	case COMMENT:
		break;

	case REVERSE: 
	case REDUCE: 
	case SCAN:
		if (axisgiven(node->a.axis))
			putnode(node->a.axis);
		putnode(node->right);
		break;

	case COMPRESS: 
	case EXPAND: 
	case ROTATE:
	case CAT: 
	case LAM:
		putnode(node->left);
		if (axisgiven(node->a.axis))
			putnode(node->a.axis);
		putnode(node->right);
		break;

	case FIDENT:
		if (LEFT != NILP)
			putnode(node->left);
		if (RIGHT != NILP)
			putnode(node->right);
		puts(node->a.namep);
		break;

	case SM:
		if (LEFT != NILP)
			putnode(node->left);
		if (RIGHT != NILP)
			putnode(node->right);
		break;

	case SYSVAR:
		if (node->right != NILP)
			putnode(node->right);
		break;

	case SUBASSIGN:
		putnode(node->left);
		putnode(node->right);
		putnode(node->a.axis);
		break;

	case CSCALAR:
	case DQQUAD: 
	case DBOX: 
	case MSOP: 
	case IOTA: 
	case ROLL:
	case RHO: 
	case RHORHO: 
	case GO: 
	case MSYSFUN:
	case TRANS: 
	case FORMAT: 
	case BOXASSIGN: 
	case QBOXAS:
	case COLLECT: 
	case SORT: 
	case CVEC: 
	case RAVEL: 
	case CIVEC:
	case INNER:
		putnode(node->right);
		break;

	case SUB: 
	case ASSIGN: 
	case DBOXAS: 
	case DQBOXAS: 
	case DSOP:
	case CGOTO:
	case DEAL: 
	case EPSILON: 
	case INDEX: 
	case DTRANS: 
	case TAKE: 
	case DROP: 
	case RESHAPE: 
	case DECODE: 
	case ENCODE:
	case OUTER: 
	case DSYSFUN: 
	case ESYSFUN: 
	case INNERCHILD:
		putnode(node->left);
		putnode(node->right);
		break;
	}
}

/* read nodes */
struct node *innode()
{
	struct node *node;
	char name[120];

	node = structalloc(node);
	if (node == (struct node *) 0)
		error("no room for parse tree");

	(void) fread((char *) node, sizeof(struct node), 1, stdin);

	switch(node->nodetype) {
	default: 
		fprintf(stderr,"node type %d\n",node->nodetype);
		error("illegal node type input");

	case IDENT:
		(void) gets(name);
		node->a.namep = (char *) malloc((unsigned) (1+strlen(name)));
		(void) strcpy(node->a.namep, name);
		break;

	case BCON: 
	case ICON: 
	case RCON: 
	case LCON: 
	case SCON:
	case AVEC: 
	case QQUAD: 
	case BOX: 
	case EMPTSEMI: 
	case COMMENT:
		break;

	case REVERSE: 
	case REDUCE: 
	case SCAN:
		if (axisgiven(node->a.axis))
			node->a.axis = innode();
		node->right = innode();
		break;

	case COMPRESS: 
	case EXPAND: 
	case ROTATE:
	case CAT: 
	case LAM:
		node->left = innode();
		if (axisgiven(node->a.axis))
			node->a.axis = innode();
		node->right = innode();
		break;

	case FIDENT:
		if (LEFT != NILP)
			node->left = innode();
		if (RIGHT != NILP)
			node->right = innode();
		(void) gets(name);
		node->a.namep = (char *) malloc((unsigned) (1+strlen(name)));
		(void) strcpy(node->a.namep, name);
		break;

	case SM:
		if (LEFT != NILP)
			node->left = innode();
		if (RIGHT != NILP)
			node->right = innode();
		break;

	case SYSVAR:
		if (node->right != NILP)
			node->right = innode();
		break;

	case SUBASSIGN:
		node->left = innode();
		node->right = innode();
		node->a.axis = innode();
		break;

	case DQQUAD: 
	case DBOX: 
	case MSOP: 
	case IOTA: 
	case ROLL:
	case RHO: 
	case RHORHO: 
	case GO: 
	case MSYSFUN:
	case TRANS: 
	case FORMAT: 
	case BOXASSIGN: 
	case QBOXAS:
	case COLLECT: 
	case SORT: 
	case CVEC: 
	case CSCALAR:
	case RAVEL: 
	case CIVEC:
	case INNER:
		node->right = innode();
		break;

	case SUB: 
	case ASSIGN: 
	case DBOXAS: 
	case DQBOXAS: 
	case DSOP:
	case CGOTO:
	case DEAL: 
	case EPSILON: 
	case INDEX: 
	case DTRANS: 
	case TAKE: 
	case DROP: 
	case RESHAPE: 
	case DECODE: 
	case ENCODE:
	case OUTER: 
	case DSYSFUN: 
	case ESYSFUN: 
	case INNERCHILD:
		node->left = innode();
		node->right = innode();
		break;
	}
	return(node);
}

/* free storage taken by node */
frenode(node)
struct node *node;
{

	switch(node->nodetype) {
	default: 
		error("illegal node type freed");

	case IDENT:
	case BCON: 
	case ICON: 
	case RCON: 
	case LCON: 
	case SCON:
	case AVEC: 
	case QQUAD: 
	case BOX: 
	case EMPTSEMI: 
	case COMMENT:
		break;

	case REVERSE: 
	case REDUCE: 
	case SCAN:
		if (axisgiven(node->a.axis))
			frenode(node->a.axis);
		frenode(node->right);
		break;

	case COMPRESS: 
	case EXPAND: 
	case ROTATE:
	case CAT: 
	case LAM:
		frenode(node->left);
		if (axisgiven(node->a.axis))
			frenode(node->a.axis);
		frenode(node->right);
		break;

	case FIDENT:
		if (LEFT != NILP)
			frenode(node->left);
		if (RIGHT != NILP)
			frenode(node->right);
		break;

	case SM:
		if (LEFT != NILP)
			frenode(node->left);
		if (RIGHT != NILP)
			frenode(node->right);
		break;

	case SYSVAR:
		if (node->right != NILP)
			frenode(node->right);
		break;

	case SUBASSIGN:
		frenode(node->left);
		frenode(node->right);
		frenode(node->a.axis);
		break;

	case DQQUAD: 
	case DBOX: 
	case MSOP: 
	case IOTA: 
	case ROLL:
	case RHO: 
	case RHORHO: 
	case GO: 
	case MSYSFUN:
	case TRANS: 
	case FORMAT: 
	case BOXASSIGN: 
	case QBOXAS:
	case COLLECT: 
	case SORT: 
	case CVEC: 
	case CSCALAR:
	case RAVEL: 
	case CIVEC:
	case INNER:
		frenode(node->right);
		break;

	case SUB: 
	case ASSIGN: 
	case DBOXAS: 
	case DQBOXAS: 
	case DSOP:
	case CGOTO:
	case DEAL: 
	case EPSILON: 
	case INDEX: 
	case DTRANS: 
	case TAKE: 
	case DROP: 
	case RESHAPE: 
	case DECODE: 
	case ENCODE:
	case OUTER: 
	case DSYSFUN: 
	case ESYSFUN: 
	case INNERCHILD:
		frenode(node->left);
		frenode(node->right);
		break;
	}
	free((char *) node);
}

