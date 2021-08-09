/*
	apl compiler
		build new parse tree nodes
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

extern char *sconsts;		/* current string constants */

/* strllen - string length without escape characters */
int slen(c)
char *c;
{
	int i;
	char *p;

	i = 0;
	for (p = c; *p ; p++)
		if (*p != '\\')
			i++;
	return(i);
}

/* newid - create a new id string */
char *newid(name)
char *name;
{
	char *x;

	x = (char *) malloc((unsigned) strlen(name)+1);
	if (x == NILCHAR)
		yyerror("out of space");
	strcpy(x, name);
	return(x);
}

/* newhead - make a new function header node */
struct headnode *newhead(name, parm1, parm2)
char *name, *parm1, *parm2;
{
	struct headnode *x;
	struct symnode *enterid();

	x = structalloc(headnode);
	if (x == NILHEAD)
		yyerror("out of space");
	enterid(name, FUNCTION, UKTYPE, NORANK);
	x->fname = name;
	x->asvar = NILCHAR;
	x->parm1 = parm1;
	x->parm2 = parm2;
	if (parm1 != NILCHAR)
		enterid(parm1, PARAM, UKTYPE, NORANK);
	if (parm2 != NILCHAR)
		enterid(parm2, PARAM, UKTYPE, NORANK);
	return(x);
}

/* direct - direct defintion declaration */
direct(label, expr)
char *label;
struct node *expr;
{
	struct node *x, *pt2(), *ptvar();
	struct symnode *s;
	struct headnode *h;
	struct statenode *newstate();
	enum   classes idclass();

	h = newhead(label, "_alpha", "_omega");
	h->asvar = "_zeta";
	idclass("_zeta", &s);
	x = pt2(ASSIGN, ptvar(s), expr);
	prog(h, newstate(NILCHAR, x));
	resetconsts();
	reinitsymtab();
}

/* newstate - produce a new statement node */
struct statenode *newstate(label, code)
char *label;
struct node *code;
{
	struct statenode *x;

	if (code->nodetype == COMMENT && label == NILCHAR)
		return(NILSTATE);
	x = structalloc(statenode);
	if (x == NILSTATE)
		yyerror("out of space");
	x->label = label;
	x->code = code;
	x->nextstate = NILSTATE;
	x->list = NILSYM;
	return(x);
}

/* addstmt - add a statement to statement list */
struct statenode *addstmt(first, new)
struct statenode *first, *new;
{
	struct statenode *i;

	if (new == NILSTATE)
		return(first);
	else if (first == NILSTATE)
		return(new);
	else {
		for (i = first; i->nextstate != NILSTATE; i = i->nextstate)
			;
		i->nextstate = new;
		return(first);
	}
}

/* newnode - produce a new parse tree node */
struct node *newnode(type)
int type;
{
	struct node *x;

	x = structalloc(node);
	if (x == (struct node *)0)
		yyerror("out of space");
	x->nodetype = type;
	x->info = 0;
	x->left = x->right = x->a.axis = NILP;
	x->type.n = x->rank.n = x->shape.n = x->values.n = 0;
	x->optype = NOT;
	x->index = 0;
	return(x);
}

/* pt1 - simple 1 child node */
struct node *pt1(type, child)
int type;
struct node *child;
{
	struct node *x;

	x = newnode(type);
	x->right = child;
	return(x);
}

/* ptaxis - process an axis specifier */
ptaxis(x, axis, ainfo)
struct node *axis, *x;
int ainfo;
{
	if (axis != NILP) {
		x->a.axis = pt1(CSCALAR, axis);
	}
	else {
		x->a.axis = NILP;
		x->info |= ainfo;
	}
}

/* pt1a - 1 child node with axis */
struct node *pt1a(type, axis, ainfo, child)
int type, ainfo;
struct node *child, *axis;
{
	struct node *x;

	x = pt1(type, child);
	ptaxis(x, axis, ainfo);
	return(x);
}

/* pt1o - 1 child node with operator */
struct node *pt1o(type, op, child)
int type;
enum sops op;
struct node *child;
{
	struct node *x;

	x = pt1(type, child);
	x->optype = op;
	return(x);
}

/* pt1ao - one child node with both axis and operator */
struct node *pt1ao(type, op, axis, child, ainfo)
int type, ainfo;
enum sops op;
struct node *axis, *child;
{
	struct node *x;

	x = pt1a(type, axis, ainfo, child);
	x->optype = op;
	return(x);
}

/* pt2 - simple 2 child node */
struct node *pt2(type, child1, child2)
int type;
struct node *child1, *child2;
{
	struct node *x;

	x = newnode(type);
	x->left = child1;
	x->right = child2;
	return(x);
}

/* pt2a - 2 child node with axis */
struct node *pt2a(type, axis, ainfo, child1, child2)
int type, ainfo;
struct node *child1, *child2, *axis;
{
	struct node *x;

	x = pt2(type, child1, child2);
	ptaxis(x, axis, ainfo);
	return(x);
}

/* pt2o - two child node with operator */
struct node *pt2o(type, op, child1, child2)
int type;
enum sops op;
struct node *child1, *child2;
{
	struct node *x;

	x = pt2(type, child1, child2);
	x->optype = op;
	return(x);
}

/* pt2ao - two child node with both operator and axis */
struct node *pt2ao(type, op, axis, ainfo, child1, child2)
int type, ainfo;
enum sops op;
struct node *axis, *child1, *child2;
{	struct node *x;

	x  = pt2a(type, axis, ainfo, child1, child2);
	x->optype = op;
	return(x);
}

/* ptsort - sort function */
struct node *ptsort(direction, child)
int direction;
struct node *child;
{	struct node *x;

	x = pt1(SORT, child);
	x->a.ptr0 = direction;
	return(x);
}

/* ptsvec - string constant (vector) */
struct node *ptsvec(chars)
char *chars;
{
	struct node *x;
	int slen();

	x = newnode(SCON);
	x->type.n = (int) CHAR;
	x->rank.n = 1;
	x->shape.n = addicon(slen(chars));
	x->values.n = slen(sconsts);
	x->info |= (TYPEDECL | TYPEKNOWN | RANKDECL | RANKKNOWN |
	    SHAPEKNOWN | VALUESKNOWN);
	if (strlen(chars) + strlen(sconsts) > MAXCONSTS)
		yyerror("too many string constants");
	strcat(sconsts, chars);
	return(x);
}

/* ptval - scalar value */
struct node *ptval(type, val)
int type;
int val;
{  
	struct node *x;

	x = newnode(type);
	x->rank.n = 0;
	x->shape.n = 1;
	x->values.n = val;
	x->info |= (TYPEDECL | TYPEKNOWN | RANKDECL | RANKKNOWN |
	    SHAPEKNOWN | VALUESKNOWN);
	return(x);
}

/* aptval - add a scalar value, forming an array */
struct node *aptval(node)
struct node *node;
{
	node->rank.n = 1;
	node->shape.n++;
	return(node);
}

/* ptvec - constant vector */
struct node *ptvec(x, type, rtype)
struct node *x;
int type;
int rtype;
{

	x->nodetype = type;
	x->type.n = (int) rtype;
	x->info |= (TYPEDECL | TYPEKNOWN);
	if (x->rank.n)
		x->shape.n = addicon(x->shape.n);
	return(x);
}

struct node *sysparm(name)
char *name;
{
	struct symnode *x, *enterid();
	struct node *ptvar();

	idclass(name, &x);
	if (x == NILSYM) {
		/* declare system generated variables */
	     	enterid(newid("_alpha"), PARAM, UKTYPE, NORANK);
	      	enterid(newid("_omega"), PARAM, UKTYPE, NORANK);
	      	enterid(newid("_zeta"),  LOCAL, UKTYPE, NORANK);
		/* try it again */
		idclass(name, &x);
		if (x == NILSYM)
			yyerror("sysparm error");
		}
	return(ptvar(x));
}

/* ptvar - variable node */
struct node *ptvar(var)
struct symnode *var;
{
	struct node *x;

	x = newnode(IDENT);
	x->a.namep = var->name;
	if ((var->type != UKTYPE) & (var->type != ANY))
		x->info |= ( TYPEDECL | TYPEKNOWN );
	x->type.n = var->type;
	if (var->vrank != NORANK)
		x->info |= ( RANKDECL | RANKKNOWN );
	x->rank.n = var->vrank;
	return(x);
}

/* ptfun - functions */
struct node *ptfun(fun, child1, child2)
struct symnode *fun;
struct node *child1, *child2;
{
	struct node *x;

	x = pt2(FIDENT, child1, child2);
	x->a.namep = fun->name;
	x->type.n = fun->type;
	if ((fun->type != UKTYPE) & (fun->type != ANY))
		x->info |= ( TYPEDECL | TYPEKNOWN );
	x->rank.n = fun->vrank;
	if (fun->vrank != NORANK)
		x->info |= ( RANKDECL | RANKKNOWN );
	return(x);
}

/* ptmrho - monadic rho (shape) */
struct node *ptmrho(child)
struct node *child;
{
	struct node *x;

	if (child->nodetype == RHO) {
		x = child;
		x->nodetype = RHORHO;
	}
	else {
		x = newnode(RHO);
		x->right = child;
	}
	return(x);
}


/* ptsub - subscripted assignment */
struct node *ptsub(child1, child2)
struct node *child1, *child2;
{
	child1->a.axis = child1->right;
	/* child1->left = child1->left */
	child1->right = child2;
	child1->nodetype = SUBASSIGN;
	return(child1);
}

/* pttop - top of expression */
struct node *pttop(child)
struct node *child;
{
	struct node *x;
	int type;

	type = child->nodetype;
	/* if not assignment or function call, print out expression */
	if (type == ASSIGN || type == BOXASSIGN || type == FIDENT ||
	    type == DBOXAS || type == QBOXAS || type == DQBOXAS)
		x = child;
	else if (type == SYSVAR && child->right != NILP)
		x = child;
	else
		x = pt1(BOXASSIGN, child);
	return(x);
}

/* ptsemi - semicolon */
struct node *ptsemi(child1, child2)
struct node *child1, *child2;
{	struct node *x;

	x = pt2(SM, child1, child2);
	if (child1 == NILP)
		x->ptr1 = 0;
	else
		x->ptr1 = child1->ptr1 + 1;
	return(x);
}
