/*
	APL compiler

		print parse tree
		(used for debugging)
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

static char *types[7] = {
	"unknown type", "bit", "int", "real", "char", "any", "label"};

static char *classes[6] = {
	"noclass", "global", "param", "local", "function", "label class"};

/* passinit - initialize this pass (do nothing) */
passinit()
{;}

/* glbvar - process a global variable name */
glbvar(name)
char *name;
{;}

doprog(head, syms, code)
struct headnode *head;
struct symnode *syms;
struct statenode *code;
{
	struct symnode *s;
	struct statenode *c;

	fprintf(stderr,"\n\nFunction ");
	if (head == NILHEAD || head->fname == NILCHAR) {
		fprintf(stderr,"main\n");
	}
	else {
		if (head->asvar != NILCHAR)
			fprintf(stderr,"%s <- ", head->asvar);
		if (head->parm1 != NILCHAR)
			fprintf(stderr,"%s ", head->parm1);
		fprintf(stderr,"%s ", head->fname);
		if (head->parm2 != NILCHAR)
			fprintf(stderr,"%s ", head->parm2);
		fprintf(stderr, "\n");
	}
	fprintf(stderr,"Symbols\n");
	for (s = syms; s != (struct symnode *) 0; s = s->next) {
		fprintf(stderr,"\t%s type %s rank %d\n", 
			s->name, types[s->type], s->vrank);
	}
	fprintf(stderr,"Counts maxtrs = %d maxmp = %d maxres = %d maxi = %d\n",
		head->maxtrs, head->maxmp, head->maxres, head->maxi);

	stmtno = 0;
	for (c = code; c != NILSTATE; c = c->nextstate) {
		fprintf(stderr,"\nStatement %d\n", ++stmtno);
		if (c->label != NILCHAR) 
			fprintf(stderr,"labeled\n");
		prntnode(c->code);
	}

}

prnttrs(node)
struct node *node;
{
	if (node->info & TYPEKNOWN)
		fprintf(stderr," type %s", types[node->type.n]);
	if (node->info & RANKKNOWN)
		fprintf(stderr," rank %d", node->rank.n);
	if (node->info & SHAPEKNOWN)
		fprintf(stderr," shape known %d", node->shape.n);
	if (node->info & VALUESKNOWN)
		fprintf(stderr," value known %d", node->values.n);
	if (node->info & SEQUENTIAL)
		fprintf(stderr," SEQUENTIAL");
	if (node->info & HAVEVALUE)
		fprintf(stderr," HAVEVALUE");
	if (node->info & NOINDEX)
		fprintf(stderr," NOINDEX");
	fprintf(stderr,"\n");
	fprintf(stderr,"\tindex = %d ptr1 = %d ptr2 = %d ptr3 = %d\n",
		node->index, node->ptr1, node->ptr2, node->ptr3);
}

prntaxis(node)
struct node *node;
{
	if (node->a.axis != NILP)
		prntnode(node->a.axis);
	else if (node->info & FIRSTAXIS)
		fprintf("\taxis along first position\n");
	else if (node->info & LASTAXIS)
		fprintf("\taxis along last position\n");
}

prntnode(node)
struct node *node;
{

	switch(node->nodetype) {
	default: 
		fprintf(stderr,"node type %d\n",node->nodetype);
		error("illegal node type printed");

	case IDENT:
		fprintf(stderr,"\n\tIdentifier %s", node->a.namep);
		prnttrs(node);
		break;

	case BCON: 
	case ICON: 
	case RCON: 
	case LCON: 
	case SCON:
		fprintf(stderr,"\n\tConstant ");
		prnttrs(node);
		break;

	case BOX: 
	case QQUAD:
		fprintf(stderr,"\n\tInput quad ");
		prnttrs(node);
		break;

	case AVEC:
		fprintf(stderr,"\n\tAtomic vector ");
		prnttrs(node);
		break;

	case EMPTSEMI: 
	case COMMENT:
		fprintf(stderr,"\n\tEmptsemi or comment ");
		prnttrs(node);
		break;

	case REVERSE: 
	case REDUCE: 
	case SCAN:
		fprintf(stderr,"\n\tReverse, reduce or scan");
		prnttrs(node);
		prntaxis(node);
		prntnode(node->right);
		break;

	case COMPRESS: 
	case EXPAND: 
	case CAT: 
	case LAM:
		fprintf(stderr,"\n\tCompress expand cat or lam");
		prnttrs(node);
		prntnode(node->left);
		prntaxis(node);
		prntnode(node->right);
		break;

	case FIDENT:
		fprintf(stderr,"\n\tFunction call %s ", node->a.namep);
		prnttrs(node);
		if (LEFT != NILP)
			prntnode(node->left);
		if (RIGHT != NILP)
			prntnode(node->right);
		break;

	case SM:
		fprintf(stderr,"\n\tSm ");
		prnttrs(node);
		if (LEFT != NILP)
			prntnode(node->left);
		if (RIGHT != NILP)
			prntnode(node->right);
		break;

	case SYSVAR:
		fprintf(stderr,"\n\tSysvar ");
		prnttrs(node);
		if (node->right != NILP)
			prntnode(node->right);
		break;

	case SUBASSIGN:
		fprintf(stderr,"\n\tSub assignment ");
		prnttrs(node);
		prntnode(node->left);
		prntnode(node->right);
		prntnode(node->a.axis);
		break;

	case IOTA:
		fprintf(stderr,"\n\tIota ");
		prnttrs(node);
		prntnode(node->right);
		break;

	case MSOP:
		fprintf(stderr,"\n\tMSOP %d ", node->optype);
		prnttrs(node);
		prntnode(node->right);
		break;

	case COLLECT: 
	case CIVEC: 
	case CVEC:
	case CSCALAR:
		fprintf(stderr,"\n\tCollect type %d ", node->nodetype);
		prnttrs(node);
		prntnode(node->right);
		break;

	case DQQUAD: 
	case DBOX: 
	case ROLL:
	case RHO: 
	case RHORHO: 
	case GO: 
	case MSYSFUN:
	case TRANS: 
	case FORMAT: 
	case BOXASSIGN: 
	case QBOXAS:
	case SORT: 
	case RAVEL: 
		fprintf(stderr,"\n\tMonadic operator %d ", node->nodetype);
		prnttrs(node);
		prntnode(node->right);
		break;

	case DSOP:
		fprintf(stderr,"\n\tDSOP %d ", node->optype);
		prnttrs(node);
		prntnode(node->left);
		prntnode(node->right);
		break;

	case ASSIGN:
		fprintf(stderr,"\n\tAssignment ", node->nodetype);
		prnttrs(node);
		prntnode(node->left);
		prntnode(node->right);
		break;

	case SUB: 
	case DBOXAS: 
	case DQBOXAS:
	case DEAL: 
	case EPSILON: 
	case INDEX: 
	case DTRANS: 
	case ROTATE:
	case TAKE: 
	case DROP: 
	case RESHAPE: 
	case DECODE: 
	case ENCODE:
	case OUTER: 
	case DSYSFUN: 
	case ESYSFUN: 
	case INNER:
		fprintf(stderr,"\n\tDyadic operator %d ", node->nodetype);
		prnttrs(node);
		prntnode(node->left);
		prntnode(node->right);
		break;
	}
}
