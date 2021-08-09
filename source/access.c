/*
	apl compiler
		access format inferencer
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
	This pass is in charge of setting two bits appropriately in the
	info field for each node - these bits are as follows

	SEQUENTIAL - access will be in ravel order, once through the entire
		structure

	HAVEVALUE - at the end of the shape phase the value field will have
		the pointer to all of memory for the node

	NOINDEX - set if the expression in the node does not need an index
		register (i.e., it is a scalar and always the same value,
		or access is sequential and it maintains its own counter).

	MERGED - set if the function of the node can be merged with its
		parent node to form a single accessor.
*/

# include "parse.h"
# include "y.tab.h"
# include <stdio.h>

# define is_scalar(node) ((node->info & RANKKNOWN) && (node->rank.n == 0))
# define is_vector(node) ((node->info & RANKKNOWN) && (node->rank.n == 1))

/* passinit - initialize this pass (do nothing) */
passinit()
{;}

/* glbvar - process a global variable name */
glbvar(name)
char *name;
{;}

/* doprog - process function */
doprog(head, syms, code)
struct headnode *head;
struct symnode *syms;
struct statenode *code;
{  
	struct statenode *p;
	int doaccess();

	stmtno = 0;
	for (p = code ; p != NILSTATE; p = p->nextstate) {
		stmtno++;
		(void) doaccess(p->code, 1);
	}
	fresyms(syms);
	frecode(code);
}

/* setaxis - set sequential mode in axis */
setaxis(node)
struct node *node;
{
	if ((node->info & FIRSTAXIS) || (node->info & LASTAXIS))
		;	/* nothing to do */
	else {
		(node->a.axis)->info |= SEQUENTIAL;
		(void) doaccess(node->a.axis, 0);
		}
}

/*
	addcollect - add a collect node into the tree to avoid
	repeated calls on a costly operation
*/
static struct node *addcollect(node)
struct node *node;
{	struct node *x;

/*fprintf(stderr,"adding a collect over %d\n", node->nodetype);*/

	x = structalloc(node);
	if (x == (struct node *) 0)
		error("out of space");
	x->nodetype = COLLECT;
	x->info = 0;
	x->left = x->a.axis = NILP;
	x->right = node;
	x->optype = NOT;
	x->index = 0;
	return(x);
}

/*
	doaccess - propigate accessing information */
int doaccess(node, top)
struct node *node;
int top;
{	int lcomp, rcomp, cmplx;

	cmplx = 0;

	switch (node->nodetype) {

	default: 
		/*caserr("doaccess",node->nodetype);*/

	case ASSIGN:
		if (top)
			node->info |= SEQUENTIAL;
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT, 0);
		if (! (node->info & SEQUENTIAL))
			node->info |= HAVEVALUE;
		cmplx = 0;
		break;

	case AVEC:
		cmplx = 0;
		break;

# define is_icon(node) ((node->nodetype == BCON) || (node->nodetype == ICON))
	case BCON: 
	case ICON: 
	case RCON: 
	case SCON:
	case IDENT:
		if (is_scalar(node) && is_icon(node))
			;
		else
			node->info |= HAVEVALUE;

		if (is_scalar(node))
			node->info |= NOINDEX;

		if ((node->info & TYPEKNOWN) && 
			(node->info & RANKKNOWN) &&
			(node->info & SEQUENTIAL))
				node->info |= NOINDEX;
		cmplx = 0;
		break;

	case BOX: 
	case QQUAD: 
	case DBOX: 
	case DQQUAD:
		if (RIGHT != NILP) {
			RIGHT->info |= SEQUENTIAL;
			doaccess(RIGHT,0);
		}
		node->info |= HAVEVALUE;
		cmplx = 0;
		break;

	case BOXASSIGN: 
	case DBOXAS: 
	case QBOXAS: 
	case DQBOXAS:
		RIGHT->info |= SEQUENTIAL;
		if (LEFT != NILP) {     /* file name */
			LEFT->info |= SEQUENTIAL;
			doaccess(LEFT,0);
		}
		doaccess(RIGHT,0);
		cmplx = 0;
		break;

	case CAT:
		if (node->info & SEQUENTIAL) {
			LEFT->info  |= SEQUENTIAL;
			RIGHT->info |= SEQUENTIAL;
		}
		/*if (axisgiven(AXIS)) {
			if ((! icons(AXIS)) && AXIS->nodetype != RCON) {
				AXIS->info |= SEQUENTIAL;
				doaccess(AXIS, 0);
			}
		}*/
		lcomp = doaccess(LEFT, 0);
		rcomp = doaccess(RIGHT, 0);
		cmplx = lcomp + rcomp;
		break;

	case COLLECT:
		RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT,0);
		cmplx = 0;
		break;

	case COMMENT:
		cmplx = 0;
		break;

	case COMPRESS:
		LEFT->info |= SEQUENTIAL;
		lcomp = doaccess(LEFT, 0);
		rcomp = doaccess(RIGHT, 0);
		cmplx = rcomp;
		break;

	case CSCALAR:
	case CVEC:
	case CIVEC:
		RIGHT->info |= SEQUENTIAL;
		rcomp = doaccess(RIGHT,0);
		node->info |= HAVEVALUE;
		cmplx = 0;
		break;

	case DEAL:
		LEFT->info |= SEQUENTIAL;
		RIGHT->info |= SEQUENTIAL;
		node->info |= HAVEVALUE;
		lcomp = doaccess(LEFT, 0);
		rcomp = doaccess(RIGHT, 0);
		cmplx = 0;
		break;

	case DECODE:
	case INNERCHILD:
		lcomp = doaccess(LEFT, 0);
		if (lcomp > 0) {
			node->left = addcollect(LEFT);
			lcomp = doaccess(LEFT, 0);
			}
		rcomp = doaccess(RIGHT, 0);
		if (rcomp > 0) {
			node->right = addcollect(RIGHT);
			rcomp = doaccess(RIGHT, 0);
			}
		cmplx = 5;
		break;

	case DSOP:
		if (node->info & SEQUENTIAL) {
			RIGHT->info |= SEQUENTIAL;
			LEFT->info  |= SEQUENTIAL;
		}
		lcomp = doaccess(RIGHT, 0);
		rcomp = doaccess(LEFT, 0);
		if ((RIGHT->info & NOINDEX) && (LEFT->info & NOINDEX))
			node->info |= NOINDEX;
		cmplx = lcomp + rcomp;
		break;

	case DSYSFUN: 
	case MSYSFUN:
		if (node->nodetype == MSYSFUN)
			doaccess(RIGHT,0);
		else {
			doaccess(LEFT, 0);
			doaccess(RIGHT, 0);
			}
		node->info |= HAVEVALUE;
		cmplx = 0;
		break;

	case DTRANS:
		if (is_mergeable(RIGHT))
			RIGHT->info |= MERGED;
		lcomp = doaccess(LEFT, 0);
		rcomp = doaccess(RIGHT, 0);
		if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;
		cmplx = rcomp;
		break;

	case EMPTSEMI:
		cmplx = 0;
		break;

	case ENCODE:
		lcomp = doaccess(LEFT, 0);
		rcomp = doaccess(RIGHT, 0);
		cmplx = lcomp + rcomp;
		break;

	case EXPAND:
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		LEFT->info |= SEQUENTIAL;
		lcomp = doaccess(LEFT, 0);
		rcomp = doaccess(RIGHT, 0);
		cmplx = rcomp;
		break;

	case EPSILON:
		if (node->info & SEQUENTIAL)
			LEFT->info |= SEQUENTIAL;
		lcomp = doaccess(LEFT, 0);
		rcomp = doaccess(RIGHT, 0);
		cmplx = lcomp;
		break;

	case FIDENT:
		node->info |= HAVEVALUE;
		if (LEFT != NILP)
			doaccess(LEFT,0);
		if (RIGHT != NILP)
			doaccess(RIGHT,0);
		cmplx = 0;
		break;

	case FORMAT:
		RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT, 0);
		cmplx = 0;
		break;

	case GO:
		RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT,0);
		cmplx = 0;
		break;

	case INDEX:
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		lcomp = doaccess(LEFT, 0);
		rcomp = doaccess(RIGHT, 0);
		cmplx = rcomp;
		break;

	case INNER:
		rcomp = doaccess(RIGHT, 0);
		if (is_scalar(node))
			node->info |= NOINDEX;
		cmplx = rcomp;
		break;

	case IOTA:
		rcomp = doaccess(RIGHT, 0);
		if (node->info & SEQUENTIAL)
			node->info |= NOINDEX;
		cmplx = 0;
		break;

	case LCON:
		node->info |= HAVEVALUE;
		cmplx = 0;
		break;

	case LAM:
		error("laminate not implemented");
		cmplx = 0;
		break;

	case MSOP:
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		rcomp = doaccess(RIGHT, 0);
		if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;
		cmplx = rcomp;
		break;

	case OUTER:
		if (node->info & SEQUENTIAL)
			LEFT->info |= SEQUENTIAL;
		rcomp = doaccess(RIGHT, 0);
		if (rcomp > 0) {
			node->right = addcollect(RIGHT);
			rcomp = doaccess(RIGHT, 0);
			}
		lcomp = doaccess(LEFT, 0);
		if (! (node->info & SEQUENTIAL))
			if (lcomp > 0) {
				node->left = addcollect(LEFT);
				lcomp = doaccess(LEFT, 0);
				}
		cmplx = rcomp + lcomp;
		break;

	case RAVEL:
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		rcomp = doaccess(RIGHT,0);
		if (RIGHT->info & HAVEVALUE)
			node->info |= HAVEVALUE;
		if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;
		cmplx = rcomp;
		break;

	case REDUCE:
		setaxis(node);
		if ((node->info & SEQUENTIAL) && 
		    (node->info & LASTAXIS) &&
		    (commute(node->optype)))
			RIGHT->info |= SEQUENTIAL;
		rcomp = doaccess(RIGHT,0);
		if (is_vector(RIGHT) && commute(node->optype))
			node->info |= NOINDEX;
		if (RIGHT->info & SEQUENTIAL)
			cmplx = 0;
		else {
			if (rcomp > 0) {
				node->right = addcollect(RIGHT);
				rcomp = doaccess(RIGHT, 0);
				}
			cmplx = 5;
			}
		break;

	case REVERSE:
		setaxis(AXIS);
		if (is_mergeable(RIGHT))
			RIGHT->info |= MERGED;
		rcomp = doaccess(RIGHT,0);
		if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;
		cmplx = rcomp;
		break;

	case RESHAPE:
		lcomp = doaccess(LEFT, 0);
		rcomp = doaccess(RIGHT, 0);
		cmplx = rcomp;
		break;

	case RHO:
		node->info |= HAVEVALUE;
		rcomp = doaccess(RIGHT,0);
		cmplx = 0;
		break;

	case RHORHO:
		node->info |= HAVEVALUE;
		rcomp = doaccess(RIGHT,0);
		cmplx = 0;
		break;

	case ROLL:
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		rcomp = doaccess(RIGHT,0);
		cmplx = 0;
		break;

	case ROTATE:
		setaxis(node);
		lcomp = doaccess(LEFT, 0);
		rcomp = doaccess(RIGHT, 0);
		cmplx = lcomp + rcomp;
		break;

	case SCAN:
		setaxis(node);
		if ((node->info & SEQUENTIAL) && 
		    (node->info & LASTAXIS) &&
		    commute(node->optype))
			RIGHT->info |= SEQUENTIAL;
		rcomp = doaccess(RIGHT, 0);
		if (RIGHT->info & SEQUENTIAL) {
			cmplx = 0;
			if (RIGHT->info & NOINDEX)
				node->info |= NOINDEX;
			}
		else {
			if (rcomp > 0) {
				node->right = addcollect(RIGHT);
				rcomp = doaccess(RIGHT, 0);
				}
			cmplx = 5;
			}
		break;

	case SM:
		if ((node->info & SEQUENTIAL) && (LEFT == NILP))
			RIGHT->info |= SEQUENTIAL;
		if (LEFT != NILP)
			lcomp = doaccess(LEFT, 0);
		else
			lcomp = 0;
		rcomp = doaccess(RIGHT, 0);
		if (LEFT == NILP) {
			if (RIGHT->info & NOINDEX)
				node->info |= NOINDEX;
			}
		else if ((LEFT->info & NOINDEX) && (RIGHT->info & NOINDEX))
			node->info |= NOINDEX;
		cmplx = lcomp + rcomp;
		break;

	case SORT:
		rcomp = doaccess(RIGHT,0);
		cmplx = 0;
		break;

	case SUB:
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		lcomp = doaccess(LEFT, 0);
		rcomp = doaccess(RIGHT, 1);
		cmplx = lcomp + rcomp;
		break;

	case SUBASSIGN:
		RIGHT->info |= SEQUENTIAL;
		rcomp = doaccess(RIGHT,0);
		lcomp = doaccess(AXIS,1);
		cmplx = 0;
		break;

	case SYSVAR:
		if (RIGHT != NILP)
			doaccess(RIGHT,0);
		node->info |= HAVEVALUE;
		cmplx = 0;
		break;

	case TAKE:
	case DROP:
		if (is_mergeable(RIGHT))
			RIGHT->info |= MERGED;
		lcomp = doaccess(LEFT, 0);
		rcomp = doaccess(RIGHT, 0);
		if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;
		cmplx = rcomp;
		break;

	case TRANS:
		if (is_mergeable(RIGHT))
			RIGHT->info |= MERGED;
		rcomp = doaccess(RIGHT, 0);
		if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;
		cmplx = rcomp;
		break;
	}
	return(cmplx);
}
