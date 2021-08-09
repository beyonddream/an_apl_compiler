/*
	apl compiler
		access format inferencer
		timothy a. budd
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

	stmtno = 0;
	for (p = code ; p != NILSTATE; p = p->nextstate) {
		stmtno++;
		doaccess(p->code, 1);
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
		doaccess(node->a.axis, 0);
		}
}

/* doaccess - propigate accessing information */
doaccess(node, top)
struct node *node;
int top;
{

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
		break;

	case AVEC:
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
		/*else if (node->info & TYPEKNOWN) - i think this is right*/
			node->info |= HAVEVALUE;

		if (is_scalar(node))
			node->info |= NOINDEX;

		if ((node->info & RANKKNOWN) && (node->info & SEQUENTIAL))
			node->info |= NOINDEX;
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
		doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		break;

	case COLLECT:
		RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT,0);
		break;

	case COMMENT:
		break;

	case COMPRESS:
		LEFT->info |= SEQUENTIAL;
		doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		break;

	case CSCALAR:
	case CVEC:
	case CIVEC:
		RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT,0);
		node->info |= HAVEVALUE;
		break;

	case DEAL:
		LEFT->info |= SEQUENTIAL;
		RIGHT->info |= SEQUENTIAL;
		node->info |= HAVEVALUE;
		doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		break;

	case DECODE:
	case INNERCHILD:
		doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		break;

	case DSOP:
		if (node->info & SEQUENTIAL) {
			RIGHT->info |= SEQUENTIAL;
			LEFT->info  |= SEQUENTIAL;
		}
		doaccess(RIGHT, 0);
		doaccess(LEFT, 0);
		if ((RIGHT->info & NOINDEX) && (LEFT->info & NOINDEX))
			node->info |= NOINDEX;
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
		break;

	case DTRANS:
		if (is_mergeable(RIGHT))
			RIGHT->info |= MERGED;
		doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;
		break;

	case EMPTSEMI:
		break;

	case ENCODE:
		doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		break;

	case EXPAND:
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		LEFT->info |= SEQUENTIAL;
		doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		break;

	case EPSILON:
		if (node->info & SEQUENTIAL)
			LEFT->info |= SEQUENTIAL;
		doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		break;

	case FIDENT:
		node->info |= HAVEVALUE;
		if (LEFT != NILP)
			doaccess(LEFT,0);
		if (RIGHT != NILP)
			doaccess(RIGHT,0);
		break;

	case FORMAT:
		RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT, 0);
		break;

	case GO:
		RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT,0);
		break;

	case INDEX:
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		break;

	case INNER:
		doaccess(RIGHT, 0);
		if (is_scalar(node))
			node->info |= NOINDEX;
		break;

	case IOTA:
		doaccess(RIGHT, 0);
		if (node->info & SEQUENTIAL)
			node->info |= NOINDEX;
		break;

	case LCON:
		node->info |= HAVEVALUE;
		break;

	case LAM:
		/*LEFT->info |= (node->info & SEQUENTIAL);
		RIGHT->info |= (node->info & SEQUENTIAL);
		lrinf(LEFT, RIGHT);
		if ((node->a.axis)->nodetype != RCON) {
			(node->a.axis)->info |= SEQUENTIAL;
			doaccess(node->a.axis, 0);
		}*/
		break;

	case MSOP:
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT, 0);
		if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;
		break;

	case OUTER:
		if (node->info & SEQUENTIAL)
			LEFT->info |= SEQUENTIAL;
		doaccess(RIGHT, 0);
		doaccess(LEFT, 0);
		break;

	case RAVEL:
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT,0);
		if (RIGHT->info & HAVEVALUE)
			node->info |= HAVEVALUE;
		if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;
		break;

	case REDUCE:
		setaxis(node);
		if ((node->info & SEQUENTIAL) && 
		    (node->info & LASTAXIS) &&
		    (commute(node->optype)))
			RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT,0);
		if (is_vector(RIGHT) && commute(node->optype))
			node->info |= NOINDEX;
		break;

	case REVERSE:
		setaxis(AXIS);
		if (is_mergeable(RIGHT))
			RIGHT->info |= MERGED;
		doaccess(RIGHT,0);
		if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;
		break;

	case RESHAPE:
		doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		/*if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;*/
		break;

	case RHO:
		node->info |= HAVEVALUE;
		doaccess(RIGHT,0);
		break;

	case RHORHO:
		node->info |= HAVEVALUE;
		doaccess(RIGHT,0);
		break;

	case ROLL:
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT,0);
		break;

	case ROTATE:
		setaxis(node);
		doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		break;

	case SCAN:
		setaxis(node);
		if ((node->info & SEQUENTIAL) && 
		    (node->info & LASTAXIS) &&
		    commute(node->optype))
			RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT, 0);
		break;

	case SM:
		if ((node->info & SEQUENTIAL) && (LEFT == NILP))
			RIGHT->info |= SEQUENTIAL;
		if (LEFT != NILP)
			doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		if (LEFT == NILP) {
			if (RIGHT->info & NOINDEX)
				node->info |= NOINDEX;
			}
		else if ((LEFT->info & NOINDEX) && (RIGHT->info & NOINDEX))
			node->info |= NOINDEX;
		break;

	case SORT:
		doaccess(RIGHT,0);
		break;

	case SUB:
		if (node->info & SEQUENTIAL)
			RIGHT->info |= SEQUENTIAL;
		doaccess(LEFT, 0);
		doaccess(RIGHT, 1);
		break;

	case SUBASSIGN:
		RIGHT->info |= SEQUENTIAL;
		doaccess(RIGHT,0);
		doaccess(AXIS,1);
		break;

	case SYSVAR:
		if (RIGHT != NILP)
			doaccess(RIGHT,0);
		node->info |= HAVEVALUE;
		break;

	case TAKE:
	case DROP:
		if (is_mergeable(RIGHT))
			RIGHT->info |= MERGED;
		doaccess(LEFT, 0);
		doaccess(RIGHT, 0);
		if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;
		break;

	case TRANS:
		if (is_mergeable(RIGHT))
			RIGHT->info |= MERGED;
		doaccess(RIGHT, 0);
		if (RIGHT->info & NOINDEX)
			node->info |= NOINDEX;
		break;
	}
}
