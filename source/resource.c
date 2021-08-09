/*
	APL compiler
		resource declarations
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
	This pass is in charge of allocating resources to the various
	nodes.

	The following are the resources considered:

	trs values - type, rank, shape structures

	mp values - memory pointers

	res values - results (memory values)

	i values - integer registers, for index registers, counters, etc.
*/

# include "parse.h"
# include "y.tab.h"
# include <stdio.h>

# define lrinf(left, right) {doinf(left, 0); doinf(right, 0);}

# define is_scalar(node) ((node->info & RANKKNOWN) && (node->rank.n == 0))
# define is_vector(node) ((node->info & RANKKNOWN) && (node->rank.n == 1))

static int trscounter;		/* counter for trs registers */
static int mpcounter;		/* counter for mp registers */
static int rescounter;		/* counter for res registers */
static int icounter;		/* counter for index registers */
static int trsmax;		/* max value for trs registers */
static int mpmax;		/* max value for mp registers */
static int resmax;		/* max value for res registers */
static int imax;		/* max value for index registers */

extern int numin();

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
	trsmax = mpmax = resmax = imax = 0;
	for (p = code ; p != NILSTATE; p = p->nextstate) {
		stmtno++;
		trscounter = mpcounter = rescounter = icounter = 1;
		doinf(p->code, 1);
		if (trscounter > trsmax) trsmax = trscounter;
		if (mpcounter > mpmax) mpmax = mpcounter;
		if (rescounter > resmax) resmax = rescounter;
		if (icounter > imax) imax = icounter;
	}
	head->maxtrs = trsmax - 1;
	head->maxmp = mpmax - 1;
	head->maxres = resmax - 1;
	head->maxi = imax - 1;
	fresyms(syms);
	frecode(code);
}

/* doaxis - set sequential mode in axis */
doaxis(node)
struct node *node;
{ 	struct node *a;

	if ((node->info & FIRSTAXIS) || (node->info & LASTAXIS))
		;
	else {
		a = node->a.axis;
		doinf(a, 0);
		}
}

/*
	mergeresource -
		process resources for mergable operators
*/
static mergeresource(node, child)
struct node *node, *child;
{
	if (! (node->info & MERGED)) {
		node->ptr1 = mpcounter++;
		node->ptr2 = mpcounter++;
		node->ptr3 = mpcounter++;
		node->ptr4 = icounter++;
		node->ptr5 = mpcounter++;
		node->ptr6 = icounter++;
		node->ptr7 = icounter++;
		node->ptr8 = icounter++;
		if (node->info & SEQUENTIAL)
			node->ptr10 = rescounter++;
		}
	if (is_mergeable(child)) {
		child->ptr1 = node->ptr1;
		child->ptr2 = node->ptr2;
		child->ptr3 = node->ptr3;
		child->ptr4 = node->ptr4;
		child->ptr5 = node->ptr5;
		child->ptr6 = node->ptr6;
		child->ptr7 = node->ptr7;
		child->ptr8 = node->ptr8;
		child->ptr10 = node->ptr10;
		child->index = node->index;
		}
	else
		child->index = icounter++;
}

/*
	doinf - node inferencer */
doinf(node, top)
struct node *node;
int top;
{
	switch (node->nodetype) {

	default: 
		/*caserr("doinf",node->nodetype);*/

	case ASSIGN:
		node->ptr1 = mpcounter++;	/* memory pointer */
		node->ptr2 = trscounter++;	/* trs register */
		node->ptr3 = icounter++;	/* size of right */
		node->ptr4 = rescounter++;	/* result register */
		if (top)
			node->index = icounter++;
		RIGHT->index = node->index;	/* loop index variable */
		doinf(RIGHT, 0);
		break;

	case AVEC:
		break;

	case BCON: 
	case ICON: 
	case RCON: 
	case SCON:
	case LCON:
	case IDENT:
		node->ptr1 = mpcounter++;	/* memory pointer */
		node->ptr2 = rescounter++;	/* result register */
		break;

	case BOX: 
	case QQUAD: 
	case DBOX: 
	case DQQUAD:
		node->ptr1 = trscounter++;
		node->ptr2 = mpcounter++;
		node->ptr3 = rescounter++;
		break;

	case BOXASSIGN: 
	case DBOXAS: 
	case QBOXAS: 
	case DQBOXAS:
		node->ptr3 = icounter++;	/* size of right */
		node->ptr4 = rescounter++;	/* result reg */
		RIGHT->index = icounter++;
		doinf(RIGHT, 0);
		break;

	case CAT:
		node->ptr1 = icounter++;
		node->ptr2 = icounter++;
		node->ptr3 = mpcounter++;
		node->ptr4 = icounter++;
		node->ptr5 = icounter++;
		node->ptr6 = icounter++;
		node->ptr7 = rescounter++;
		node->ptr8 = icounter++;
		node->ptr9 = icounter++;
		node->ptr10 = icounter++;
		if (node->info & SEQUENTIAL) {
			LEFT->index = icounter++;
			RIGHT->index = icounter++;
			}
		else {
			if (is_scalar(LEFT) || is_vector(LEFT))
				LEFT->index = node->ptr9;
			else
				LEFT->index = icounter++;
			if (is_scalar(RIGHT) || is_vector(RIGHT))
				RIGHT->index = node->ptr9;
			else if (LEFT->index != node->ptr9)
				RIGHT->index = LEFT->index;
			else
				RIGHT->index = icounter++;
			}
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		break;

	case COMMENT:
		break;

	case COMPRESS:
	case EXPAND:
		LEFT->index = icounter++;
		doinf(LEFT, 0);
		node->ptr1 = icounter++;
		node->ptr2 = icounter++;
		node->ptr3 = icounter++;
		node->ptr4 = mpcounter++;
		node->ptr5 = icounter++;
		node->ptr6 = rescounter++;
		node->ptr7 = rescounter++;
		node->ptr8 = mpcounter++;
		node->ptr9 = icounter++;
		if (is_scalar(RIGHT) || is_vector(RIGHT))
			RIGHT->index = node->ptr9;
		else
			RIGHT->index = icounter++;
		doinf(RIGHT, 0);
		break;

	case CSCALAR:
		RIGHT->index = icounter++;
		doinf(RIGHT, 0);
		node->ptr1 = rescounter++;
		break;

	case COLLECT:
	case CVEC:
	case CIVEC:
		RIGHT->index = icounter++;
		doinf(RIGHT, 0);
		node->ptr1 = icounter++;
		node->ptr3 = mpcounter++;
		node->ptr4 = mpcounter++;
		node->ptr5 = rescounter++;
		break;

	case DEAL:
		break;

	case DECODE:
	case INNERCHILD:
		if (! (node->info & TYPEKNOWN))
			node->ptr1 = icounter++;
		if (! (node->info & RANKKNOWN))
			node->ptr2 = icounter++;
		if (! (node->info & SHAPEKNOWN))
			node->ptr3 = mpcounter++;
		node->ptr5 = icounter++; /* loop counter */
		node->ptr6 = rescounter++;
		node->ptr7 = rescounter++;
		node->ptr8 = rescounter++;
		node->ptr9 = rescounter++;
		if (is_vector(LEFT))
			LEFT->index = node->ptr5;
		else
			LEFT->index = icounter++;
		if (is_vector(RIGHT))
			RIGHT->index = node->ptr5;
		else	
			RIGHT->index = icounter++;
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		break;

	case DSOP:
		RIGHT->index = LEFT->index = node->index;
		doinf(RIGHT, 0);
		doinf(LEFT, 0);
		node->ptr1 = rescounter++;
		node->ptr2 = rescounter++;
		node->ptr3 = icounter++;
		node->ptr4 = icounter++;
		node->ptr5 = mpcounter++;
		break;

	case DSYSFUN: 
	case MSYSFUN:
		break;

	case DTRANS:
		mergeresource(node, RIGHT);
		if (! (node->info & SHAPEKNOWN))
			node->ptr9 = mpcounter++;
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		break;

	case EMPTSEMI:
		if (! (node->info & SHAPEKNOWN))
			node->ptr4 = mpcounter++;
		break;

	case ENCODE:
		lrinf(LEFT,RIGHT);
		break;

	case EPSILON:
		LEFT->index = node->index;
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		node->ptr1 = rescounter++;
		node->ptr2 = mpcounter++;
		break;

	case FIDENT:
	case SYSVAR:
		if (LEFT != NILP)
			doinf(LEFT,0);
		if (RIGHT != NILP)
			doinf(RIGHT,0);
		node->ptr1 = trscounter++;
		node->ptr2 = trscounter++;
		node->ptr3 = trscounter++;
		node->ptr4 = mpcounter++;
		node->ptr5 = rescounter++;
		break;

	case FORMAT:
		doinf(RIGHT, 0);
		break;

	case GO:
		RIGHT->index = icounter++;
		doinf(RIGHT,0);
		node->ptr1 = icounter++;
		break;

	case INDEX:
		RIGHT->index = node->index;
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		node->ptr1 = icounter++;
		node->ptr2 = rescounter++;
		node->ptr3 = mpcounter++;
		break;

	case INNER:
		RIGHT->index = node->index;
		doinf(RIGHT, 0);
		node->ptr1 = RIGHT->ptr1;
		node->ptr2 = icounter++;
		node->ptr3 = icounter++;
		node->ptr4 = icounter++;
		node->ptr6 = rescounter++;
		break;

	case IOTA:
		doinf(RIGHT,0);
		node->ptr1 = mpcounter++;
		if (node->info & SEQUENTIAL)
			node->ptr2 = icounter++;
		break;

	case LAM:
		break;

	case MSOP:
		RIGHT->index = node->index;
		doinf(RIGHT,0);
		if (! (node->info & TYPEKNOWN))
			node->ptr1 = icounter++;
		node->ptr2 = rescounter++;
		break;

	case OUTER:
		LEFT->index = icounter++;
		RIGHT->index = icounter++;
		doinf(RIGHT, 0);
		doinf(LEFT, 0);
		if (! (node->info & SHAPEKNOWN))
			node->ptr1 = mpcounter++;
		node->ptr2 = icounter++;
		node->ptr4 = rescounter++;
		node->ptr5 = rescounter++;
		if (! (node->info & RANKKNOWN))
			node->ptr6 = icounter++;
		break;

	case RAVEL:
		RIGHT->index = node->index;
		doinf(RIGHT,0);
		node->ptr1 = mpcounter++;
		node->ptr2 = icounter++;
		break;

	case REDUCE:

		doaxis(node);

		RIGHT->index = icounter++;
		doinf(RIGHT,0);
		node->ptr1 = mpcounter++;
		node->ptr2 = icounter++;
		node->ptr3 = icounter++;
		node->ptr4 = icounter++;
		node->ptr5 = rescounter++;
		if (! (node->info & TYPEKNOWN))
			node->ptr6 = icounter++;
		if (! (node->info & RANKKNOWN))
			node->ptr7 = icounter++;
		node->ptr8 = icounter++;
		node->ptr9 = icounter++;
		break;

	case REVERSE:
		doaxis(AXIS);
		mergeresource(node, RIGHT);
		node->ptr9 = icounter++;
		doinf(RIGHT,0);
		break;

	case RESHAPE:
		doinf(LEFT, 0);
		if ((node->info & SHAPEKNOWN) && 
			(RIGHT->info & SHAPEKNOWN) &&
			(numin(RIGHT) >= numin(node))) {
			RIGHT->index = node->index;
			if (node->info & SEQUENTIAL) {
				RIGHT->info |= SEQUENTIAL;
				/* this situation occurs often enough to
				make it a worthwhile special case */
				if (RIGHT->nodetype == IOTA)
					node->info |= NOINDEX;
				}
			}
		else {
			RIGHT->index = icounter++;
			node->ptr1 = icounter++;
			}
		doinf(RIGHT, 0);
		break;

	case RHO:
		doinf(RIGHT,0);
		node->ptr1 = mpcounter++;
		node->ptr2 = mpcounter++;
		break;

	case RHORHO:
		doinf(RIGHT,0);
		node->ptr1 = icounter++;
		break;

	case ROLL:
		doinf(RIGHT,0);
		break;

	case ROTATE:
		doaxis(node);
		node->ptr1 = icounter++;
		node->ptr2 = icounter++;
		node->ptr3 = icounter++;
		LEFT->index = icounter++;
		if (is_scalar(RIGHT) || is_vector(RIGHT))
			RIGHT->index = node->ptr3;
		else
			RIGHT->index = icounter++;
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		break;

	case SCAN:
		doaxis(node);
		doinf(RIGHT, 0);
		node->ptr1 = icounter++;
		if (! (RIGHT->info & SEQUENTIAL))
			node->ptr2 = icounter++;
		node->ptr3 = icounter++;
		node->ptr5 = icounter++;
		if (RIGHT->info & SEQUENTIAL)
			RIGHT->index = node->index;
		else if (is_vector(RIGHT))
			RIGHT->index = node->ptr5;
		else
			RIGHT->index = icounter++;
		node->ptr6 = rescounter++;
		if (! (node->info & TYPEKNOWN))
			node->ptr7 = icounter++;
		break;

	case SM:
		if (LEFT == NILP)
			RIGHT->index = node->index;
		else {
			LEFT->ptr3 = node->ptr3;
			LEFT->index = icounter++;
			doinf(LEFT, 0);
			RIGHT->index = icounter++;
			}
		doinf(RIGHT, 0);
		node->ptr2 = icounter++;
		if (! (node->info & RANKKNOWN))
			node->ptr4 = icounter++;
		if (! (node->info & SHAPEKNOWN))
			node->ptr5 = mpcounter++;
		node->ptr6 = rescounter++;
		break;

	case SORT:
		doinf(RIGHT, 0);
		node->ptr1 = rescounter++;
		node->ptr2 = mpcounter++;
		node->ptr3 = mpcounter++;
		break;

	case SUB:
		LEFT->index = RIGHT->ptr3 = icounter++;
		RIGHT->index = node->index;
		doinf(LEFT,0);
		doinf(RIGHT,1);
		break;

	case SUBASSIGN:
		doinf(RIGHT,0);
		doinf(AXIS,1);
		break;

	case DROP:
	case TAKE:
		mergeresource(node, RIGHT);
		if (! (node->info & SHAPEKNOWN))
			node->ptr9 = mpcounter++;
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		break;

	case TRANS:
		mergeresource(node, RIGHT);
		if (! (node->info & SHAPEKNOWN))
			node->ptr9 = mpcounter++;
		doinf(RIGHT, 0);
		break;
	}
}
