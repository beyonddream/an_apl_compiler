/*
	APL Compiler

	Code generation routines having to do with collecting values
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
# include "y.tab.h"
# include <stdio.h>

extern struct codetree *looprank;

/* colloop - make a simple collection loop */
colloop(node, indexval, maxval)
struct node *node;
int indexval, maxval;
{
	if (is_scalar(node)) {
		if (! (node->info & NOINDEX))
			setizero(indexval);
		printf("{\n");
		}
	else {
		iiloop(indexval, maxval);
		}
}

/*
	gencsclar - generate code for collecting an integer scalar
*/
gencscalar(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - register for result, if needed
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			if (node->info & VALUESKNOWN) {
				node->values.c = 
					gicn(icnst, 
						iconsts[valvalue(node)], INT);
				return;
				}
			switchbox(RIGHT, SHAPE, 0);
			if (! (RIGHT->info & TYPEKNOWN))
				testtype(RIGHT->type.c, INT);
			if (! (RIGHT->info & RANKKNOWN))
				testrank(RIGHT->rank.c, 0);
			if (RIGHT->info & HAVEVALUE) {
				if ((RIGHT->values.c)->cop == memptr)
					RIGHT->values.c->c2.ctype = INT;
				node->values.c = 
					gmon(deref, RIGHT->values.c);
				}
			else {
				if (! (RIGHT->info & NOINDEX))
					setizero(RIGHT->index);
				switchbox(RIGHT, VALUE, 0);
				mkrktype(RIGHT, INT, node->ptr1);
				node->values.c = RIGHT->values.c;
				}
			break;
		
		case FINISH:
			if (! (node->info & VALUESKNOWN))
				switchbox(RIGHT, FINISH, 0);
			break;
	}
}

/*
	gencollect - generate code for collecting values
		used by 
		civec - collect integer vector
		cvec - collect vectors
		collect - collect arbitrary objects
*/
gencollect(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{	int save;
	struct codetree *savetree;
/*
	ptr1 - size of right
	ptr3 - mp for values
	ptr4 - mp for results (set to same as ptr3)
	ptr5 - result register
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(RIGHT, SHAPE, 0);
			copychild(node, RIGHT, 1, 1, 1);
			if ((node->nodetype == CIVEC) &&
				(! (RIGHT->info & TYPEKNOWN)))
				testtype(RIGHT->type.c, INT);
			if ((node->nodetype == CVEC) || 
				(node->nodetype == CIVEC))
				if (! (RIGHT->info & RANKKNOWN))
					testrank(RIGHT->rank.c, 1);
			if (node->info & VALUESKNOWN) return;

			if (RIGHT->info & HAVEVALUE) {
				node->values.c = RIGHT->values.c;
				}
			else {
				getsize(node->ptr1, RIGHT);
				savetree = looprank;
				looprank = RIGHT->rank.c;
				impalloc(node->ptr3, node->ptr1);
				mpeqmp(node->ptr4, node->ptr3);
				colloop(RIGHT, RIGHT->index, node->ptr1);
				switchbox(RIGHT, VALUE, 0);
				save = node->index;
				node->index = RIGHT->index;
				asntchk(1, node, RIGHT, rtype(node), 
						node->ptr5, node->ptr3);
				node->index = save;
				rbr();
				node->values.c = 
					gicn(memptr, node->ptr4, rtype(node));
				looprank = savetree;
				}
			break;

		case VALUE:
			leafvalue(node, node->ptr4, 
				rtype(node), node->ptr5);
			break;

		case FINISH:
			if (node->info & VALUESKNOWN)
				break;
			if (! (RIGHT->info & HAVEVALUE))
				mpfree(node->ptr4);
			switchbox(RIGHT, FINISH, 0);
			break;
	}
}

/*
	genreshape - generate code for reshape (dyadic rho) operator
*/
genreshape(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - size of right hand side
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			if (! (node->info & SHAPEKNOWN)) {
				switchbox(LEFT, SHAPE, 0);
				if (! (node->info & RANKKNOWN))
					node->rank.c = 
						gmon(deref, LEFT->shape.c);
				node->shape.c = LEFT->values.c;
				}
			switchbox(RIGHT, SHAPE, 0);
			if (! (node->info & TYPEKNOWN))
				node->type.c = RIGHT->type.c;
			if ((node->index != RIGHT->index) && 
				(! (RIGHT->info & NOINDEX)))
				getsize(node->ptr1, RIGHT);
			break;

		case VALUE:
			if ((node->index != RIGHT->index) && 
				(! (RIGHT->info & NOINDEX)))
				iopi(RIGHT->index, node->index, "%",
					node->ptr1);
			switchbox(RIGHT, VALUE, 0);
			node->values.c = RIGHT->values.c;
			break;

		case FINISH:
			if (! (node->info & SHAPEKNOWN))
				switchbox(LEFT, FINISH, 0);
			switchbox(RIGHT, FINISH, 0);
			break;
	}
}
