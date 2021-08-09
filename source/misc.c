/*
	APL Compiler

	Code Generation routines for misc. functions
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

/*
	geniota - generate code for monadic iota operator
*/
geniota(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - mp for shape
	ptr2 - value if sequential
	ptr3 - flag (see code)
*/
	switch(mode) {
case SHAPE:
	adjdcls(node);
	if (! (node->info & SHAPEKNOWN)) {
		switchbox(RIGHT, SHAPE, 0);
		if ((RIGHT->info & HAVEVALUE) &&
		    ((RIGHT->values.c)->cop == deref)) {
			node->ptr3 = 0;
			/* strip off deref */
			node->shape.c = (RIGHT->values.c)->c0.cleft;
			}
		else {
			node->ptr3 = 1;
			smpalloc(node->ptr1);
			smpieqt(node->ptr1, RIGHT->values.c);
			node->shape.c = gicn(memptr, node->ptr1, INT);
			}
		}
	if (node->info & SEQUENTIAL)
		ieqtree(node->ptr2, gixorg());
	break;

case VALUE:
	if (node->info & SEQUENTIAL)
		node->values.c = gmon(postinc, gicn(iptr, node->ptr2, 0));
	else
		node->values.c = gsop(PLUS, gicn(iptr, node->index, 0),
			gixorg());
	break;

case FINISH:
	if (! (node->info & SHAPEKNOWN)) {
		switchbox(RIGHT, FINISH, 0);
		if (node->ptr3)
			mpfree(node->ptr1);
		}
	break;
	}
}

/*
	genravel - generate code for ravel instruction
*/
genravel(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - memory pointer for shape
	ptr2 - integer value for getsize
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(RIGHT, SHAPE, 0);
			if (! (node->info & TYPEKNOWN))
				node->type.c = RIGHT->type.c;
			if (! (node->info & SHAPEKNOWN)) {
				smpalloc(node->ptr1);
				getsize(node->ptr2, RIGHT);
				setmptoi(node->ptr1, node->ptr2);
				node->shape.c = gicn(memptr, node->ptr1,
					INT);
				}
			if (RIGHT->info & HAVEVALUE)
				node->values.c = RIGHT->values.c;
			break;

		case VALUE:
			switchbox(RIGHT, VALUE, 0);
			node->values.c = RIGHT->values.c;
			break;

		case FINISH:
			switchbox(RIGHT, FINISH, 0);
			if (! (node->info & SHAPEKNOWN)) 
				mpfree(node->ptr1);
	}
}
/*
	genavec -
		generate code for atomic vector
*/
genavec(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
	switch (mode) {
		case SHAPE:
			adjdcls(node);
			break;

		case VALUE:
			node->values.c = gicn(iptr, node->index, INT);
			break;
		}
}

/*
	genroll - generate code for random operation
*/
genroll(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - result for left hand side
	ptr2 - i value
*/
	switch (mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(RIGHT, SHAPE, 0);
			copychild(node, RIGHT, 0, 1, 1);
			break;

		case VALUE:
			switchbox(RIGHT, VALUE, 0);
			node->ptr1 = resinreg(RIGHT, node->ptr1);
			ieq(node->ptr2);
			printf("randint(res%d.i);\n", node->ptr1);
			node->values.c = gicn(iptr, node->ptr2, INT);
			break;

		case FINISH:
			switchbox(RIGHT, FINISH, 0);
			break;
	}
}

/*
	gendeal -
		generate code for deal operation
*/
gendeal(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - mp for shape
	ptr2 - values of deal
	ptr3 - results
*/
	switch (mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(LEFT, SHAPE, 0);
			switchbox(RIGHT, SHAPE, 0);
			if (! (node->info & SHAPEKNOWN)) {
				smpalloc(node->ptr1);
				smpieqt(node->ptr1, LEFT->values.c);
				node->shape.c = 
					gicn(memptr, node->ptr1, INT);
				}
			printf("deal(&mp%d, ", node->ptr2);
			ctgen(LEFT->values.c);
			commasp();
			ctgen(RIGHT->values.c);
			seminl();
			node->values.c = gicn(memptr, node->ptr2, INT);
			break;

		case VALUE:
			leafvalue(node, node->ptr2, INT, node->ptr3);
			break;
	}
}
/*
	gensub - generate code for subscripting operations
*/
gensub(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(LEFT, SHAPE, 0);
			RIGHT->a.axis = LEFT;
			switchbox(RIGHT, SHAPE, 1);
			copychild(node, LEFT, 1, 0, 0);
			copychild(node, RIGHT, 0, 1, 1);
			break;

		case VALUE:
			switchbox(RIGHT, VALUE, 1);
			ieqtree(LEFT->index, RIGHT->values.c);
			switchbox(LEFT, VALUE, 0);
			node->values.c = LEFT->values.c;
			break;

		case FINISH:
			switchbox(LEFT, FINISH, 0);
			switchbox(RIGHT, FINISH, 0);
			break;
	}
}
/*
	subgen -
		generate the shape of the current position 
*/
static struct codetree *subgen(parent, position)
struct node *parent;
int position;
{
	return(gmon(deref, gsop(PLUS, parent->shape.c,
		gicn(icnst, position, INT))));
}
/*
	gensemi - generate code for semicolon
			(this should be optimized a little more)
*/
gensemi(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - position (set by parser)
	ptr2 - size of right hand side
	ptr3 - index of top (set by sub)
	ptr4 - rank
	ptr5 - shape
	ptr6 - result of children
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			if (LEFT != NILP) {
				LEFT->a.axis = node->a.axis;
				switchbox(LEFT, SHAPE, 0);
				}
			if (RIGHT->nodetype == EMPTSEMI)
				RIGHT->a.axis = node->a.axis;
			switchbox(RIGHT, SHAPE, 0);
			if (LEFT == NILP)
				copychild(node, RIGHT, 0, 1, 1);
			else if (! (node->info & SHAPEKNOWN)) {
				rkeq(node, node->ptr4);
				printf("outershape(&mp%d, ", node->ptr5);
				lrnsrrns(LEFT, RIGHT);
				rpseminl();
				node->shape.c = gicn(memptr, node->ptr5, INT);
				}
			if (LEFT != NILP)
				getsize(node->ptr2, RIGHT);
			break;

		case VALUE:
			if (LEFT == NILP) {
				switchbox(RIGHT, VALUE, 0);
				mkrktype(RIGHT, INT, node->ptr6);
				if (RIGHT->nodetype == EMPTSEMI)
					node->values.c = RIGHT->values.c;
				else
					node->values.c = 
						gsop(MINUS, RIGHT->values.c,
						gixorg());
				}
			else {
				if (! (RIGHT->info & NOINDEX))
					iopi(RIGHT->index, node->index, 
						"%", node->ptr2);
				switchbox(RIGHT, VALUE, 0);
				mkrktype(RIGHT, INT, node->ptr6);
				if (! (LEFT->info & NOINDEX))
					iopi(LEFT->index, node->index, 
						"/", node->ptr2);
				switchbox(LEFT, VALUE, 0);
				node->values.c =
				gsop(PLUS,
				gsop(TIMES, LEFT->values.c,
				subgen(node->a.axis, node->ptr1)),
				RIGHT->values.c);
				if (RIGHT->nodetype == EMPTSEMI)
					;
				else
					node->values.c =
					gsop(MINUS, node->values.c,
					gixorg());
				}
			break;

		case FINISH:
			if (LEFT != NILP)
				switchbox(LEFT, FINISH, 0);
			switchbox(RIGHT, FINISH, 0);
			if (! (node->info & SHAPEKNOWN))
				mpfree(node->ptr5);
	}
}

/*
	genempt - generate code for the empty semicolon position
*/
genempt(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr4 - shape
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			if (! (node->info & SHAPEKNOWN)) {
				smpalloc(node->ptr4);
				smpieqt(node->ptr4, 
					subgen(node->a.axis, node->ptr1));
				node->shape.c = gicn(memptr, node->ptr4, INT);
				}
			break;

		case VALUE:
			node->values.c = gicn(iptr, node->index, INT);
			break;

		case FINISH:
			if (! (node->info & SHAPEKNOWN))
				mpfree(node->ptr4);
			break;
	}
}
/*
	gengo -
		generate code for unconditional goto
*/
gengo(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{	struct codetree *savetree;
/*
	ptr1 - size of right side
	ptr2 = result value
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(RIGHT, SHAPE, 0);
			getsize(node->ptr1, RIGHT);
			printf("if (i%d > 0) {\n", node->ptr1);
			savetree = looprank;
			looprank = RIGHT->rank.c;
			if (! (RIGHT->info & NOINDEX))
				seticon(RIGHT->index, 0);
			switchbox(RIGHT, VALUE, 0);
			mkrktype(RIGHT, INT, node->ptr2);
			printf("stmtno = ");
			ctgen(RIGHT->values.c);
			seminl();
			printf("break;\n");
			rbr();
			looprank = savetree;
			break;

		case FINISH:
			switchbox(RIGHT, FINISH, 0);
			break;
	}
}
