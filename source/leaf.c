/*
	APL Compiler

	code generation for leafs
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

extern int *iconsts;

struct codetree *looprank = 0;	/* rank of current loop */

char *lastvar = "";		/* see genident */

static char *mpfields[] = {"unknown", "ip", "ip", "rp", "cp", "any",
	"label"};

/* leafshape - do common shape operations for leafs */
leafshape(node, mpval)
struct node *node;
int mpval;
{
	if (rtype(node)== UKTYPE) {
		printf("mp%d = ", mpval);
		}
	else {
		printf("mp%d.%s = ", mpval, mpfields[rtype(node)]);
		}
	ctgen(node->values.c);
	seminl();
	node->values.c = gicn(memptr, mpval, rtype(node));
}
	
/* leafvalue - do common value operations for leaves */
leafvalue(node, mpval, mptype, resval)
struct node *node;
int mpval, mptype, resval;
{	

	if (mptype == UKTYPE) {
		printf("getmp(&res%d, &mp%d, ", resval, mpval);
		if (is_scalar(node))
			printf("0");
		else if ((node->info & RANKKNOWN) ||
				cteq(node->rank.c, looprank))
			printf("i%d", node->index);
		else {
			printf("((");
			ctgen(node->rank.c);
			printf(") ? i%d : 0)", node->index);
			}
		commasp();
		ctgen(node->type.c);
		rpseminl();
		node->values.c = gicn(resptr, resval, UKTYPE);
		}
	else {	/* type known, can generate better code */
		if (is_scalar(node))
			node->values.c = gmon(deref, node->values.c);
		else if ((node->info & RANKKNOWN) || 
			cteq(node->rank.c, looprank)) {
			/* if rank is known but not scalar, must be
			nonscalar */
			if (node->info & SEQUENTIAL)
				node->values.c = gmon(deref, 
					gmon(postinc, node->values.c));
			else 
				node->values.c = gmon(deref,
					gsop(PLUS, node->values.c,
					gicn(iptr, node->index, 0)));
			}
		else {
				/* have to make sure it is nonscalar */
				node->values.c = gmon(deref,
					gsop(PLUS, node->values.c,
					gcond(node->rank.c,
					gicn(iptr, node->index, 0),
					gicn(icnst, 0, 0))));
			}
		
	}
}

/*genconst - generate code for constant nodes */

genconst(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 = memory pointer
	ptr2 = result register
	ptr3 = constant type
	ptr4 = scalar integer constant value
*/
	switch(mode) {
		case SHAPE:
			node->ptr3 = node->type.n;
			node->ptr4 = node->values.n;
			adjdcls(node);
			if (node->nodetype == LCON)
				(node->values.c)->c2.ctype = LABEL;
			if (is_scalar(node) && is_icon(node))
				node->ptr4 = iconsts[node->ptr4];
			else {
			/* only put it into a mp if we have to */
			if (rtype(node) == UKTYPE)
				leafshape(node, node->ptr1);
			else if ((node->info & SEQUENTIAL) &&
				 ! is_scalar(node))
				leafshape(node, node->ptr1);
			/* otherwise might as well use given value */
			}
			break;

		case VALUE:
			if (is_scalar(node) && is_icon(node))
				node->values.c = gicn(icnst, node->ptr4, 0);
			else
				leafvalue(node, node->ptr1, 
					node->ptr3, node->ptr2);
			break;

		case FINISH:
			break;
		}
}

/*genident - generate code for identifier nodes */
genident(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 = memory pointer
	ptr2 = result register
*/

	switch(mode) {
		case SHAPE:
			adjdcls(node);

			if (strcmp(node->a.namep, lastvar)) {
				/* by remembering the name we prevent
				two checks immediately adjacent */
				lastvar = node->a.namep;
				printf("if (%s%stype == UKTYPE) ", 
					lastvar,
					(is_parm(lastvar) ? "->" : "."));
				prerror("undefined value used");
				}

			if (! (node->info & TYPEKNOWN))
				node->type.c = 
					gident(node->a.namep, ctypefield,
						UKTYPE);
			if (! (node->info & RANKKNOWN))
				node->rank.c = 
					gident(node->a.namep, crankfield,
						UKTYPE);
			if (! (node->info & SHAPEKNOWN))
				node->shape.c = 
					gident(node->a.namep, cshapefield,
						UKTYPE);
			node->values.c = gident(node->a.namep, cvalfield,
				rtype(node));

			/* only put it into a mp if we have to */
			if (rtype(node) == UKTYPE)
				leafshape(node, node->ptr1);
			else if ((node->info & SEQUENTIAL) &&
				 ! is_scalar(node))
				leafshape(node, node->ptr1);
			/* otherwise might as well use given value */

			break;

		case VALUE:
			leafvalue(node, node->ptr1, rtype(node), node->ptr2);
			break;

		case FINISH:
			break;
		}
}

/*
	genfun - generate code for function calls
*/
static char *sysnames[] = {"PP", "PW", "RL", "IO", "TS", "FM", 
"PF", "OP", "EX", "CL", "AG", "MD"};

genfun(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - left trs
	ptr2 - right trs
	ptr3 - result trs
	ptr4 - mp for values
	ptr5 - result values
*/
	switch (mode) {
		case SHAPE:
			adjdcls(node);
			if (LEFT != NILP) {
				switchbox(LEFT, SHAPE, 0);
				filtrs(node->ptr1, LEFT);
				}
			else
				trsuk(node->ptr1);
			if (RIGHT != NILP) {
				switchbox(RIGHT, SHAPE, 0);
				filtrs(node->ptr2, RIGHT);
				}
			else
				trsuk(node->ptr2);
			if (node->nodetype == SYSVAR)
				printf("sysvar(%s, ", 
					sysnames[(int) node->optype]);
			else
				printf("%s(", node->a.namep);
			printf("&trs%d, &trs%d, &trs%d);\n",
				node->ptr3, node->ptr1, node->ptr2);
			if (! (node->info & TYPEKNOWN))
				node->type.c = 
					gtrs(node->ptr3, ctypefield, UKTYPE);
			if (! (node->info & RANKKNOWN))
				node->rank.c =
					gtrs(node->ptr3, crankfield, UKTYPE);
			node->shape.c = gtrs(node->ptr3, cshapefield, UKTYPE);
			node->values.c = 
				gtrs(node->ptr3, cvalfield, rtype(node));
			if (! top)
				/* only put it into a mp if we have to */
				if (rtype(node) == UKTYPE)
					leafshape(node, node->ptr4);
				else if ((node->info & SEQUENTIAL) &&
				 	! is_scalar(node))
					leafshape(node, node->ptr4);
				/* otherwise might as well use given value */
			break;

		case VALUE:
			leafvalue(node, node->ptr4, rtype(node),
			node->ptr5);
			break;

		case FINISH:
			if (LEFT != NILP)
				switchbox(LEFT, FINISH, 0);
			if (RIGHT != NILP)
				switchbox(RIGHT, FINISH, 0);
			break;
	}
}

/*
	genquad -
		generate code for input quad
*/
genquad(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*	
	ptr1 - trs for result
	ptr2 - mp for values
	ptr3 - result register
*/
	switch (mode) {
		case SHAPE:
			printf("quad(stdin, &trs%d);\n", node->ptr1);
			node->type.c = 
				gtrs(node->ptr1, ctypefield, UKTYPE);
			node->rank.c = 
				gtrs(node->ptr1, crankfield, UKTYPE);
			node->shape.c =
				gtrs(node->ptr1, cshapefield, UKTYPE);
			node->values.c =
				gtrs(node->ptr1, cvalfield, UKTYPE);
			leafshape(node, node->ptr2);
			break;

		case VALUE:
			leafvalue(node, node->ptr2, UKTYPE, node->ptr3);
			break;
	}
}
/*
	genrho - generate code for monadic rho (shape) function
*/
genrho(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - memory pointer for shape
	ptr2 = memroy pointer for values
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(RIGHT, SHAPE, 0);
			if (! (node->info & SHAPEKNOWN)) {
				smpalloc(node->ptr1);
				smpieqt(node->ptr1, RIGHT->rank.c);
				node->shape.c = 
					gicn(memptr, node->ptr1, INT);
				}
			node->values.c = RIGHT->shape.c;
			leafshape(node, node->ptr2);
			break;

		case VALUE:
			leafvalue(node, node->ptr2, INT, 0);
			break;

		case FINISH:
			if (! (node->info & SHAPEKNOWN))
				mpfree(node->ptr1);
			switchbox(RIGHT, FINISH, 0);
	}
}

/*
	genrrho - generate code for rho rho instruction (rank)
*/
genrrho(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - value of result
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			if (RIGHT->info & RANKKNOWN)
				seticon(node->ptr1, RIGHT->rank.n);
			else {
				switchbox(RIGHT, SHAPE, 0);
				ieqtree(node->ptr1, RIGHT->rank.c);
				}
			break;

		case VALUE:
			node->values.c = gicn(iptr, node->ptr1, INT);
			break;

		case FINISH:
			if (! (RIGHT->info & RANKKNOWN))
				switchbox(RIGHT, FINISH, 0);
			break;
	}
}
/*
	gensort - generate code for the sort operator
*/
gensort(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*	
	ptr1 - result values
	ptr2 - node values
	ptr3 - mp for right hand side
*/
	switch (mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(RIGHT, SHAPE, 0);
			copychild(node, RIGHT, 0, 0, 1);
			node->ptr3 = smpval(RIGHT, node->ptr3);
			printf("aplsort(&mp%d, &mp%d, ", 
				node->ptr2, node->ptr3);
			ctgen(gmon(deref, RIGHT->shape.c));
			commasp();
			ctgen(RIGHT->type.c);
			printf(", %d);\n", node->a.ptr0);
			node->values.c = gicn(memptr, node->ptr2, INT);
			break;

		case VALUE:
			leafvalue(node, node->ptr2, INT, node->ptr1);
			node->values.c = gsop(PLUS, node->values.c,
					gixorg());
			break;

		case FINISH:
			switchbox(RIGHT, FINISH, 0);
			mpfree(node->ptr2);
	}
}
/*
	genmember - generate code for the membership
				(dyadic epsilon) function 
*/
genmember(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{	int i;
/*
	ptr1 - result value
	ptr2 - values of right hand side
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(LEFT, SHAPE, 0);
			switchbox(RIGHT, SHAPE, 0);
			copychild(node, LEFT, 0, 1, 1);
			node->ptr2 = smpval(RIGHT->right, node->ptr2);
			break;

		case VALUE:
			switchbox(LEFT, VALUE, 0);
			mktmatch(RIGHT->right, LEFT, node->ptr1);
			i = resinreg(LEFT, node->ptr1);
			printf("res%d.i = aplsearch(", node->ptr1);
			ctgen(RIGHT->values.c);
			printf(", &mp%d, &res%d, ", node->ptr2, i);
			ctgen((RIGHT->right)->type.c);
			commasp();
			ctgen(gmon(deref, RIGHT->shape.c));
			printf(") < ");
			ctgen(gmon(deref, RIGHT->shape.c));
			seminl();
			node->values.c = gicn(resptr, node->ptr1, BIT);
			break;

		case FINISH:
			switchbox(LEFT, FINISH, 0);
			switchbox(RIGHT, FINISH, 0);
	}
}
/*
	genindex - generate code for index-of (dyadic iota)
			function
*/
genindex(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{	int i;
/*
	ptr1 - result value (an integer)
	ptr2 - register for right value
	ptr3 - mp for right hand values
*/
	switch (mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(LEFT, SHAPE, 0);
			switchbox(RIGHT, SHAPE, 0);
			copychild(node, RIGHT, 0, 1, 1);
			node->ptr3 = smpval(LEFT->right, node->ptr3);
			break;

		case VALUE:
			switchbox(RIGHT, VALUE, 0);
			mktmatch((LEFT->right), RIGHT, node->ptr2);
			i = resinreg(RIGHT, node->ptr2);
			printf("res%d.i = aplsearch(", node->ptr2);
			ctgen(LEFT->values.c);
			printf(", &mp%d, &res%d, ", node->ptr3, i);
			ctgen((LEFT->right)->type.c);
			commasp();
			ctgen(gmon(deref, LEFT->shape.c));
			rpseminl();
			node->values.c = 
				gsop(PLUS,
				gicn(resptr, node->ptr2, INT),
				gixorg());
			break;

		case FINISH:
			switchbox(LEFT, FINISH, 0);
			switchbox(RIGHT, FINISH, 0);
			break;

	}
}
