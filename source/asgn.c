/*
	APL Compiler

	code generation for assignment and I/O statements
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

/* looprank is the rank of the object currently being looped over */
extern struct codetree *looprank;

/* asntchk - do assignment statement type checking */
asntchk(gen, dest, source, destype, regval, mpval)
int gen, destype, regval, mpval;
struct node *dest, *source;
{
	if (destype != UKTYPE) {	/* known target type */
		mkrktype(source, destype, regval);
		if (is_scalar(source))
			dest->values.c = gbin(asgn,
				gmon(deref, gicn(memptr, mpval, destype)),
				source->values.c);
		else
			dest->values.c = gbin(asgn,
				gmon(deref, gmon(postinc,
				gicn(memptr, mpval, destype))),
				source->values.c);
		if (gen) {
			ctgen(dest->values.c);
			seminl();
			}
		}
	else if (rtype(source) != UKTYPE) {
		if (is_scalar(source))
			dest->values.c = gbin(asgn,
				gmon(deref, gicn(memptr, mpval, rtype(source))),
				source->values.c);
		else
			dest->values.c = gbin(asgn,
				gmon(deref, gmon(postinc,
				gicn(memptr, mpval, rtype(source)))),
				source->values.c);
		if (gen) {
			ctgen(dest->values.c);
			seminl();
			}
		}
	else {		/* don't know, and don't care, about type */
		regval = resinreg(source, regval);
		printf("setmp(&res%d, &mp%d, i%d, ", regval, mpval,
			dest->index);
		ctgen(dest->type.c);
		rpseminl();
		dest->values.c = gicn(resptr, regval, UKTYPE);
		}
}

/*genassign - generate code for assignment statements */
genassign(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{	struct codetree *savetree;
/*
	ptr1 = memory pointer
	ptr2 = trs register pointer
	ptr3 = size of right hand side
	ptr4 = result register
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(RIGHT, SHAPE, 0);
			copychild(node, RIGHT, 1, 1, 1);
			if (! (node->info & TYPEKNOWN)) {
				if (LEFT->info & TYPEDECL)
					testtype(node->type.c, LEFT->type.n);
				}
			if (! (node->info & RANKKNOWN)) {
				if (LEFT->info & RANKDECL)
					testrank(node->rank.c, LEFT->rank.n);
				}

			if (RIGHT->nodetype == FIDENT) {
				node->ptr2 = RIGHT->ptr3;
				break;
				}

			settrs(node->ptr2, node);
			savetree = looprank;
			looprank = node->rank.c;

			if (! (node->info & TYPEKNOWN))
				node->type.c = 
					gtrs(node->ptr2, ctypefield, UKTYPE);
			if (! (node->info & RANKKNOWN))
				node->rank.c = 
					gtrs(node->ptr2, crankfield, UKTYPE);
			if (! (node->info & SHAPEKNOWN))
				node->shape.c = 
					gtrs(node->ptr2, cshapefield, UKTYPE);

			ieq(node->ptr3);
			printf("talloc(&trs%d);\n", node->ptr2);
			node->values.c = 
				gtrs(node->ptr2, cvalfield, rtype(node));
			leafshape(node, node->ptr1);

			if (top || ! (node->info & SEQUENTIAL)) {
				colloop(RIGHT, RIGHT->index, node->ptr3);
				switchbox(RIGHT, VALUE, 0);
				asntchk(1, node, RIGHT, LEFT->type.n, 
					node->ptr4, node->ptr1);
				rbr();
				if (! (node->info & SEQUENTIAL)) {
					node->values.c = 
						gtrs(node->ptr2, 
							cvalfield,
							rtype(node));
					leafshape(node, node->ptr1);
					}
				}
			looprank = savetree;
			break;

		case VALUE:
			if (RIGHT->nodetype == FIDENT) {
				switchbox(RIGHT, VALUE, 0);
				node->values.c = RIGHT->values.c;
				}
			else if (node->info & SEQUENTIAL) {
				switchbox(RIGHT, VALUE, 0);
				asntchk(0, node, RIGHT, LEFT->type.n, 
					node->ptr4, node->ptr1);
				}
			else
				leafvalue(RIGHT, node->ptr1, rtype(node),
					node->ptr4);
			break;

		case FINISH:
			printf("assign(%c%s, &trs%d);\n", 
				(is_parm(LEFT->a.namep) ? ' ' : '&'),
				LEFT->a.namep, node->ptr2);
			switchbox(RIGHT, FINISH, 0);
			break;
		}
}

/*genbassign - box assignment (output) */
genbassign(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{	struct codetree *savetree;
/*
	ptr3 = size of right hand size
	ptr4 = result register
*/
	switch(mode) {
		case SHAPE:
			switchbox(RIGHT, SHAPE, 0);
			if (! is_scalar(RIGHT)) {
				getsize(node->ptr3, RIGHT);
				}
			savetree = looprank;
			looprank = RIGHT->rank.c;
			colloop(RIGHT, RIGHT->index, node->ptr3);
			switchbox(RIGHT, VALUE, 0);
			node->ptr2 = resinreg(RIGHT, node->ptr4);
			printf("printit(&res%d, ", node->ptr2);
			ctgen(RIGHT->type.c);
			if (is_scalar(RIGHT))
				printf(", 1");
			else {
				printf(", bmpnl(i%d, ", RIGHT->index);
				rns(RIGHT);
				rp();
				}
			rpseminl();
			rbr();
			looprank = savetree;
			break;

		case FINISH:
			switchbox(RIGHT, FINISH, 0);
			break;
		}
}
