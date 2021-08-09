/*
	APL Compiler

	utilities used in code generation
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
extern struct node *zeronode;

/* copychild - copy information for a child node into the parent node */
copychild(node, child, type, rank, shape)
struct node *node, *child;
int type, rank, shape;
{
	if (type && ! (node->info & TYPEKNOWN))
		node->type.c = child->type.c;
	if (rank && ! (node->info & RANKKNOWN))
		node->rank.c = child->rank.c;
	if (shape && ! (node->info & SHAPEKNOWN))
		node->shape.c = child->shape.c;
}

/* gcant_happen - something that can't happen just did */
gcant_happen(n)
int n;
{
	fprintf(stderr,"during code generation, impossible condition %d\n",
		n);
	exit(1);
}

/* rtype - return the constant known type of a node */
int rtype(node)
struct node *node;
{
	if (! (node->info & TYPEKNOWN))
		return(UKTYPE);
	if ((node->type.c)->cop != tcnst)
		gcant_happen(10);
	return((node->type.c)->c0.cindex);
}

/* rankvalue - return the constant known rank of a node */
int rankvalue(node)
struct node *node;
{
	if (! (node->info & RANKKNOWN))
		gcant_happen(111);
	if ((node->rank.c)->cop != icnst)
		gcant_happen(10);
	return((node->rank.c)->c0.cindex);
}

/* shapevalue - return the constant known shape of a node */
int shapevalue(node)
struct node *node;
{	struct codetree *child;

	if (! (node->info & SHAPEKNOWN))
		gcant_happen(112);
	if ((node->shape.c)->cop != cnst)
		gcant_happen(10);
	child = (node->shape.c)->c0.cleft;
	if (child->cop != icnst)
		gcant_happen(1112);
	return(child->c0.cindex);
}

/* valvalue - return the constant offset of a known value */
int valvalue(node)
struct node *node;
{	struct codetree *child;

	if (! (node->info & VALUESKNOWN))
		gcant_happen(113);
	if ((node->values.c)->cop != cnst)
		gcant_happen(10);
	child = (node->shape.c)->c0.cleft;
	if (child->cop != icnst)
		gcant_happen(1112);
	return(child->c0.cindex);
}

/* is_scalar - return true if a node is a known scalar */
int is_scalar(node)
struct node *node;
{
	if ((node->info & RANKKNOWN) && (rankvalue(node) == 0))
		return(1);
	return(0);
}

/* is_vector - return true is a node is a known vector */
int is_vector(node)
struct node *node;
{
	if ((node->info & RANKKNOWN) && (rankvalue(node) == 1))
		return(1);
	return(0);
}

/* adjdcls - adjust declarations to tree form */
adjdcls(node)
struct node *node;
{

	if (node->info & TYPEKNOWN) {
		node->type.c = gicn(tcnst, node->type.n, INT);
		}
	if (node->info & RANKKNOWN) {
		node->rank.c = gicn(icnst, node->rank.n, INT);
		}
	if (node->info & SHAPEKNOWN) {
		node->shape.c = 
			gcast(cnst, gicn(icnst, node->shape.n, INT), INT);
		}
	if (node->info & VALUESKNOWN) {
		node->values.c = 
			gcast(cnst, gicn(icnst, node->values.n, INT), rtype(node));
		}
}

/* resinreg - make sure a value is in a register, putting it there if
necessary.  Return the number of the register it is in */
int resinreg(node, regval)
struct node *node;
int regval;
{	struct codetree *tree;

	tree = node->values.c;
	if (tree->cop == resptr)
		return(tree->c0.cindex);
	else {	/* put into register */
		ctgen(gbin(asgn, 
			gicn(resptr, regval, rtype(node)),
			node->values.c));
		seminl();
		node->values.c = gicn(resptr, regval, rtype(node));
		}
	return(regval);
}

/* ksize - get the size of a known shape node */
int ksize(node)
struct node *node;
{	int rank, pos, size, j;

	rank = rankvalue(node);
	pos = shapevalue(node);
	size = 1;
	for (j = rank-1; j >= 0; j--)
		size *= iconsts[pos + j];
	return(size);
}

/* getsize - get the size of the left hand node */
getsize(i, node)
int i;
struct node *node;
{	int rank;

	if ((node->info & RANKKNOWN) && 
		((node->rank.c)->cop == icnst)) {
		rank = (node->rank.c)->c0.cindex;
		if (rank == 0) {
			seticon(i, 1);
			return;
			}
		if (node->info & SHAPEKNOWN) {
			seticon(i, ksize(node));
			return;
			}
		if (rank == 1) {
			ieq(i);
			ctgen(gmon(deref, node->shape.c));
			seminl();
			return;
			}
		}
	printf("i%d = vsize(", i);
	rns(node);
	rpseminl();
}

/* fixzero - make an uninitialized zero node */
fixzero()
{
	zeronode->nodetype = ICON;
	zeronode->info = (TYPEKNOWN | RANKKNOWN | SHAPEKNOWN | VALUESKNOWN);
	zeronode->type.c = gicn(tcnst, INT, INT);
	zeronode->rank.c = gicn(icnst, 0, INT);
	zeronode->shape.c = gcast(cnst, gicn(icnst, 1, INT), INT);
	zeronode->values.c = gicn(icnst, 0, INT);
}

/* cpshape - copy a shape vector into a new vector */
cpshape(memval, node)
int memval;
struct node *node;
{
	if (is_vector(node)) {
		printf("*mp%d.ip = ", memval);
		ctgen(gmon(deref, node->shape.c));
		seminl();
		}
	else {
		printf("cpvec(mp%d.ip, ", memval);
		rns(node);
		rpseminl();
		}
}

/* Mktmatch - make sure a given type matches another node type */
mktmatch(want, have, reg)
struct node *want, *have;
int reg;
{
	if (want->info & TYPEKNOWN)
		mkrktype(have, rtype(want), reg);
	else {
		reg = resinreg(have, reg);
		if (! cteq(want->type.c, have->type.c)) {
			printf("cktype(&res%d, ", reg);
			ctgen(want->type.c);
			commasp();
			ctgen(have->type.c);
			rpseminl();
			}
		have->values.c = gicn(resptr, reg, UKTYPE);
		}
}
