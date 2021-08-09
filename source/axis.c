/*
	APL Compiler

	Code Generation routines dealing with axis
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
extern struct node *zeronode;
extern int indxorgin;

/* cksh - generate a test to check shape for non-scalar */
static cksh(i, child, tree)
int i;
struct node *child;
struct codetree *tree;
{
	ieqtree(i, gcond(child->rank.c,
		gmon(deref, tree),
		gicn(icnst, 1, INT)));
}
/*
	axies - generate code to produce the following values

	i - the position of the axis (zero based)
	e - the expansion vector value at position i (e sub i)
	s - the shape of the node at position i (s sub i)

	the argument i, e, and s contain the index registers into which the
	associated values are to be placed.  In all three cases in the
	value is zero no code is generated to place the value.
	One of i, e or s must be non-zero.
*/

axies(node, axis, child, i, e, s)
struct node *node, *axis, *child;
int i, e, s;
{	int r, sh, subi, v;

	if (is_scalar(child)) {
		if (i) seticon(i, 1);
		if (s) seticon(s, 1);
		if (e) seticon(e, 1);
		return;
		}

	if (i && (node->info & FIRSTAXIS)) {
		setizero(i);
		i = 0;
		}

	if (e && (node->info & LASTAXIS)) {
		seticon(e, 1);
		e = 0;
		}

	if (child->info & SHAPEKNOWN) {
		r = rankvalue(child);
		sh = shapevalue(child);
		if (node->info & FIRSTAXIS) {
			if (s) {
				seticon(s, iconsts[sh]);
				s = 0;
				}
			if (e) {
				seticon(e, ksize(child) / iconsts[sh]);
				e = 0;
				}
			}
		else if (node->info & LASTAXIS) {
			if (i) {
				seticon(i, r-1);
				i = 0;
				}
			if (s) {
				seticon(s, iconsts[sh + r-1]);
				s = 0;
				}
			}
		else if ((axis->info & VALUESKNOWN) &&
			(indxorgin != DEFAULTINDEX)) {
			/* get value - compute s and e */
			v = iconsts[axis->values.n] - indxorgin;
			if (i) {
				seticon(i, v);
				i = 0;
				}
			if (s) {
				seticon(s, iconsts[sh + v]);
				s = 0;
				}
			if (e) {
				ieq(e);
				printf("esubi(%d, ", v);
				rns(child);
				rpseminl();
				e = 0;
				}
			}
		}
	if (node->info & FIRSTAXIS) {
		if (s) {
			cksh(s, child, child->shape.c);
			s = 0;
			}
		if (e) {
			ieq(e);
			if ((child->info & RANKKNOWN) && 
				(rankvalue(child) == 2)) {
				ctgen(gmon(deref, gsop(PLUS,
				child->shape.c, gicn(icnst, 1, INT))));
				seminl();
				}
			else {
				printf("esubi(0, ");
				rns(child);
				rpseminl();
				}
			e = 0;
			}
		}
	else if (node->info & LASTAXIS) {
		if (i) {
			if (child->info & RANKKNOWN)
				seticon(i, rankvalue(child) - 1);
			else {
				ieqtree(i, gsop(MINUS, child->rank.c,
					gicn(icnst, 1, INT)));
				}
			if (s) {
				cksh(s, child, 
					gsop(PLUS, child->shape.c,
					gicn(iptr, i, 0)));
				s = 0;
				}
			i = 0;
			}
		if (s) {
			cksh(s, child,
				gsop(PLUS, child->shape.c,
				gsop(MINUS, child->rank.c, 
				gicn(icnst, 1, 0))));
			s = 0;
			}
		}
	else if (i || s || e) {
		switchbox(axis, SHAPE, 0);
		if (i) {
			ieqtree(i, gsop(MINUS, axis->values.c,
				gixorg()));
			subi = i;
			}
		else if ((axis->values.c)->cop == iptr) {
			subi = (axis->values.c)->c0.cindex;
			}
		else {
			subi = e;
			ieqtree(e, gsop(MINUS, axis->values.c,
				gixorg()));
			}
		if (s) {
			cksh(s, child, 
				gsop(PLUS, child->shape.c,
				gicn(iptr, subi, 0)));
			s = 0;
			}
		if (e) {
			ieq(e);
			printf("esubi(i%d, ", subi);
			rns(child);
			rpseminl();
			e = 0;
			}
		switchbox(axis, FINISH, 0);
		}
}
/*
	asubi -
		generate code to produce the i'th element of the
	subscripts corresponding to the given axis.  Note that knowing
	i is not necessary, as e[i] and s[i] encode everything important
*/
asubi(node, child, index, e, s)
struct node *node, *child;
int index, e, s;
{
	if (is_vector(child) || is_scalar(child))
		printf("i%d", index);
	else if (node->info & FIRSTAXIS)
		printf("i%d / i%d", index, e);
	else if (node->info & LASTAXIS)
		printf("i%d %% i%d", index, s);
	else
		printf("(i%d / i%d) % i%d", index, e, s);
}

/*
	redshape = 
		generate code for reduce shape information
		also used by inner product
*/
redshape(node, axis, i, e, s, mpval, rank)
struct node *node, *axis;
int i, e, s, mpval, rank;
{
	if (! (node->info & RANKKNOWN)) {
		ieqtree(rank, gcond(RIGHT->rank.c,
				gsop(MINUS, RIGHT->rank.c,
				gicn(icnst, 1, INT)), RIGHT->rank.c));
		node->rank.c = gicn(iptr, rank, INT);
		}

	if (node->info & LASTAXIS)
		if (is_vector(RIGHT))
			axies(node, axis, RIGHT, 0, 0, s);
		else
			axies(node, axis, RIGHT, 0, 0, s); 
	else if (node->info & FIRSTAXIS)
		axies(node, axis, RIGHT, 0, e, s);
	else
		axies(node, axis, RIGHT, i, e, s);

	if (! (node->info & SHAPEKNOWN)) {
		if (node->info & FIRSTAXIS)
			node->shape.c = gsop(PLUS, RIGHT->shape.c,
					gicn(icnst, 1, INT));
		else if (node->info & LASTAXIS)
			node->shape.c = RIGHT->shape.c;
		else {
			printf("valloc(&mp%d, ", mpval);
			ctgen(node->rank.c);
			printf(", INT);\n");
			printf("cpwo(mp%d.ip, i%d, ", mpval, i);
			rns(RIGHT);
			rpseminl();
			node->shape.c = gicn(memptr, mpval, INT);
			}
		}
}

/*
	cmpidx1 -
		generate the index into the index conversion table for
		compress (and expand)
*/
cmpidx1(node, child, newindex, table, index, e, s)
struct node *node, *child;
int newindex, table, index, e, s;
{
	ieq(newindex);
	printf("*(mp%d.ip + ", table);
	asubi(node, child, index, e, s);
	rpseminl();
}

/*
	cmpidx2 -
		generate the new offset into a structure based on the
		old offset for compression, expansion and catenation.
		here the size of the axis may be changed between the 
		old and new offsets
*/
static cmpidx2(node, temp, newindex, index, a, e, sp, s)
struct node *node;
int temp, newindex, index, a, e, sp, s;
{
	if (node->info & LASTAXIS)
		printf("i%d = (i%d / i%d) * i%d + i%d;\n", 
			newindex, index, s, sp, a);
	else {
		divmod(index, e, temp, newindex);
		printf("i%d += ", newindex);
		if (node->info & FIRSTAXIS)
			printf("i%d * i%d;\n", a, e);
		else
			printf("((i%d / i%d) * i%d + i%d) * i%d;\n",
				temp, s, sp, a, e);
		}
}

/*
	compshape -
		generate shape information for compress
		(also used by expand)
*/
compshape(node, child, mpval, i, sp)
struct node *node, *child;
int mpval, i, sp;
{
	printf("valloc(&mp%d, ", mpval);

	if (is_vector(child)) {
		printf("1, INT);\n");
		setmptoi(mpval, sp);
		}
	else {
		ctgen(child->rank.c);
		printf(", INT);\n");
		cpshape(mpval, child);
		if (node->info & FIRSTAXIS)
			setmptoi(mpval, sp);
		else {
			mpipieq(mpval, i);
			printf("i%d;\n", sp);
			}
		}
	node->shape.c = gicn(memptr, node->ptr8, INT);
}

/*
	comploop -
		do the common loop gathering operatorations of
		compress and expand
*/
comploop(node, i, e, s, mpval, ms, resl, resr)
struct node *node;
int i, e, s, mpval, ms, resl, resr;
{
	switchbox(LEFT, SHAPE, 0);
	switchbox(RIGHT, SHAPE, 0);
	if (! (node->info & TYPEKNOWN))
		node->type.c = RIGHT->type.c;
	if (! (node->info & RANKKNOWN))
		node->rank.c = RIGHT->rank.c;

	if (node->info & LASTAXIS)
		axies(node, node->a.axis, RIGHT, i, 0, s);
	else
		axies(node, node->a.axis, RIGHT, i, e, s);

	if (node->nodetype == EXPAND)
		getsize(ms, LEFT);
	impalloc(mpval, ms);
	colloop(node, LEFT->index, ms);
	switchbox(LEFT, VALUE, 0);
	fixzero();	/* make sure zero is ok */
	dsopv(NE, node, LEFT, zeronode, resl, resl, resr);
}

/*
	gencompress - generate code for compression operator
*/
gencompress(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - axis position
	ptr2 - e sub i
	ptr3 - s sub i
	ptr4 - mp for values of left hand side
	ptr5 - size of left
	ptr6 - result register for computing left
	ptr7 - result register for computing right
	ptr8 - mp for shape
	ptr9 - a sub i (value of indexed element in conversion table)
*/
	switch(mode) {
case SHAPE:
	adjdcls(node);

	setizero(node->ptr5);

	comploop(node, node->ptr1, node->ptr2, node->ptr3,
		node->ptr4, node->ptr3, node->ptr6, node->ptr7);
	iflp();
	ctgen(node->values.c);
	printf(")\n*(mp%d.ip + i%d++) = i%d;\n",
		node->ptr4, node->ptr5, LEFT->index);
	rbr();
	switchbox(LEFT, FINISH, 0);
	if (! (node->info & SHAPEKNOWN))
		compshape(node, RIGHT, node->ptr8, node->ptr1, node->ptr5);
	break;

case VALUE:
	if (! (RIGHT->info & NOINDEX)) {
		/* first compute a prime sub i */
		cmpidx1(node, RIGHT, node->ptr9,
			node->ptr4, node->index, node->ptr2, node->ptr3);
		/* then compute new offset */
		if (! (is_vector(RIGHT) || is_scalar(RIGHT)))
			cmpidx2(node, LEFT->index, RIGHT->index,
				node->index, node->ptr9,
				node->ptr2, node->ptr5, node->ptr3);
		}
	switchbox(RIGHT, VALUE, 0);
	node->values.c = RIGHT->values.c;
	break;

case FINISH:
	if (! (node->info & SHAPEKNOWN))
		mpfree(node->ptr8);
	mpfree(node->ptr4);
	switchbox(RIGHT, FINISH, 0);
	break;
	}
}

/*
	genexpand - generate code for expansion operator
*/
genexpand(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - axis position
	ptr2 - e sub i
	ptr3 - s sub i
	ptr4 - mp for values of left hand side
	ptr5 - size of left
	ptr6 - result register for computing left
	ptr7 - result register for computing right
	ptr8 - mp for shape
	ptr9 - a sub i (value of indexed element in conversion table)
*/
	switch(mode) {
case SHAPE:
	adjdcls(node);

	setizero(RIGHT->index);

	comploop(node, node->ptr1, node->ptr2, node->ptr3,
		node->ptr4, node->ptr5, node->ptr6, node->ptr7);

	mpipieq(node->ptr4, LEFT->index);
	lp();
	ctgen(node->values.c);
	printf(") ? i%d++ : -1;\n", RIGHT->index);
	rbr();
	switchbox(LEFT, FINISH, 0);
	if (! (node->info & SHAPEKNOWN))
		compshape(node, RIGHT, node->ptr8, node->ptr1, node->ptr5);

	if ((node->info & SEQUENTIAL) && (! (RIGHT->info & NOINDEX)))
		setizero(RIGHT->index);
	break;

case VALUE:
	cmpidx1(node, RIGHT, node->ptr9, node->ptr4, node->index, 
		node->ptr2, node->ptr3);
	printf("if (i%d >= 0) {\n", node->ptr9);

	if (node->info & SEQUENTIAL)
		;	/* do nothing now */
	else if (! (RIGHT->info & NOINDEX)) {
		if (! (is_scalar(RIGHT) || is_vector(RIGHT)))
			cmpidx2(node, LEFT->index, RIGHT->index,
				node->index, node->ptr9, node->ptr2,
				node->ptr5, node->ptr3);
		}
	switchbox(RIGHT, VALUE, 0);
	node->ptr6 = resinreg(RIGHT, node->ptr6);

	if (node->info & SEQUENTIAL)
		iincr(RIGHT->index);

	rbr();
	elsebr();
	identity(PLUS, node->ptr6, RIGHT);
	rbr();
	node->values.c = gicn(resptr, node->ptr6,
		rtype(RIGHT));
	break;

case FINISH:
	if (! (node->info & SHAPEKNOWN))
		mpfree(node->ptr8);
	mpfree(node->ptr4);
	switchbox(RIGHT, FINISH, 0);
	break;
	}
}
/*
	gencat - generate code for catenation function
*/

# define non_index(node) ((node->info & SEQUENTIAL) || \
	(node->info & NOINDEX) || is_scalar(node) || is_vector(node))

gencat(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - type
	ptr2 - rank
	ptr3 - shape
	ptr4 - s sub i for left hand side
	ptr5 - e sub i for left hand side
	ptr6 - size along ith dimension (sum of s's)
	ptr7 - result
	ptr8 - s sub i for right hand side
	ptr9 - value in given axis
	ptr10 - temp
*/
	switch (mode) {
case SHAPE:
	adjdcls(node);
	switchbox(LEFT, SHAPE, 0);
	switchbox(RIGHT, SHAPE, 0);

	if (! (node->info & TYPEKNOWN))
		dsopt(NOT, node, LEFT, RIGHT, node->ptr1);

	if (! (node->info & SHAPEKNOWN)) {
		rkeq(node, node->ptr2);
		printf("catshape(&mp%d, ", node->ptr3);
		lrnsrrns(LEFT, RIGHT);
		rpseminl();
		node->shape.c = gicn(memptr, node->ptr3, INT);
		}

	if (node->info & LASTAXIS) {
		axies(node, node->a.axis, LEFT, 0, 0, node->ptr4);
		axies(node, node->a.axis, RIGHT, 0, 0, node->ptr8);
		}
	else {
		error("cat on axis not implemented yet");
		axies(node, node->a.axis, LEFT, 0, node->ptr5, node->ptr4);
		axies(node, node->a.axis, RIGHT, 0, LEFT->index, node->ptr8);
		}

	/* compute sum of left and right along axis */
	iopi(node->ptr6, node->ptr4, "+", node->ptr8);

	/* sequential axis is special case */
	if (node->info & SEQUENTIAL) {
		if (! (LEFT->info & NOINDEX))
			setizero(LEFT->index);
		if (! (RIGHT->info & NOINDEX))
			setizero(RIGHT->index);
		}
	break;

case VALUE:
	/* first compute asubi */
	ieq(node->ptr9);
	asubi(node, RIGHT, node->index, node->ptr5, node->ptr6);
	seminl();
	printf("if (i%d < i%d) {\n", node->ptr9, node->ptr4);

	/* left side */
	if (! non_index(LEFT))
		cmpidx2(node, node->ptr10, LEFT->index, node->index,
			node->ptr9, node->ptr5, node->ptr4, node->ptr6);
	switchbox(LEFT, VALUE, 0);
	node->ptr7 = resinreg(LEFT, node->ptr7);
	if (node->info & SEQUENTIAL)
		if (! (LEFT->info & NOINDEX))
			iincr(LEFT->index);
	rbr();

	/* right side */
	elsebr();
	if (! ((RIGHT->info & NOINDEX) || 
		(node->info & SEQUENTIAL)))
		printf("i%d -= i%d;\n", node->ptr9, node->ptr4);
	if (! non_index(RIGHT))
		cmpidx2(node, node->ptr10, RIGHT->index, node->index,
			node->ptr9, node->ptr5, node->ptr8, node->ptr6);
	switchbox(RIGHT, VALUE, 0);
	node->ptr2 = resinreg(RIGHT, node->ptr7);
	if (node->ptr2 != node->ptr7)
		printf("res%d = res%d;\n", node->ptr7,
		node->ptr2);
	if (node->info & SEQUENTIAL)
		if (! (RIGHT->info & NOINDEX))
			iincr(RIGHT->index);
	rbr();
	node->values.c = 
		gicn(resptr, node->ptr7, rtype(node));
	break;

case FINISH:
	switchbox(LEFT, FINISH, 0);
	switchbox(RIGHT, FINISH, 0);
	if (! (node->info & SHAPEKNOWN))
		mpfree(node->ptr3);
	break;
	}
}

/*
	genrotate -
		generate code for dyadic rotate instruction
*/
genrotate(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - e sub i
	ptr2 - s sub i
	ptr3 - new index for right side along axis
*/
	switch (mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(LEFT, SHAPE, 0);
			switchbox(RIGHT, SHAPE, 0);
			copychild(node, RIGHT, 1, 1, 1);
			axies(node, node->a.axis, RIGHT, 0, node->ptr1,
					node->ptr2);
			break;

		case VALUE:
			/* compute left index */
			if (node->info & LASTAXIS)
				iopi(LEFT->index, node->index, "/",
					node->ptr2);
			else if (node->info & FIRSTAXIS)
				iopi(LEFT->index, node->index, "%", 
					node->ptr1);
			else {
				divmod(node->index, node->ptr1,
					RIGHT->index, LEFT->index);
				printf("i%d += (i%d / i%d) * i%d;\n",
					LEFT->index, RIGHT->index,
					node->ptr2, node->ptr1);
				}

			/* get value */
			switchbox(LEFT, VALUE, 0);

			/* add value to appropriate position of rhs */
			ieq(node->ptr3);
			lp();
			ctgen(LEFT->values.c);
			printf(" + ");
			asubi(node, RIGHT, node->index, node->ptr1, node->ptr2);
			seminl();

			/* make sure it is right range */
			printf("if (i%d < 0) i%d += i%d;\n",
				node->ptr3, node->ptr3, node->ptr2);
			printf("if (i%d >= i%d) i%d -= i%d;\n",
				node->ptr3, node->ptr2, node->ptr3,
				node->ptr2);

			/* now compute right value */
			if (! is_vector(RIGHT))
				cmpidx2(node, LEFT->index, RIGHT->index,
					node->index, node->ptr3,
					node->ptr1, node->ptr2,
					node->ptr2);
			switchbox(RIGHT, VALUE, 0);
			node->values.c = RIGHT->values.c;
			break;

		case FINISH:
			switchbox(LEFT, FINISH, 0);
			switchbox(RIGHT, FINISH, 0);
			break;
	}
}
