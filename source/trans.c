/*
	APL Compiler

	Generate code for those operators that can be merge into
	a single accessor
	(rotation, transpose, etc)
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
# include <stdio.h>

/* topmerge -
	we are at the top of a mergeable unit -
	create the initial values for the vectors q, s and d
*/

static struct node *topnode; 	/* remember the top of the current stack
					of mergeable operators */

static topmerge(node)
struct node *node;
{

	topnode = node;
	if (! (node->info & NOINDEX)) {
		ieq(node->ptr8);
		printf("qsdalloc(");
		ctgen(node->rank.c);
		printf(", &mp%d, &mp%d, &mp%d);\n", 
			node->ptr1, node->ptr2, node->ptr3);
		switchbox(node, COMBINE, 0);
	}
}

/* prqsd - print out the q, s and d arguments in a function call */
static prqsd(node)
struct node *node;
{
	printf("mp%d.ip, mp%d.ip, mp%d.ip",
		node->ptr1, node->ptr2, node->ptr3);
}

/* midmerge - we are in the middle of a mergeable unit */
static midmerge(node, child)
struct node *node, *child;
{
	if (is_mergeable(child))
		switchbox(child, COMBINE, 0);
	else if (! (child->info & NOINDEX)) {
		ieq(node->ptr4);	/* alpha */
		printf("accessor(");
		rns(topnode);
		commasp();
		rns(child);
		printf(", &mp%d, ", node->ptr5);
		prqsd(node);
		rpseminl();
		if (topnode->info & SEQUENTIAL)
			ieqi(child->index, node->ptr4);
		}
}

/*
	mergevalue - do value phase processing of mergeable node */
static mergevalue(node, child)
struct node *node, *child;
{
	/* take care of trivial case first */
	if (is_mergeable(child) || (child->info & NOINDEX)) {
		switchbox(child, VALUE, 0);
		node->values.c = child->values.c;
		return;
		}

	/* now we can get down to business */

	if (! (topnode->info & SEQUENTIAL)) {
		ieqi(node->ptr6, node->index);
		ieqi(child->index, node->ptr4);
		izloop(node->ptr7, node->ptr8);
		printf("i%d += i%d * *(mp%d.ip + i%d);\n",
			child->index, node->ptr6, node->ptr5, node->ptr7);
		printf("i%d /= *(", node->ptr6);
		ctgen(topnode->shape.c);
		printf(" + i%d);\n", node->ptr7);
		rbr();
		}

	switchbox(child, VALUE, 0);
	node->values.c = child->values.c;

	if (topnode->info & SEQUENTIAL) {
		resinreg(node, node->ptr10);
		ieq(node->ptr6);
		ctgen(gmon(deref, gsop(PLUS, topnode->shape.c,
			gsop(MINUS, gicn(iptr, node->ptr8, INT),
				gicn(icnst, 1, INT)))));
		seminl();
		izloop(node->ptr7, node->ptr8);
		printf("i%d += *(mp%d.ip + i%d);\n",
			child->index, node->ptr5, node->ptr7);
		printf("if (0 == ((i%d + 1) %% i%d))\n",
			topnode->index, node->ptr6);
		printf("i%d *= ", node->ptr6);
		ctgen(gmon(deref, gsop(PLUS, topnode->shape.c,
			gsop(MINUS, gicn(iptr, node->ptr7, INT),
				gicn(icnst, 1, INT)))));
		seminl();
		printf("else\nbreak;\n}\n");
		}
}

/* freemerge -
	free up the storage used by the merged operands */
static freemerge(node)
struct node *node;
{
	;	/* fill it in later */
}

/*
	gentrans -
		generate code for monadic transpose
*/
gentrans(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - q
	ptr2 - s
	ptr3 - d
	ptr4 - alpha
	ptr5 - delta
	ptr6 - temporary variable
	ptr7 - counter
	ptr8 - rank
	ptr9 - shape
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(RIGHT, SHAPE, 0);
			copychild(node, RIGHT, 1, 1, 0);
			if (! (node->info & SHAPEKNOWN)) {
				setrank(node->ptr6, node);
				impalloc(node->ptr9, node->ptr6);
				izloop(node->ptr7, node->ptr6);
				mpipieq(node->ptr9, node->ptr7);
				printf("*(");
				ctgen(RIGHT->shape.c);
				printf(" + (i%d - i%d) - 1);\n", 
				node->ptr6, node->ptr7);
				rbr();
				node->shape.c = 
					gicn(memptr, node->ptr9, INT);
				}
			if (! (node->info & MERGED))
				topmerge(node);
			break;

		case COMBINE:
			printf("trmerge(");
			ctgen(node->rank.c);
			commasp();
			prqsd(node);
			rpseminl();
			midmerge(node, RIGHT);
			break;

		case VALUE:
			mergevalue(node, RIGHT);
			break;

		case FINISH:
			if (! (node->info & MERGED))
				freemerge(node);
			switchbox(RIGHT, FINISH, 0);
			break;
	}
}

/*
	genreverse - generate code for monadic reversal
*/
genreverse(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - q
	ptr2 - s
	ptr3 - d
	ptr4 - alpha
	ptr5 - delta
	ptr6 - temporary variable
	ptr7 - counter
	ptr8 - rank
	ptr9 - axis
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(RIGHT, SHAPE, 0);
			copychild(node, RIGHT, 1, 1, 1);
			axies(node, node->a.axis, RIGHT, node->ptr9, 0, 0);
			if (! (node->info & MERGED))
				topmerge(node);
			break;

		case COMBINE:
			mpipieq(node->ptr2, node->ptr9);
			lp();
			trepi(node->shape.c, node->ptr9);
			printf(" - ");
			dmppi(node->ptr2, node->ptr9);
			printf(") -1;\n");
			mpipieq(node->ptr3, node->ptr9);
			printf(" - ");
			dmppi(node->ptr3, node->ptr9);
			seminl();
			midmerge(node, RIGHT);
			break;

		case VALUE:
			mergevalue(node, RIGHT);
			break;

		case FINISH:
			if (! (node->info & MERGED))
				freemerge(node);
			switchbox(RIGHT, FINISH, 0);
			break;
	}
}
/*
	gentake - generate code for take function
*/
gentake(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - q
	ptr2 - s
	ptr3 - d
	ptr4 - alpha
	ptr5 - delta
	ptr6 - temporary variable
	ptr7 - counter
	ptr8 - rank
	ptr9 - shape
*/
	switch (mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(LEFT, SHAPE, 0);
			switchbox(RIGHT, SHAPE, 0);
			copychild(node, RIGHT, 1, 1, 0);
			if (! (node->info & SHAPEKNOWN)) {
				setrank(node->ptr6, node);
				impalloc(node->ptr9, node->ptr6);
				izloop(node->ptr7, node->ptr6);
				mpipieq(node->ptr9, node->ptr7);
				printf("iabs(");
				trepi(LEFT->values.c, node->ptr7);
				rpseminl();
				rbr();
				node->shape.c = 
					gicn(memptr, node->ptr9, INT);
				}
			if (! (node->info & MERGED))
				topmerge(node);
			break;

		case COMBINE:
			rkloop(node, node->ptr7, node->ptr6);
			ctzero(LEFT->values.c, node->ptr7, "<");
			dmppi(node->ptr2, node->ptr7);
			printf(" += ");
			trepi(RIGHT->shape.c, node->ptr7);
			printf(" + ");
			trepi(LEFT->values.c, node->ptr7);
			seminl();
			rbr();
			midmerge(node, RIGHT);
			break;

		case VALUE:
			mergevalue(node, RIGHT);
			break;

		case FINISH:
			if (! (node->info & MERGED))
				freemerge(node);
			switchbox(RIGHT, FINISH, 0);
			break;
		
	}
}
/*
	gendrop - generate code for drop function
*/
gendrop(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - q
	ptr2 - s
	ptr3 - d
	ptr4 - alpha
	ptr5 - delta
	ptr6 - temporary variable
	ptr7 - counter
	ptr8 - rank
	ptr9 - shape
*/
	switch (mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(LEFT, SHAPE, 0);
			switchbox(RIGHT, SHAPE, 0);
			copychild(node, RIGHT, 1, 1, 0);
			if (! (node->info & SHAPEKNOWN)) {
				setrank(node->ptr6, node);
				impalloc(node->ptr9, node->ptr6);
				izloop(node->ptr7, node->ptr6);
				mpipieq(node->ptr9, node->ptr7);
				trepi(RIGHT->shape.c, node->ptr7);
				printf(" - iabs(");
				trepi(LEFT->values.c, node->ptr7);
				rpseminl();
				rbr();
				node->shape.c = 
					gicn(memptr, node->ptr9, INT);
				}
			if (! (node->info & MERGED))
				topmerge(node);
			break;

		case COMBINE:
			if (is_scalar(LEFT->right)) {
				if (LEFT->info & VALUESKNOWN) {
					top = iconsts[valvalue(LEFT)];
					if (top < 0) {
						printf("*mp%d.ip += %d;\n",
							node->ptr2, top);
						}
					}
				else {
					ctzero(LEFT->values.c, 0, ">=");
					printf("*mp%d.ip += ", node->ptr2);
					ctgen(LEFT->values.c);
					seminl();
					}
				}
			else {
				rkloop(node, node->ptr7, node->ptr6);
				ctzero(LEFT->values.c, node->ptr7, ">=");
				dmppi(node->ptr2, node->ptr7);
				printf(" += ");
				trepi(LEFT->values.c, node->ptr7);
				seminl();
				rbr();
				}
			midmerge(node, RIGHT);
			break;

		case VALUE:
			mergevalue(node, RIGHT);
			break;

		case FINISH:
			if (! (node->info & MERGED))
				freemerge(node);
			switchbox(RIGHT, FINISH, 0);
			break;

	}
}

/*
	gendtrans -
		generate code for dyadic transpose
*/
gendtrans(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - q
	ptr2 - s
	ptr3 - d
	ptr4 - alpha
	ptr5 - delta
	ptr6 - temporary variable
	ptr7 - counter
	ptr8 - rank
	ptr9 - shape
*/
	switch (mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(LEFT, SHAPE, 0);
			switchbox(RIGHT, SHAPE, 0);
			copychild(node, RIGHT, 1, 0, 0);
			if (! (node->info & SHAPEKNOWN)) {
				rkeq(node, node->ptr6);
				printf("dtshape(&mp%d, ", node->ptr9);
				ctgen(LEFT->values.c);
				commasp();
				rns(RIGHT);
				rpseminl();
				node->shape.c =
					gicn(memptr, node->ptr9, INT);
				}
			if (! (node->info & MERGED))
				topmerge(node);
			break;

		case COMBINE:
			printf("dtmerge(");
			ctgen(LEFT->values.c);
			commasp();
			ctgen(RIGHT->rank.c);
			printf(", &mp%d, &mp%d, &mp%d);\n",
				node->ptr1, node->ptr2, node->ptr3);
			midmerge(node, RIGHT);
			break;

		case VALUE:
			mergevalue(node, RIGHT);
			break;

		case FINISH:
			if (! (node->info & MERGED))
				freemerge(node);
			switchbox(RIGHT, FINISH, 0);
			break;

	}
}
