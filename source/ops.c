/*
	APL Compiler

	Code Generation routines having to do with Operators
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

/*
	dsopt - generate type information for dyadic scalar ops 
*/

static char *dostrs[] = {"NOT",
"FLOOR", "CEIL", "PLUS", "MINUS", "TIMES", "ABS", "DIVIDE", "EXP",
"LOG", "CIRCLE", "FACT",
"AND", "OR", "NAND", "NOR", "LT", "LE", "EQ", "NE", "GE", "GT"};

dsopt(op, node, left, right, ival)
enum sops op;
struct node *node, *left, *right;
int ival;
{
	/*
		note - not is used for cat function to represent the type
		of the result
	*/

	if (cteq(left->type.c, right->type.c)) {
		if ((op == NOT) || (op == PLUS) || 
		(op == MINUS) || (op == TIMES)) {
			node->type.c = left->type.c;
			return;
			}
		}
	ieq(ival);
	printf("dsopt(%s, ", dostrs[(int) op]);
	ctgen(left->type.c);
	commasp();
	ctgen(right->type.c);
	rpseminl();
	node->type.c = gicn(iptr, ival, 0);
}

/*
	dsopv - generate values for dyadic scalar ops 
*/

dsopv(op, node, left, right, resval, lresval, rresval)
enum sops op;
struct node *node, *left, *right;
int resval, lresval, rresval;
{	int rt;

	rt = UKTYPE;

	switch(op) {
		case PLUS: case MINUS: case TIMES:
			if (rtype(node)== UKTYPE)
				break;
			mkrktype(left, rtype(node), lresval);
			mkrktype(right, rtype(node), rresval);
			node->values.c = gsop(op, left->values.c,
				right->values.c);
			return;

		case DIVIDE:
			if (! (left->info & TYPEKNOWN)) break;
			if (! (right->info & TYPEKNOWN)) break;
			mkrktype(left, REAL, lresval);
			mkrktype(right, REAL, rresval);
			node->values.c = gsop(op, left->values.c, 
				right->values.c);
			return;
			
		case ABS:
			if (! (left->info & TYPEKNOWN)) break;
			if (! (right->info & TYPEKNOWN)) break;
			rt = maxtype(rtype(left), rtype(right));
			if (rt != INT) break;
			mkrktype(left, rt, lresval);
			mkrktype(right, rt, rresval);
			node->values.c = gsop(op, left->values.c, 
				right->values.c);
			return;

		case AND: case OR:
			mkrktype(left, BIT, lresval);
			mkrktype(right, BIT, rresval);
			node->values.c = gsop(op, left->values.c,
				right->values.c);
			return;

		case LT: case LE: case EQ: case NE: case GT: case GE:
			rt = BIT;
			if (! (left->info & TYPEKNOWN)) break;
			if (! (right->info & TYPEKNOWN)) break;
			rt = maxtype(rtype(left), rtype(right));
			mkrktype(left, rt, lresval);
			mkrktype(right, rt, rresval);
			node->values.c = gsop(op, left->values.c, 
				right->values.c);
			return;
	}

	rresval = resinreg(right, rresval);
	lresval = resinreg(left, lresval);
	if (op == NOT)
		printf("dsopv(CAT, ");
	else
		printf("dsopv(%s, ", dostrs[(int) op]);
	printf("&res%d, &res%d, ", resval, lresval);
	ctgen(left->type.c);
	printf(", &res%d, ", rresval);
	ctgen(right->type.c);
	rpseminl();
	node->values.c = gicn(resptr, resval, rtype(node));
}

/*
	adsop - append a value to an existing register
*/
adsop(op, node, child, resval)
enum sops op;
struct node *node, *child;
int resval;
{	int r;

	r = rtype(node);

	switch(op) {
		case PLUS:
			if (r == INT) {
				printf("res%d.i += ", resval);
				ctgen(child->values.c);
				seminl();
				node->values.c = gicn(resptr, resval, INT);
				return;
				}
			break;

		case TIMES:
			if (r == INT) {
				printf("res%d.i *= ", resval);
				ctgen(child->values.c);
				seminl();
				node->values.c = gicn(resptr, resval, INT);
				return;
				}
			break;
		}
	dsopv(op, node, child, node, resval, 0, resval);
	r = resinreg(node, resval);
	if (r != resval)
		printf("res%d = res%d;\n", resval, r);
}

/*
	msopv - generate values for monadic scalar operators
*/

static char *tfields[] = {"i", "i", "i", "r", "c", "i", "i"};

msopv(op, node, child, resval)
enum sops op;
struct node *node, *child;
int resval;
{	int rt;
	char *s;

	rt = UKTYPE;
	switch(op) {
		case NOT:
			if (! (child->info & TYPEKNOWN)) break;
			mkrktype(child, BIT, resval);
			node->values.c = gmop(op, child->values.c);
			return;

		case FLOOR: case CEIL: case DIVIDE:
		case EXP: case LOG: case CIRCLE:
			if (! (child->info & TYPEKNOWN)) break;
			mkrktype(child, REAL, resval);
			node->values.c = gmop(op, child->values.c);
			return;

		case PLUS:
			node->values.c = child->values.c;
			return;

		case MINUS:
			if (! (child->info & TYPEKNOWN)) break;
			node->values.c = gmop(op, child->values.c);
			return;

		case ABS:
			if (! (node->info & TYPEKNOWN)) break;
			resval = resinreg(child, resval);
			s = tfields[rtype(node)];
			printf("if (res%d.%s < 0)\n", resval, s);
			printf("res%d.%s = - res%d.%s;\n", 
				resval, s, resval, s);
			node->values.c = 
				gicn(resptr, resval, rtype(node));
			return;
	}

	resval = resinreg(child, resval);
	printf("msopv(%s, &res%d, ", dostrs[(int) op], resval);
	ctgen(child->type.c);
	rpseminl();
	node->values.c = gicn(resptr, resval, rtype(node));
}
/*
	identity -
		generate a left identity for an op
*/
identity(op, resval, node)
enum sops op;
int resval;
struct node *node;
{	int t;

	t = rtype(node);
	node->values.c = gicn(resptr, resval, t);

	switch(op) {
		case PLUS: case MINUS: case OR: case LT: case NE:
			if ((t == BIT) || (t == INT)) {
				printf("res%d.i = 0;\n", resval);
				return;
				}
			if (t == REAL) {
				printf("res%d.r = 0;\n", resval);
				return;
				}
			break;

		case TIMES: case DIVIDE: case EXP: case AND:
		case FACT: case LE: case EQ:
			if ((t == BIT) || (t == INT)) {
				printf("res%d.i = 1;\n", resval);
				return;
				}
			if (t == REAL) {
				printf("res%d.r = 1.0;\n", resval);
				return;
				}
			break;

		case CEIL: /* smallest number */
			break;

		case FLOOR: /* largest number */
			break;

		}
	printf("identity(%s, &res%d, ", dostrs[(int) op], resval);
	ctgen(node->type.c);
	rpseminl();
}

/*gendsop - generate code for dyadic scalar ops */
gendsop(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - result register for left side
	ptr2 - result register for right side
	ptr3 - i register for type
	ptr4 - i register for rank
	ptr5 - mp for shape
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(LEFT, mode, 0);
			switchbox(RIGHT, mode, 0);
			if (! (node->info & TYPEKNOWN))
				dsopt(node->optype, node, LEFT, RIGHT, 
					node->ptr3);
			if (! (node->info & SHAPEKNOWN)) {
				if (is_scalar(LEFT))
					copychild(node, RIGHT, 0, 1, 1);
				else if (is_scalar(RIGHT) ||
					(cteq(LEFT->rank.c, RIGHT->rank.c) &&
					cteq(LEFT->shape.c,
					RIGHT->shape.c))) 
					copychild(node, LEFT, 0, 1, 1);
				else {
					rkeq(node, node->ptr4);
					printf("dsops(&mp%d, ", node->ptr5);
					lrnsrrns(LEFT, RIGHT);
					rpseminl();
					node->shape.c = 
						gicn(memptr, node->ptr5, INT);
					}
				}
			break;

		case VALUE:
			switchbox(LEFT, mode, 0);
			switchbox(RIGHT, mode, 0);
			dsopv(node->optype, node, LEFT, RIGHT, node->ptr1,
				node->ptr1, node->ptr2);
			break;

		case FINISH:
			switchbox(LEFT, mode, 0);
			switchbox(RIGHT, mode, 0);
			break;

	}
}

/*
	genmsop - generate code for monadic scalar operators */
genmsop(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{	enum sops op;
/*
	ptr1 - type
	ptr2 - result
*/
	switch(mode) {
		case SHAPE:
			adjdcls(node);
			switchbox(RIGHT, SHAPE, 0);
			op = node->optype;
			if (! (node->info & TYPEKNOWN)) {
				if ((op == NOT) || 
					(op == MINUS) ||
					(op == ABS))
					node->type.c = RIGHT->type.c;
				else {
					ieq(node->ptr1);
					printf("msopt(%s, ", dostrs[(int) op]);
					ctgen(RIGHT->type.c);
					rpseminl();
					node->type.c = 
						gicn(iptr, node->ptr1, INT);
					}
				}
			copychild(node, RIGHT, 0, 1, 1);
			break;

		case VALUE:
			switchbox(RIGHT, VALUE, 0);
			msopv(node->optype, node, RIGHT, node->ptr2);
			break;

		case FINISH:
			switchbox(RIGHT, FINISH, 0);
			break;
	}
}
/*genouter - generate code for outer product */
genouter(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 = mp for shape
	ptr2 = size of right hand side
	ptr4 = res register for left hand side
	ptr5 = res register for right hand side
	ptr6 = rank of result
*/
	switch(mode) {
case SHAPE:
	adjdcls(node);
	switchbox(LEFT, SHAPE, 0);
	switchbox(RIGHT, SHAPE, 0);
	if (! (node->info & TYPEKNOWN))
		dsopt(node->optype, node, LEFT, RIGHT, node->a.ptr0);
	if (! (node->info & SHAPEKNOWN)) {
		rkeq(node, node->ptr6);
		printf("outershape(&mp%d, ", node->ptr1);
		lrnsrrns(LEFT, RIGHT);
		rpseminl();
		node->shape.c = gicn(memptr, node->ptr1, INT);
		}
	getsize(node->ptr2, RIGHT);
	if (node->info & SEQUENTIAL) {
		ieqi(RIGHT->index, node->ptr2);
		if (! (LEFT->info & NOINDEX))
			seticon(LEFT->index, -1);
		}
	break;

case VALUE:
	if (node->info & SEQUENTIAL) {
		printf("if (i%d >= i%d) {\n", RIGHT->index, node->ptr2);
		iincr(LEFT->index);
		}
	else
		divmod(node->index, node->ptr2, LEFT->index, RIGHT->index);

	switchbox(LEFT, VALUE, 0);

	if (node->info & SEQUENTIAL) {
		resinreg(LEFT, node->ptr4);
		seticon(RIGHT->index, -1);
		rbr();
		iincr(RIGHT->index);
		}

	switchbox(RIGHT, VALUE, 0);

	dsopv(node->optype, node, LEFT, RIGHT, node->ptr5,
		node->ptr4, node->ptr5);
	break;

case FINISH:
	switchbox(LEFT, FINISH, 0);
	switchbox(RIGHT, FINISH, 0);
	if (! (node->info & SHAPEKNOWN))
		mpfree(node->ptr1);
	break;
	}
}
/*
	genred - generate code for reduction function
*/
genred(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - memory pointer for shape
	ptr2 - e sub i
	ptr3 - s sub i
	ptr4 - axis, also loop counter
	ptr5 - result register
	ptr6 - type
	ptr7 - rank
	ptr8 - e sub i * s sub i
	ptr9 - offset div e sub i
*/
	switch(mode) {
case SHAPE:
	adjdcls(node);
	switchbox(RIGHT, SHAPE, 0);
	if (! (node->info & TYPEKNOWN))
		dsopt(node->optype, node, RIGHT, RIGHT, node->ptr6);

	redshape(node, node->a.axis, node->ptr4, node->ptr2, node->ptr3, 
			node->ptr1, node->ptr7);

	/* if sequential initialize right index */
	if (! (RIGHT->info & NOINDEX))
		if ((RIGHT->info & SEQUENTIAL) && ! is_vector(RIGHT))
			setizero(RIGHT->index);

	/* if we need it, compute temporary e sub i * s sub i */
	if (! (RIGHT->info & SEQUENTIAL))
		if (node->info & FIRSTAXIS)
			printf("i%d = (i%d - 1) * i%d;\n",
				node->ptr8, node->ptr3, node->ptr2);
		else if (! (node->info & LASTAXIS))
			iopi(node->ptr8, node->ptr2, "*", node->ptr3);

	break;

case VALUE:

	identity(node->optype, node->ptr5, node);

	/* if not sequential and not vector we need to compute base */
	if (! ((RIGHT->info & SEQUENTIAL) || is_vector(RIGHT))) {
		if (! (RIGHT->info & NOINDEX)) {
			if (node->info & FIRSTAXIS)
				printf("i%d = i%d + i%d;\n", 
				   RIGHT->index, node->ptr8, node->index);
			else if (node->info & LASTAXIS)
				printf("i%d = (i%d + 1) * i%d - 1;\n", 
				   RIGHT->index, node->index, node->ptr3);
			else {
				divmod(node->index, node->ptr2, RIGHT->index, 
					node->ptr9);
				printf("i%d += (i%d + 1) * i%d - i%d;\n", 
				   RIGHT->index, node->ptr9,
				   node->ptr8, node->ptr2);
				}
			}
		}

	if (is_vector(RIGHT) && commute(node->optype))
		iiloop(RIGHT->index, node->ptr3);
	else
		izloop(node->ptr4, node->ptr3);

	switchbox(RIGHT, VALUE, 0);
	adsop(node->optype, node, RIGHT, node->ptr5);

	if (is_vector(RIGHT))
		;	/* do nothing, updated in loop */
	else if (! (RIGHT->info & NOINDEX)) {
		if (RIGHT->info & SEQUENTIAL)
			iincr(RIGHT->index);
		else 
			esubtract(node, RIGHT->index, node->ptr2);
		}
	rbr();
	break;

case FINISH:
	if ( ! ((node->info & FIRSTAXIS) || (node->info & LASTAXIS)))
		mpfree(node->ptr1);
	switchbox(RIGHT, FINISH, 0);
	break;
	}
}
/*
	genscan - generate code for scan function
*/
genscan(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 - axis position
	ptr2 - e sub i
	ptr3 = s sub i
	ptr5 - counter for loop
	ptr6  - result register
	ptr7 - type
*/
	switch(mode) {
case SHAPE:
	adjdcls(node);
	switchbox(RIGHT, SHAPE, 0);

	if (! (node->info & TYPEKNOWN))
		dsopt(node->optype, node, RIGHT, RIGHT, node->ptr7);

	copychild(node, RIGHT, 0, 1, 1);

	if (RIGHT->info & SEQUENTIAL)
		axies(node, node->a.axis, RIGHT, 0, 0, node->ptr3);
	else
		axies(node, node->a.axis, RIGHT, 0, node->ptr2, node->ptr3);

	if (RIGHT->info & SEQUENTIAL) {
		if (is_vector(RIGHT))
			identity(node->optype, node->ptr6, node);
		else if (! (RIGHT->info & NOINDEX))
			ieqi(node->ptr5, node->ptr3); /* counter = s sub i */
		}

	break;

case VALUE:
	if (RIGHT->info & SEQUENTIAL) {
		if (! is_vector(RIGHT)) {
			printf("if (i%d >= i%d) {\n", node->ptr5, node->ptr3);
			identity(node->optype, node->ptr6, node);
			seticon(node->ptr5, 0);
			rbr();
			}
		}

	else {
		identity(node->optype, node->ptr6, node);
		if (! is_vector(RIGHT))
			ieqi(RIGHT->index, node->index);
		printf("for (i%d = ", node->ptr5);
		asubi(node, RIGHT, node->index, node->ptr2, node->ptr3);
		printf("; i%d >= 0; i%d--) {\n", node->ptr5, node->ptr5);
		}

	switchbox(RIGHT, VALUE, 0);
	adsop(node->optype, node, RIGHT, node->ptr6);

	if (RIGHT->info & SEQUENTIAL) {
		if (! is_vector(RIGHT))
			iincr(node->ptr5);
		}
	else {
		if (! is_vector(RIGHT))
			esubtract(node, RIGHT->index, node->ptr2);
		rbr();
		}
	break;

case FINISH:
	switchbox(RIGHT, FINISH, 0);
	break;
	}
}

/*
	gendecode -
		code generation routines for both the inner portion of
		inner product and for decode
*/
gendecode(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{	struct codetree *savetree;
/*
	ptr1 - type
	ptr2 - rank
	ptr3 - shape
	ptr6 - decode intermediate result register
	ptr7 - result
	ptr8 - left result register
	ptr9 - right result register
*/
	switch(mode) {
case SHAPE:
	adjdcls(node);
	switchbox(LEFT, SHAPE, 0);
	switchbox(RIGHT, SHAPE, 0);

	if (! (node->info & TYPEKNOWN))
		dsopt(node->optype, node, LEFT, RIGHT, node->ptr1);

	if (! (node->info & SHAPEKNOWN)) {
		rkeq(node, node->ptr2);
		printf("innershape(&mp%d, ", node->ptr3);
		lrnsrrns(LEFT, RIGHT);
		rpseminl();
		node->shape.c = gicn(memptr, node->ptr3, INT);
		}
	break;

case VALUE:	
	/* compute the two values */
	if (node->nodetype == DECODE) {
		savetree = LEFT->values.c;
		LEFT->values.c = gicn(resptr, node->ptr6, INT);
		}
	else
		switchbox(LEFT, VALUE, 0);
	switchbox(RIGHT, VALUE, 0);
	dsopv(node->optype, node, LEFT, RIGHT, 
		node->ptr7, node->ptr8, node->ptr9);
	if (node->nodetype == DECODE) {
		LEFT->values.c = savetree;
		resinreg(node, node->ptr7);
		savetree = node->values.c;
		node->values.c = gicn(resptr, node->ptr6, INT);
		switchbox(LEFT, VALUE, 0);
		adsop(node->optype, node, LEFT, node->ptr6);
		node->values.c = savetree;
		}
	break;

case FINISH:
	switchbox(LEFT, FINISH, 0);
	switchbox(RIGHT, FINISH, 0);
	if (! (node->info & SHAPEKNOWN))
		mpfree(node->ptr3);
	}
}

/*
	geninner -
		generate code for both inner product and decode
*/
/* 	macros to access the grandchildren */

# define RIGHTRIGHT (RIGHT->right)
# define RIGHTLEFT (RIGHT->left)

geninner(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
/*
	ptr1 -type, shared with child
	ptr2 - s sub i for right child of right child
	ptr3 - e sub i for right child of right child
	ptr4 - e sub i * (s sub i - 1)
	ptr6 - final result
*/
	switch(mode) {
case SHAPE:
	adjdcls(node);
	switchbox(RIGHT, SHAPE, 0);
	if (! (node->info & TYPEKNOWN))
		dsopt(node->optype, node, RIGHT, RIGHT, node->ptr1);
	copychild(node, RIGHT, 0, 1, 1);

	/* now compute extent of inner loop, and axis values */
	axies(RIGHT, NILP, RIGHTRIGHT, 0, node->ptr3, node->ptr2);
	if (is_scalar(RIGHTRIGHT))
		axies(node, NILP, RIGHTLEFT, 0, 0, node->ptr2);
	else if (! (RIGHTRIGHT->info & RANKKNOWN)) {
		printf("if (0 == ");
		ctgen(RIGHTRIGHT->rank.c);
		printf(") {\n");
		axies(node, NILP, RIGHTLEFT, 0, 0, node->ptr2);
		rbr();
		}
	idecr(node->ptr2);
	iopi(node->ptr4, node->ptr2, "*", node->ptr3);
	break;

case VALUE:
# define no_index(node) (is_scalar(node) || is_vector(node) \
	|| (node->info & NOINDEX))

	/* compute base values for start of loop */
	if (! (no_index(RIGHTRIGHT) && no_index(RIGHTLEFT)))
		divmod(node->index, node->ptr3, 
			RIGHTLEFT->index, RIGHTRIGHT->index);
	if (! no_index(RIGHTLEFT))
		printf("i%d = i%d * ( i%d + 1 ) + i%d;\n", RIGHTLEFT->index,
			RIGHTLEFT->index, node->ptr2, node->ptr2);
	if (! no_index(RIGHTRIGHT))
		printf("i%d += i%d;\n", RIGHTRIGHT->index, node->ptr4);

	identity(node->optype, node->ptr6, node);
	if (RIGHT->nodetype == DECODE)
		identity(RIGHT->optype, RIGHT->ptr6, RIGHT);

	/* generate loop for values */
	isloop(RIGHT->ptr5, node->ptr2);
	switchbox(RIGHT, VALUE, 0);
	adsop(node->optype, node, RIGHT, node->ptr6);

	/* decrement index values for right and left side */
	if (! no_index(RIGHTLEFT))
		idecr(RIGHTLEFT->index);
	if (! no_index(RIGHTRIGHT))
		esubtract(RIGHT, RIGHTRIGHT->index, node->ptr3);

	rbr();
	break;
	}
}
