/*
	APL Compiler

	Run time system
	routines having to do with type and shape computations
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
# include "aplc.h"
# include <stdio.h>

/* dsopt - return the type of a dyadic scalar function */
int dsopt(op, lefttype, righttype)
enum sops op;
int lefttype, righttype;
{	int maxtype;

	maxtype = (lefttype > righttype) ? lefttype : righttype;

	switch(op) {
		case AND: case OR: case NAND: case NOR:
		case LT: case LE: case EQ: case NE: case GE: case GT:
			return(BIT);

		case CAT: case PLUS: case MINUS: case TIMES: case ABS:
			return(maxtype);

		case DIVIDE: case EXP: case LOG: case FACT: case CIRCLE:
			return(REAL);

		default: 
			fprintf(stderr,"operator %d\n", op);
			error("unimplemented operator in dsopt used");
		}
	return(UKTYPE);	/* lint stuffing */
}

/* msopt - return the type of a monadic scalar function */
int msopt(op, type)
enum sops op;
int type;
{
	switch(op) {
		case ABS: case MINUS:
			return(type);

		default: 
			fprintf(stderr,"operator %d\n", (int) op);
			error("unimplemented operator in msopt used");
		}
}

/* dsops - compute rank and shape of dyadic scalar function */
int dsops(shape, leftrank, leftshape, rightrank, rightshape)
union mp_struct *shape;
int leftrank, *leftshape, rightrank, *rightshape;
{	int i;

	if (leftrank == 0) {
		valloc(shape, rightrank, INT);
		cpvec(shape->ip, rightrank, rightshape);
		return(rightrank);
		}
	if (rightrank == 0) {
		valloc(shape, leftrank, INT);
		cpvec(shape->ip, leftrank, leftshape);
		return(leftrank);
		}
	if (leftrank != rightrank)
		error("dyadic scalar rank comformability error");
	for (i = 0; i < rightrank; i++)
		if (leftshape[i] != rightshape[i])
			error("dyadic scalar shape conformability error");
	valloc(shape, rightrank, INT);
	cpvec(shape->ip, rightrank, rightshape);
	return(rightrank);
}

/* outershape - compute rank and shape of outer product */
int outershape(shape, leftrank, leftshape, rightrank, rightshape)
union mp_struct *shape;
int leftrank, *leftshape, rightrank, *rightshape;
{	int rank;

	rank = leftrank + rightrank;
	valloc(shape, rank, INT);
	cpvec(shape->ip, leftrank, leftshape);
	cpvec(shape->ip + leftrank, rightrank, rightshape);
	return(rank);
}

/* innershape - compute rank and shape of inner product */
int innershape(shape, leftrank, leftshape, rightrank, rightshape)
union mp_struct *shape;
int leftrank, *leftshape, rightrank, *rightshape;
{	int rank, start;

	rank = 0;
	if (leftrank > 0) rank += leftrank - 1;
	if (rightrank > 0) rank += rightrank - 1;
	valloc(shape, rank, INT);
	if (leftrank > 0) {
		start = leftrank - 1;
		cpvec(shape->ip, start, leftshape);
		}
	else
		start = 0;
	if (rightrank > 0) 
		cpvec(shape->ip + start, rightrank - 1, rightshape + 1);
	return(rank);
}

/* catshape - compute shape for catenate */
int catshape(shape, leftrank, leftshape, rightrank, rightshape)
union mp_struct *shape;
int leftrank, *leftshape, rightrank, *rightshape;
{	int rank;
	int axis;	/* this should be a parameter */

	if (leftrank == 0) {
		if (rightrank == 0) {
			rank = 1;
			valloc(shape, 1, INT);
			*(shape->ip) = 2;
			}
		else {
			rank = rightrank;
			valloc(shape, rightrank, INT);
			cpvec(shape->ip, rank, rightshape);
			/* set axis value -- this needs to be fixed */
			axis = rank-1;
			shape->ip[axis]++;
			}
		}
	else if (rightrank == 0) {
		rank = leftrank;
		valloc(shape, rank, INT);
		cpvec(shape->ip, rank, leftshape);
		/* set axis value -- this needs to be fixed */
		axis = rank-1;
		shape->ip[axis]++;
		}
	else if (leftrank == rightrank) {
		rank = leftrank;
		valloc(shape, rank, INT);
		cpvec(shape->ip, rank, leftshape);
		/* set axis value -- this needs to be fixed */
		axis = rank-1;
		for (rightrank--; rightrank >= 0; rightrank--)
			if (axis == rightrank)
				shape->ip[rightrank] += rightshape[axis];
			else
				if (leftshape[rightrank] != 
					rightshape[rightrank])
					error("cat shape conformabiltiy");
		}
	/* else there are a couple more cases to consider */
	else
		error("catenate conformability error");

	return(rank);
}

/* dtshape - compute shape of dyadic transpose */
int dtshape(shape, lvalues, rrank, rshape)
union mp_struct *shape;
int *lvalues, rrank, *rshape;
{	int rank, i, j, s;

	rank = 0;
	for (i = 0; i < rrank; i++)
		if (lvalues[i] > rank)
			rank = lvalues[i];

	valloc(shape, rank, INT);

	for (i = 1; i <= rank; i++) {
		s = 9999;
		for (j = 0; j < rrank; j++)
			if (lvalues[j] == i)
				if (rshape[j] < s)
					s = rshape[j];
		shape->ip[i - 1] = s;
		}

	return(rank);
}
