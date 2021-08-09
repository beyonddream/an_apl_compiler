/*
	APL Compiler

	Run time routines - Memory Allocation / TRS manipulation
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

/* assign - assign a trs structure to an identifier */
assign(id, trs)
struct trs_struct *id, *trs;
{
	id->type = trs->type;
	id->rank = trs->rank;
	id->shape = trs->shape;
	id->value = trs->value;
}

/* settrs - set values into a trs structure */
settrs(trs, type, rank, shape)
struct trs_struct *trs;
int type, rank, *shape;
{
	trs->type = type;
	trs->rank = rank;
	trs->shape = shape;
}

/* getmp - get a value from storage for a variable */
getmp(res, mp, i, type)
union res_struct *res;
union mp_struct *mp;
int i, type;
{
	switch (type) {
		case CHAR:
			res->c = *(mp->cp + i); break;
		case BIT: case INT:
			res->i = *(mp->ip + i); break;
		case REAL:
			res->r = *(mp->rp + i); break;
		default:
			error("impossible condition in getmp");
		}
}

/* setmp - get a value into storage for a variable */
setmp(res, mp, i, type)
union res_struct *res;
union mp_struct *mp;
int i, type;
{
	switch (type) {
		case CHAR:
			*(mp->cp + i) = res->c; break;
		case BIT: case INT:
			*(mp->ip + i) = res->i; break;
		case REAL:
			*(mp->rp + i) = res->r; break;
		default:
			fprintf(stderr,"type %d\n", type);
			error("impossible condition in setmp");
		}
}

/* valloc - allocate a vector - main memory allocation interface */
valloc(mp, size, type)
union mp_struct *mp;
int size, type;
{	char *c, *malloc();

	switch(type) {
		case CHAR:
			mp->cp = c = malloc(size);
			break;

		case BIT: case INT:
			mp->ip = (int *) (c = malloc(size * sizeof(int)));
			break;

		case REAL:
			mp->rp = (double *) (c = malloc(size * sizeof(double)));
			break;

		default:
			error("impossible case in valloc");
			break;
		}
	if (c == (char *) 0)
		error("out of memory allocation space");
}

/* vsize - compute the size of an object */
int vsize(rank, shape)
int rank, *shape;
{	register int size;

	size = 1;
	for (rank--; rank >= 0; rank--)
		size *= *(shape + rank);
	return(size);
}

/* talloc - allocate storage in a trs structure */
int talloc(trs)
struct trs_struct *trs;
{	int size;

	size = vsize(trs->rank, trs->shape);
	valloc(&trs->value, size, trs->type);
	return(size);
}

/* cpvec - copy an integer vector */
cpvec(to, size, from)
int size;
register int *to, *from;
{
	for (; size; size--)
		*to++ = *from++;
}

/* cpwo - copy an integer vector without a specific element */
cpwo(to, wo, size, from)
int size, wo;
register int *to, *from;
{	int i;

	for (i = 0; i < size; i++)
		if (i != wo)
			*to++ = *from++;
}

/* esubi - return the i'th element in the expansion vector */
int esubi(i, rank, shape)
int i, *shape, rank;
{	register int val;

	val = 1;
	for (rank--; rank > i; rank--)
		val *= *(shape + rank);
	return(val);
}
