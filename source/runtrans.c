/*
	APL Compiler

	run time system -
	routines having to do with universal accessor/ merged steppers
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

/* qsdalloc - allocate the q, s and d vectors for a stepper */
int qsdalloc(rank, q, s, d)
int rank;
union mp_struct *q, *s, *d;
{	int i;

	valloc(q, rank, INT);
	valloc(s, rank, INT);
	valloc(d, rank, INT);
	for (i = rank-1; i >= 0; i--) {
		q->ip[i] = i;
		s->ip[i] = 0;
		d->ip[i] = 1;
		}
	return(rank);
}

/* accessor - construct the accessor values for a stepper */
int accessor(toprank, topshape, rank, shape, delta, q, s, d)
int toprank, *topshape, rank, *shape, *q, *s, *d;
union mp_struct *delta;
{	int e, alpha, i;

	e = 1; alpha = 0;
	valloc(delta, toprank, INT);
	for (i = rank - 1; i >= 0; i--) {
		delta->ip[i] = 0;
		}
	for (i = rank - 1; i >= 0; i--) {
		delta->ip[q[i]] += d[i] * e;
		alpha += s[i] * e;
		e *= shape[i];
		}
	for (i = 0; i < toprank-1; i++) {
		delta->ip[i] -= delta->ip[i+1] * topshape[i+1];
		}
	return(alpha);
}

/* trmerge - merge the information from a monadic transpose */
trmerge(rank, q, s, d)
int rank, *q, *s, *d;
{	int i, j, k;

	for (i = rank/2 - 1; i >= 0; i--) {
		j = (rank - i) - 1;
		k = *(q +i);
		*(q +i) = *(q + j);
		*(q +j) = k;
		k = *(s + i);
		*(s + i) = *(s + j);
		*(s + j) = k;
		k = *(d + i);
		*(d + i) = *(d + j);
		*(d + j) = k;
		}
}

/* dtmerge - dyadic transpose merge */
dtmerge(control, rank, q, s, d)
int *control, rank;
union mp_struct *q, *s, *d;
{	int i, j, *od, *os, *oq;

	oq = q->ip;
	os = s->ip;
	od = d->ip;
	valloc(q, rank, INT);
	valloc(s, rank, INT);
	valloc(d, rank, INT);
	for (i = 0; i < rank; i++) {
		j = *(control + i) - 1;
		q->ip[i] = oq[j];
		s->ip[i] = os[j];
		d->ip[i] = od[j];
		}
}

