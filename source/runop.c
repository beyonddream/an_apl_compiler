/*
	APL Compiler

	Ryun time system
	routines having to do with scalar functions values
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
# include <math.h>

int _ixorg = 1;

int iabs(i) int i; { return( (i < 0) ? - i : i);}

/* cktype - check to see if a value is of the correct type, converting it
if necessary */
cktype(res, type, giventype)
union res_struct *res;
int type, giventype;
{
	if (type == giventype)
		return;
	switch (type) {
		case BIT:
			if (giventype == INT)
				if ((res->i == 0) || (res->i == 1))
					return;
			error("illegal coversion to bit");

		case INT:
			if (giventype == BIT)
				return;
			error("illegal conversion to int");

		case REAL:
			if ((giventype == INT) || (giventype == INT)) {
				res->r = res->i;
				return;
				}
			error("illegal conversion to real");
		case CHAR:
			error("illegal conversion to char");

		default:
			error("unknown type in cktype");
		}
}

/* identity - compute an identity value */
identity(op, res, type)
enum sops op;
union res_struct *res;
int type;
{
	switch (op) {
		case PLUS: case MINUS: case OR: case LT: case NE:
			switch(type) {
				case BIT: case INT:
					res->i = 0; break;
				case REAL:
					res->r = 0.0; break;
				default:
					error("identity type error");
			}
			break;

		case TIMES: case DIVIDE: case EXP: case AND: case FACT:
		case LE: case EQ:
			switch(type) {
				case BIT: case INT:
					res->i = 1; break;
				case REAL:
					res->r = 1.0; break;
				default:
					error("identity type error");
			}
			break;


		case CEIL:	/* smallest number */
		case FLOOR:	/* largest number */
		default: 
			fprintf(stderr,"operator %d\n", op);
			error("unknown op used in identity");
		}
}

/* dsopv - compute the value of a dyadic scalar function */
dsopv(op, res, lval, ltype, rval, rtype)
enum sops op;
union res_struct *res, *lval, *rval;
int ltype, rtype;
{	int maxtype;

	maxtype = (ltype > rtype) ? ltype : rtype;

	switch(op) {
		case FLOOR:
			cktype(lval, maxtype, ltype);
			cktype(rval, maxtype, rtype);
			switch(maxtype) {
				case CHAR:
					if (rval->c < (res->c = lval->c))
						res->c = rval->c;
					break;
				case BIT: case INT:
					if (rval->i < (res->i = lval->i))
						res->i = rval->i;
					break;
				case REAL:
					if (rval->r < (res->r = lval->r))
						res->r = rval->r;
					break;
				}
			break;

		case CEIL:
			cktype(lval, maxtype, ltype);
			cktype(rval, maxtype, rtype);
			switch(maxtype) {
				case CHAR:
					if (rval->c > (res->c = lval->c))
						res->c = rval->c;
					break;
				case BIT: case INT:
					if (rval->i > (res->i = lval->i))
						res->i = rval->i;
					break;
				case REAL:
					if (rval->r > (res->r = lval->r))
						res->r = rval->r;
					break;
				}
			break;

		case PLUS:
			cktype(lval, maxtype, ltype);
			cktype(rval, maxtype, rtype);
			switch(maxtype) {
				case BIT: case INT:
					res->i = lval->i + rval->i; break;
				case REAL:
					res->r = lval->r + rval->r; break;
				default:
					error("illegal types for op");
			}
			break;

		case MINUS:
			cktype(lval, maxtype, ltype);
			cktype(rval, maxtype, rtype);
			switch(maxtype) {
				case BIT: case INT:
					res->i = lval->i - rval->i; break;
				case REAL:
					res->r = lval->r - rval->r; break;
				default:
					error("illegal types for op");
			}
			break;

		case ABS:
			cktype(lval, maxtype, ltype);
			cktype(rval, maxtype, rtype);
			switch(maxtype) {
				case BIT: case INT:
					res->i = rval->i % lval->i; break;
				default:
					error("type error for abs function");
				}
			break;

		case TIMES:
			cktype(lval, maxtype, ltype);
			cktype(rval, maxtype, rtype);
			switch(maxtype) {
				case BIT: case INT:
					res->i = lval->i * rval->i; break;
				case REAL:
					res->r = lval->r * rval->r; break;
				default:
					error("illegal types for op");
			}
			break;

		case DIVIDE:
			cktype(lval, REAL, ltype);
			cktype(rval, REAL, rtype);
			res->r = lval->r / rval->r; 
			break;

		case EXP:
			cktype(lval, REAL, ltype);
			cktype(rval, REAL, rtype);
			res->r = pow(lval->r, rval->r);
			break;

		case LOG:
			cktype(lval, REAL, ltype);
			cktype(rval, REAL, rtype);
			res->r = log(rval->r) / log(lval->r);
			break;

		case CIRCLE:
			cktype(lval, INT, ltype);
			cktype(rval, REAL, rtype);
			switch(lval->i) {
				default: error("illegal circular function");

				case -4:
					res->r = sqrt((-1) + rval->r*rval->r);
					break;

				case -3:
					res->r = atan(rval->r);
					break;

				case -2:
					res->r = acos(rval->r);
					break;

				case -1:
					res->r = asin(rval->r);
					break;

				case 0:
					res->r = sqrt(1 - rval->r*rval->r);
					break;

				case 1:
					res->r = sin(rval->r);
					break;

				case 2:
					res->r = cos(rval->r);
					break;

				case 3:
					res->r = sin(rval->r) / cos(rval->r);
					break;

				case 4:
					res->r = sqrt(1 + rval->r*rval->r);
					break;

				case 5:
					res->r = sinh(rval->r);
					break;

				case 6:
					res->r = cosh(rval->r);
					break;

				case 7:
					res->r = tanh(rval->r);
					break;
				}
			break;

		case AND:
			cktype(lval, BIT, ltype);
			cktype(rval, BIT, rtype);
			res->i = (lval->i && rval->i);
			break;

		case OR:
			cktype(lval, BIT, ltype);
			cktype(rval, BIT, rtype);
			res->i = (lval->i || rval->i);
			break;

		case NAND:
			cktype(lval, BIT, ltype);
			cktype(rval, BIT, rtype);
			res->i = ! (lval->i && rval->i);
			break;

		case NOR:
			cktype(lval, BIT, ltype);
			cktype(rval, BIT, rtype);
			res->i = ! (lval->i || rval->i);
			break;

		case LT:
			cktype(lval, maxtype, ltype);
			cktype(rval, maxtype, rtype);
			switch(maxtype) {
				case CHAR:
					res->i = lval->c < rval->c; break;
				case BIT: case INT:
					res->i = lval->i < rval->i; break;
				case REAL:
					res->i = lval->r < rval->r; break;
				default:
					error("illegal types for op");
			}
			break;

		case LE:
			cktype(lval, maxtype, ltype);
			cktype(rval, maxtype, rtype);
			switch(maxtype) {
				case CHAR:
					res->i = lval->c <= rval->c; break;
				case BIT: case INT:
					res->i = lval->i <= rval->i; break;
				case REAL:
					res->i = lval->r <= rval->r; break;
				default:
					error("illegal types for op");
			}
			break;

		case EQ:
			cktype(lval, maxtype, ltype);
			cktype(rval, maxtype, rtype);
			switch(maxtype) {
				case CHAR:
					res->i = lval->c == rval->c; break;
				case BIT: case INT:
					res->i = lval->i == rval->i; break;
				case REAL:
					res->i = lval->r == rval->r; break;
				default:
					error("illegal types for op");
			}
			break;

		case NE:
			cktype(lval, maxtype, ltype);
			cktype(rval, maxtype, rtype);
			switch(maxtype) {
				case CHAR:
					res->i = lval->c != rval->c; break;
				case BIT: case INT:
					res->i = lval->i != rval->i; break;
				case REAL:
					res->i = lval->r != rval->r; break;
				default:
					error("illegal types for op");
			}
			break;

		
		case GT:
			cktype(lval, maxtype, ltype);
			cktype(rval, maxtype, rtype);
			switch(maxtype) {
				case CHAR:
					res->i = lval->c > rval->c; break;
				case BIT: case INT:
					res->i = lval->i > rval->i; break;
				case REAL:
					res->i = lval->r > rval->r; break;
				default:
					error("illegal types for op");
			}
			break;

		case GE:
			cktype(lval, maxtype, ltype);
			cktype(rval, maxtype, rtype);
			switch(maxtype) {
				case CHAR:
					res->i = lval->c >= rval->c; break;
				case BIT: case INT:
					res->i = lval->i >= rval->i; break;
				case REAL:
					res->i = lval->r >= rval->r; break;
				default:
					error("illegal types for op");
			}
			break;

		default: error("unknown op used in dsopv");
		}
}

/*
	msopv - compute the value of a monadic scalar function */
msopv(op, res, type)
enum sops op;
union res_struct *res;
int type;
{
	switch(op) {
		case NOT:
			cktype(res, BIT, type);
			res->i = ! res->i; 
			break;

		case FLOOR:
			cktype(res, REAL, type);
			res->i = (int) floor(res->r);
			break;

		case CEIL:
			cktype(res, REAL, type);
			res->i = (int) ceil(res->r);
			break;

		case PLUS:
			break;

		case MINUS:
			switch(type) {
				case BIT: case INT:
					res->i = - res->i; break;
				case REAL:
					res->r = - res->r; break;
				}
			break;

		case TIMES:
			switch(type) {
				case BIT: case INT:
					if (res->i < 0) res->i = -1;
					else if (res->i == 0) res->i = 0;
					else res->i = 1;
					break;
				case REAL:
					if (res->r < 0.0) res->i = -1;
					else if (res->i == 0.0) res->i = 0;
					else res->i = 1;
					break;
				}
			break;

		case ABS:
			switch(type) {
				case BIT: case INT:
					if (res->i < 0) res->i = - res->i;
					break;
				case REAL:
					if (res->r < 0) res->r = - res->r;
					break;
				}
			break;

		case DIVIDE:
			cktype(res, REAL, type);
			res->r = 1.0 / res->r;
			break;


		case EXP:
			cktype(res, REAL, type);
			res->r = exp(res->r);
			break;

		case LOG:
			cktype(res, REAL, type);
			res->r = log(res->r);
			break;

		default: 
			fprintf(stderr," operator %d\n", op);
			error("unimplemented op used in msopv");
		}
}

/*
	sorting routines */

static int stype, sdir;
static union mp_struct *svals;

static int aplcomp(i, j)
int *i, *j;
{	int r;

	switch(stype) {
		case BIT: case INT:
			if (svals->ip[*i] == svals->ip[*j]) r = 0;
			else if (svals->ip[*i] < svals->ip[*j]) r = -1;
			else r = 1;
			break;
		case REAL:
			if (svals->rp[*i] == svals->rp[*j]) r = 0;
			else if (svals->rp[*i] < svals->rp[*j]) r = -1;
			else r = 1;
			break;
		case CHAR:
			if (svals->cp[*i] == svals->cp[*j]) r = 0;
			else if (svals->cp[*i] < svals->cp[*j]) r = -1;
			else r = 1;
			break;
		default:
			r = 1;
		}
	return(sdir * r);
}

aplsort(result, values, size, type, direction)
union mp_struct *result, *values;
int size, type, direction;
{	int i;

	valloc(result, size, INT);
	for (i = 0; i < size; i++)
		result->ip[i] = i;
	stype = type;
	svals = values;
	sdir = direction;
	qsort((char *) result->ip, size, sizeof(int), aplcomp);
}

/*
	this should be optimized to use binary search,
	check types only once (instead of each iteration through loop),
	but this will work for now
*/
int aplsearch(index, values, result, type, size)
int *index, type, size;
union mp_struct *values;
union res_struct *result;
{	int i;
	register int step, position;

	for (step = 1; 2 * step <= size; step *= 2) ;
	position = step - 1;

	switch (type) {
		case CHAR:
			do {
				step /= 2;
				if (values->cp[index[position]] ==
					result->i) return(index[position]);
				else if (values->cp[index[position]] <
					result->i) {
						position += step;
						if (position >= size)
							position = size-1;
						}
				else position -= step;
				} while (step != 0);
			break;
		case INT: case BIT:
			do {
				step /= 2;
				if (values->ip[index[position]] ==
					result->i) return(index[position]);
				else if (values->ip[index[position]] <
					result->i) {
						position += step;
						if (position >= size)
							position = size-1;
						}
				else position -= step;
				} while (step != 0);
			break;
		case REAL:
			do {
				step /= 2;
				if (values->rp[index[position]] ==
					result->i) return(index[position]);
				else if (values->rp[index[position]] <
					result->i) {
						position += step;
						if (position >= size)
							position = size-1;
						}
				else position -= step;
				} while (step != 0);
			break;
		}
	return(size + 1);
}

/*
	roll and deal functions
*/

randint(limit)
int limit;
{
	return( _ixorg + (rand() / 100) % limit );
}

deal(values, num, val)
union mp_struct *values;
int num, val;
{	int i, j, k;

	for (i = 0; i < num; i++) {
		j = 1;
		while (j == 1) {
			values->ip[i] = randint(val);
			j = 0;
			for (k = 0; k < i; k++)
				if (values->ip[i] == values->ip[k])
					j = 1;
			}
		}
}
