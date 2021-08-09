/*
	APL Compiler -
		Run Time System
		I/O routines
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
# include <time.h>
# include <stdio.h>
# include <ctype.h>

int stmtno;

/* error - print an error message */
error(s)
char *s;
{
	fprintf(stderr,"statement %d: apl run-time error: \n%s\n", stmtno, s);
}

/*
	printing precision widths
*/
static int intwidth = 5;

/* printit - print an object */
printit(res, type, nls)
union res_struct *res;
int type, nls;
{
	switch(type) {
		case CHAR:
			printf("%c", res->c); break;
		case BIT: case INT:
			printf("%*d", intwidth, res->i); break;
		case REAL:
			printf("%*g", intwidth, res->r); break;
		default:
			error("printit run-time type error");
			break;
		}

	for ( ; nls > 0 ; nls--)
		printf("\n");
}

bmpnl(index, rank, shape)
int index, rank, *shape;
{	int temp, i, counter;

	i = rank - 1;
	counter = 0;
	temp = *(shape + i);
	while (i >= 0)
		if (((index + 1) % temp) == 0) {
			counter++;
			i--;
			temp *= *(shape + i);
			}
		else
			break;
	return(counter);
}


/*
	sysfun - system functions
*/
static int sone[] = {1};

static proint(trs, val)
struct trs_struct *trs;
int val;
{	
	trs->type = INT;
	trs->rank = 0;
	trs->shape = sone;
	valloc(&trs->value, 1, INT);
	*trs->value.ip = val;
}

sysvar(fun, res, left, right)
enum sysvars fun;
struct trs_struct *res, *left, *right;
{	long tmeres, time();
	int i, *tme;
	union mp_struct mptemp;
	char *x, *y;

	switch(fun) {
		default:
			error("unimplemented system function error");

		case EX:
			i = system(right->value.cp);
			proint(res, i);
			break;

		case IO:
			if (right->type != UKTYPE) {
				_ixorg = *(right->value.ip);
				}
			else 
				proint(res, _ixorg);
			break;

		case RL:
			if (right->type != UKTYPE) {
				srand(*(right->value.ip));
				}
			else
				proint(res, rand());
			break;
		case TS:
			tmeres = time(0);
			tme = (int *) localtime(&tmeres);
			res->type = INT;
			res->rank = 1;
			valloc(&mptemp, 1, INT);
			res->shape = mptemp.ip;
			*res->shape = 7;
			valloc(&res->value, 7, INT);
			res->value.ip[0] = tme[5] + 1900;
			res->value.ip[1] = tme[4] + 1;
			res->value.ip[2] = tme[3];
			res->value.ip[3] = tme[2];
			res->value.ip[4] = tme[1];
			res->value.ip[5] = tme[0];
			res->value.ip[6] = 0;
			break;
		}
}

/*
	getnum - read in a single number
*/
static char numstr[30];

static int getnum(file)
FILE *file;
{	int sign = 1;
	int c, type;
	char *p;

	c = fgetc(file);
	while ((c == ' ') || (c == '\t')) {
		c = fgetc(file);
		if ((c == '-') && (sign == 1)) {
			sign = -1;
			c = fgetc(file);
			}
		}
	if (c == EOF)
		error("unexpected end of file");
	if (c == '\n')
		return(UKTYPE);
	type = INT;
	for (p = numstr; isdigit(c); c = fgetc(file))
		*p++ = c;
	if (c == '.') {
		type = REAL;
		*p++ = c;
		for (c = fgetc(file); isdigit(c); c = fgetc(file))
			*p++ = c;
		}
	if ((c == 'e') || (c == 'E')) {
		type = REAL;
		*p++ = c;
		c = fgetc(file);
		if ((c == '+') || (c == '-')) {
			*p++ = c;
			c = fgetc(file);
			}
		for( ; isdigit(c); c = fgetc(file))
			*p++ = c;
		}
	ungetc(c, file);
	*p = '\0';
	return(type);
}

/* quad - input quad */
quad(file, trs)
FILE *file;
struct trs_struct *trs;
{	int i, c, ntype, type;

	printf(".bx:\n");
	c = fgetc(file);
	if (c == '\'') {
		error("character input not implemented yet");
		return;
		}
	ungetc(c, file);
	trs->rank = 0;	/* use the rank as a counter */
	/* allocate fifty places, this should be fixed later */
	valloc(&trs->value, 50, REAL);
	type = INT;
	while ((ntype = getnum(file)) != UKTYPE) 
		switch(ntype) {
			case INT:
				if (type == INT)
					trs->value.ip[trs->rank++] =
						atoi(numstr);
				else
					trs->value.rp[trs->rank++] =
						(double) atoi(numstr);
				break;
			case REAL:
				if (type == INT) {
					/* convert into real */
					for (i = trs->rank-1; i >= 0; i--)
						trs->value.rp[i] =
							trs->value.ip[i];
					trs->value.rp[trs->rank++] =
						atof(numstr);
					type = REAL;
					}
				else 
					trs->value.rp[trs->rank++] =
						atof(numstr);
				break;
			}
	trs->type = type;
	if (trs->rank == 1) {
		trs->rank = 0;
		trs->shape = sone;
		}
	else {	union mp_struct shape;
		valloc(&shape, 1, INT);
		*shape.ip = trs->rank;
		trs->shape = shape.ip;
		trs->rank = 1;
		}
}

