/*
	apl compiler
		constant management 
			timothy a. budd
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

int giconsts[MAXCONSTS];        /* global integer program constants */
int liconsts[MAXCONSTS];        /* local integer program constants */
int *iconsts;
double grconsts[MAXCONSTS];     /* global float program constants */
double lrconsts[MAXCONSTS];     /* local float program constants */
double *rconsts;
char gsconsts[MAXCONSTS];       /* global string program constants */
char lsconsts[MAXCONSTS];       /* local string program constants */
char *sconsts;

union label_struct lconsts[MAXCONSTS];	/* label constants */

int ictop, gictop;              /* integer constant top */
int rctop, grctop;              /* float constant top */
int sctop, gsctop;              /* string constant top */
int lctop, glctop;              /* label constant top */

setlocalcons() {
	/* save global information gathered so far, and set local
	      pointers up */
	gictop = ictop;
	ictop = 0;
	iconsts = liconsts;
	grctop = rctop;
	rctop = 0;
	rconsts = lrconsts;
	gsctop = sctop;
	sconsts = lsconsts;
	sconsts[0] = '\0';
	lctop = 0;
}

resetconsts() {
	/* reset global information in place */
	ictop = gictop;
	iconsts = giconsts;
	rctop = grctop;
	rconsts = grconsts;
	sctop = gsctop;
	sconsts = gsconsts;
	lctop = 0;
}

/* add a constant to the constant array */
int addicon(x)
int x;
{
	iconsts[ictop++] = x;
	if (ictop > MAXCONSTS)
		error("too many integer constants");
	return(ictop - 1);
}

/* add a double constant to the constant array */
int addrcon(x)
double x;
{
	rconsts[rctop++] = x;
	if (rctop > MAXCONSTS)
		error("too many real constants");
	return(rctop-1);
}

/* addlcon - add a label constant to the constant array */
int addlcon(x)
char *x;
{
	lconsts[lctop++].label_name = x;
	if (lctop > MAXCONSTS)
		error("too many label constants");
	return(lctop - 1);
}

expanivec(ivec, r)
struct node *ivec;
double r;
{
	int i, top;
	double x;

	i = ivec->values.n;
	top = ictop;
	ictop = i;
	ivec->values.n = rctop;
	for (; i < top; i = i + 1) {
		x = iconsts[i];
		addrcon(x);
	}
	addrcon(r);
}

