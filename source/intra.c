/*
	apl compiler
		intra-procedural dataflow analysis
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
# include <stdio.h>


# define MAXSTMTS 50

short branchto[MAXSTMTS];

static struct symnode *currentlist;
static struct symnode *symtab;

doprog(head, syms, code)
struct headnode *head;
struct symnode *syms;
struct statenode *code;
{  
	struct statenode *s, *s2;
	struct symnode *makelist();
	int countstmts = 0;
	int newstate, i;

	symtab = syms;

	/* associate a copy of the symbol table with each statement */
	for (s = code; s != NILSTATE; s = s->nextstate) {
		countstmts++;
		s->list = makelist(syms);
	}

	/* make a copy of the symbol table corresponding to the cursor */
	currentlist = makelist(syms);

	/* now march through the code, updating the cursor, and 
	make the local versions into the most general form known */
	stmtno = 0;
	s = code;
	while (s != NILSTATE) {
		stmtno++;
		for (i = 0; i < countstmts; i++)
			branchto[i] = 0;
		merge(currentlist, 1, s->list, 1);
		doinf(s->code, 1);
/* at the moment this algorithm is wrong, it just carries information
forward */
		newstate = stmtno+1;
		for (i = 0, s2 = code; i < countstmts; i++, s2 = s2->nextstate) {
			if (merge(currentlist, 0, s2->list, 1) && i < newstate)
				newstate = i;
		}
		s = s->nextstate;
	}
}

/* makelist - generate a copy of the local symbol table list */
struct symnode *makelist(syms)
struct symnode *syms;
{  
	struct symnode *new, *onew;

	new = onew = NILSYM;
	for (; syms != NILSYM; syms = syms->next) {
		new = structalloc(symnode);
		new->next = onew;
		new->type = UKTYPE;
		new->vrank = -1;
		onew = new;
	}
	return(new);
}

/* getsinfo - get current symbol table info for a particular name */
struct symnode *getsinfo(name)
char *name;
{   
	struct symnode *s, *s2;

	for (s = symtab, s2 = currentlist; 
		s != NILSYM; s = s->next, s2 = s2->next)
			if (strcmp(s->name, name) == 0) {
				return(s2);
			}
	return(NILSYM);
}

/* doassign - update the currentlist from an assignment */
doassign(name, node)
char *name;
struct node *node;
{  
	struct symnode *s, *s2;
	int mrgtype();

	for (s = symtab, s2 = currentlist; 
		s != NILSYM; s = s->next,s2 = s2->next)
		if (strcmp(s->name, name) == 0) {
			if (node->info & TYPEKNOWN)
				s2->type = mrgtype(s2->type, node->type.n);
			if (node->info & RANKKNOWN)
				if (s2->vrank == NORANK)
					s2->vrank = node->rank.n;
				else if (node->rank.n >= s2->vrank) 
					s2->vrank = node->rank.n;
		}
}

/* mrgtype - find the most general category for a pair of types */
int mrgtype(t1, t2)
int t1, t2;
{  
	int maxtype;
	int errorflag = 0;

	if (t1 == t2) maxtype = t1;
	else if (t2 == UKTYPE) maxtype = t1;
	else 
	    switch(t1) {
	case BIT: 
	case INT:
		if (t2 == INT || t2 == BIT) maxtype = INT;
		else if (t2 == REAL || t2 == CHAR || t2 == ANY)
			maxtype = ANY;
		else errorflag = 1;
		break;
	case REAL:
		if (t2 == INT || t2 == BIT || t2 == ANY)
			maxtype = ANY;
		else errorflag = 1;
		break;
	case CHAR:
		if (t2 == BIT || t2 == INT || t2 == REAL || t2 == ANY)
			maxtype = ANY;
		else errorflag = 1;
		break;
	case ANY:
		if (t2 == BIT || t2 == INT || t2 == REAL || t2 == CHAR)
			maxtype = ANY;
		else errorflag = 1;
		break;
	case UKTYPE: 
		maxtype = t2; 
		break;
	default: 
		fprintf(stderr,"types are %d %d\n", t1, t2);
		error("impossible condition in maxtype");
	}
	if (errorflag) error("some error in maxtype");
	return(maxtype);
}

/* merge - merge two symbol table lists together,
returning 1 if any changes are noted.
Only update a list if the corresponding ``up'' argument is 1 */

int merge(list1, up1, list2, up2)
struct symnode *list1, *list2;
int up1, up2;
{  
	int errorflag, changed;
	int t1, t2, maxt;
	int  r1, r2, maxrank;

	changed = errorflag = 0;
	for (; list1 != NILSYM ; list1 = list1->next, list2 = list2->next) {
		if (list2 == NILSYM) error("impossible merge condition");
		t1 = list1->type;
		t2 = list2->type;
		maxt = mrgtype(t1, t2);
		if (up1 && t1 != maxt) {
			changed = 1;
			list1->type = maxt;
		}
		if (up2 && t2 != maxt) {
			changed = 1;
			list2->type = maxt;
		}
		r1 = list1->vrank;
		r2 = list2->vrank;
		if (r1 == NORANK) maxrank = r2;
		else if (r2 == NORANK) maxrank = r1;
		else maxrank = (r1 > r2) ? r1 : r2;
		if (up1 && r1 != maxrank) {
			changed = 1;
			list1->vrank = maxrank;
		}
		if (up2 && r2 != maxrank) {
			changed = 1;
			list2->vrank = maxrank;
		}

	}
	return(changed);
}


