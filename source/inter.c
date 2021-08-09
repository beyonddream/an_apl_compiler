/*
	apl compiler
		intra-procedural dataflow analysis
		timothy a. budd
*/

# include "parse.h"


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
	int maxstmts = 0;
	int newstate, i;

	symtab = syms;
	for (s = code; s != NILSTATE; s = s->nextstate) {
		maxstmts++;
		s->list = makelist(syms);
	}
	currentlist = makelist(syms);
	stmtno = 0;
	s = code;
	while (s != NILSTATE) {
		stmtno++;
		for (i = 0; i < maxstmts; i++)
			branchto[i] = 0;
		merge(currentlist, 1, s->list, 1);
		doinf(s->code, 1);
		newstate = stmtno+1;
		for (i = 0, s2 = code; i < maxstmts; i++, s2 = s2->nextstate) {
			if (merge(currentlist, 0, s2->list, 1) && i < newstate)
				newstate = i;
		}
		s = s->nextstate;
	}
}

/* getsinfo - get current symbol table info for a particular name */
struct symnode *getsinfo(name)
char *name;
{   
	struct symnode *s, *s2;

	for (s = symtab, s2 = currentlist; s != NILSYM; s = s->next, s2 = s2->next)
		if (strcmp(s->name, name) == 0) {
			return(s2);
		}
	return(NILSYM);
}

/* doassign - update the currentlist from an assignment */
doassign(name, type, rank)
char *name;
int type;
int rank;
{  
	struct symnode *s, *s2;
	int maxtype();

	for (s = symtab, s2 = currentlist; s != NILSYM; s = s->next,s2 = s2->next)
		if (strcmp(s->name, name) == 0) {
			s2->type = maxtype(s2->type, type);
			if (s2->vrank == NIL) s2->vrank = rank;
			else if (rank != NIL && rank != s2->vrank) s2->vrank = 10;
		}
}

/* makelist - generate a new local symbol table list */
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

int maxtype(t1, t2)
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

/* merge - merge two symbol table lists together */
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
		maxt = maxtype(t1, t2);
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
		if (r1 == NIL) maxrank = r2;
		else if (r2 == NIL) maxrank = r1;
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


