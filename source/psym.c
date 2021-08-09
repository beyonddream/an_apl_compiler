/*
	APL compiler
		parser symbol table routines
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

/***********************
	SYMBOL TABLES
************************/
struct symnode *symtab  = NILSYM;	/* local symbols */
struct symnode *gsymtab = NILSYM;	/* global symbols */
struct symnode *fsymtab = NILSYM;	/* functions */

/* initialize local symbol table */
reinitsymtab()
{   
	symtab = 0; 
}

/* looksym - see if a name is in a table */
static struct symnode *looksym(name, table)
char *name;
struct symnode *table;
{
	for (; table != 0; table = table->next) {
		if (strcmp(name, table->name) == 0)
			return(table);
	}
	return(0);
}

/* placeid - place a new id into a symbol table */
static struct symnode *placeid(name, table, type, rank)
char *name;
struct symnode **table;
int type;
int rank;
{
	struct symnode *x;

	x = looksym(name, *table);
	if (x != NILSYM) {
		/* already in table, update attributes */
		if (type != UKTYPE)
			if (x->type != UKTYPE && x->type != type)
				error("conflicting type declaractions");
			else
				x->type = type;
		if (rank != NORANK)
			if (x->vrank != NORANK && x->vrank != rank)
				error("conflicting rank declaractions");
			else
				x->vrank = rank;
	}
	else {
		x = structalloc(symnode);
		if (x == NILSYM)
			error("out of room");
		x->name = name;
		x->type = type;
		x->vrank = rank;
		x->next = *table;
		*table = x;
	}
	return(x);
}

enum classes idclass(name, symptr)
char *name;
struct symnode **symptr;
{  
	struct symnode *p;

	p = looksym(name, symtab);
	if (p) {
		*symptr = p; 
		return(LOCAL);
	}
	p = looksym(name, gsymtab);
	if (p) {
		*symptr = p; 
		return(GLOBAL);
	}
	p = looksym(name, fsymtab);
	if (p) {
		*symptr = p; 
		return(FUNCTION);
	}
	*symptr = NILSYM;
	return(NOCLASS);
}

/* rmglob - remove a global identifier from local symbol table */
rmglob(name, newp)
char *name;
struct symnode *newp;
{  
	struct symnode *oldp;

	oldp = looksym(name, symtab);
	if (oldp != NILSYM) {
		oldp->name = '\0';
		if (oldp->type != UKTYPE)
			newp->type = oldp->type;
		if (oldp->vrank != NORANK)
			newp->vrank = oldp->vrank;
	}
}

/* enterid - enter a new id into symbol table */
struct symnode *enterid(name, class, type, rank)
char *name;
enum classes class;
int  type;
int  rank;
{
	struct symnode *x;

	switch(class) {
	case LABCLASS:
		x = placeid(name, &symtab, LABEL, 0);
		break;
	case PARAM:
		x = placeid(name, &symtab, type, rank);
		break;
	case LOCAL:
		x = placeid(name, &symtab, type, rank);
		break;
	case FUNCTION:
		x = placeid(name, &fsymtab, type, rank);
		rmglob(name, x);
		break;
	case GLOBAL:
		x = looksym(name, gsymtab);
		if (x == NILSYM)
			printf("%c%s\n",GLSYM,name);
		x = placeid(name, &gsymtab, type, rank);
		rmglob(name, x);
		break;
	default:
		yyerror("unknown case in entersym\n");
	}
	return(x);
}
