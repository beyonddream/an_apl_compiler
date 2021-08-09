/*
	APL Compiler

	Utility routines having to do with nodes
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

extern struct headnode head;

/* maxtype - return the maximum type of two types */
int maxtype(ltype, rtype)
int ltype, rtype;
{
	/* should check that types are legal */
	if (rtype > ltype)
		ltype = rtype;
	return(ltype);
}

/* commute - see if operator commutes */
int commute(op)
enum sops op;
{
	if (op == FLOOR || op == CEIL || op == PLUS || op == TIMES ||
	    op == AND || op == OR || op == NAND || op == NOR)
		return(1);
	return(0);
}

/* is_icon - see if a node is an integer constant */
int is_icon(node)
struct node *node;
{
	return((node->nodetype == BCON) ||
		(node->nodetype == ICON));
}

/* is_mergeable - see if a node can be merged into a single accessor */
int is_mergeable(node)
struct node *node;
{	int ntype;

	ntype = node->nodetype;
	if ((ntype == TRANS) || (ntype == DTRANS) || (ntype == TAKE) ||
	(ntype == DROP) || (ntype == REVERSE))
		return(1);
	return(0);
}

/* is_parm - see if a name is a parameter */
int is_parm(name)
char *name;
{
	if (head.asvar != NILCHAR)
		if (strcmp(head.asvar, name) == 0)
			return(1);
	if (head.parm1 != NILCHAR)
		if (strcmp(head.parm1, name) == 0)
			return(1);
	if (head.parm2 != NILCHAR)
		if (strcmp(head.parm2, name) == 0)
			return(1);
	return(0);
}
