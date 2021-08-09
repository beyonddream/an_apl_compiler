/* 
	APL compiler
		utilities common to all passes of compiler
		timothy a. budd
*/

# include "parse.h"
# include "y.tab.h"
# include <stdio.h>

extern int stmtno;
extern int *iconsts;
extern int ictop;

/* impossible case error */
caserr(where, num)
char *where;
int num;
{
	fprintf(stderr,"case error in %s. case = %d\n",where,num);
	fprintf(stderr,"statement number %d\n",stmtno);
	exit(1);
}

/* cant - can't happen */
cant (where)
char *where;
{
	fprintf(stderr,"can't happen: %s\n",where);
	exit(1);
}

/* mergetop - is node at top of merge tree fragment */
int mergetop(node)
struct node *node;
{
	/*if ( !(node->info & MERGE))
		return(1);
	else
		return(0);*/
}

/* mergebot - is node at bottom of merge tree fragment */
int mergebot(node)
struct node *node;
{
	/*if ( !(RIGHT->info & MERGE))
		return(1);
	else
		return(0);*/
}
/*
	numin - return the number of elements in a structure with
	known shape
*/

int numin(node)
struct node *node;
{	int i, r, s, size;

	r = node->rank.n;
	s = node->shape.n;

	size = 1;
	for (i = 0; i < r; i++)
		size *= iconsts[s + i];
	return(size);
}

