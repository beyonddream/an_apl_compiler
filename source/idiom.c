/*
	APL Compiler

	Idiom recognition
*/
# include "parse.h"
# include "y.tab.h"
# include <stdio.h>

extern struct node *pt1(), *pt2();

/* ptgo - recognize the following goto idioms
	
	-> label X i condition
*/
struct node *ptgo(child)
struct node *child;
{	struct node *right, *x;

	if ((child->nodetype == DSOP) && (child->optype == TIMES)) {
		right = child->right;
		if (right->nodetype == IOTA) {
			right = right->right;
			return(pt2(CGOTO, right, child->left));
			}
		}
	return(pt1(GO, child));
}


