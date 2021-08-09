/*
	APL Compiler

	switchbox - control transfer for code generation
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

switchbox(node, mode, top)
struct node *node;
enum pmodes mode;
int top;
{
	switch(node->nodetype) {
		default:
			fprintf(stderr,"unknown node in switch box %d\n",
				node->nodetype);
			exit(1);
			break;

		case ASSIGN:
			genassign(node, mode, top);
			break;

		case AVEC:
			genavec(node, mode, top);
			break;

		case BOX:
			genquad(node, mode, top);
			break;

		case BOXASSIGN:
			genbassign(node, mode, top);
			break;

		case CAT:
			gencat(node, mode, top);
			break;

		case COLLECT:
		case CVEC:
		case CIVEC:
			gencollect(node, mode, top);
			break;

		case COMMENT:
			break;

		case COMPRESS:
			gencompress(node, mode, top);
			break;

		case CSCALAR:
			gencscalar(node, mode, top);
			break;

		case DEAL:
			gendeal(node, mode, top);
			break;

		case DECODE:
		case INNERCHILD:
			gendecode(node, mode, top);
			break;

		case DROP:
			gendrop(node, mode, top);
			break;

		case DSOP:
			gendsop(node, mode, top);
			break;

		case DTRANS:
			gendtrans(node, mode, top);
			break;

		case EPSILON:
			genmember(node, mode, top);
			break;

		case EMPTSEMI:
			genempt(node, mode, top);
			break;

		case EXPAND:
			genexpand(node, mode, top);
			break;

		case FIDENT:
		case SYSVAR:
			genfun(node, mode, top);
			break;

		case GO:
			gengo(node, mode, top);
			break;

		case ICON: case BCON: case RCON: case SCON: case LCON:
			genconst(node, mode, top);
			break;

		case IDENT:
			genident(node, mode, top);
			break;

		case INDEX:
			genindex(node, mode, top);
			break;

		case INNER:
			geninner(node, mode, top);
			break;

		case IOTA:
			geniota(node, mode, top);
			break;

		case MSOP:
			genmsop(node, mode, top);
			break;

		case OUTER:
			genouter(node, mode, top);
			break;

		case RAVEL:
			genravel(node, mode, top);
			break;

		case REDUCE:
			genred(node, mode, top);
			break;

		case RESHAPE:
			genreshape(node, mode, top);
			break;

		case REVERSE:
			genreverse(node, mode, top);
			break;

		case RHO:
			genrho(node, mode, top);
			break;

		case RHORHO:
			genrrho(node, mode, top);
			break;

		case ROLL:
			genroll(node, mode, top);
			break;

		case ROTATE:
			genrotate(node, mode, top);
			break;

		case SCAN:
			genscan(node, mode, top);
			break;

		case SM:
			gensemi(node, mode, top);
			break;

		case SORT:
			gensort(node, mode, top);
			break;

		case SUB:
			gensub(node, mode, top);
			break;

		case TAKE:
			gentake(node, mode, top);
			break;

		case TRANS:
			gentrans(node, mode, top);
			break;
		}
}

