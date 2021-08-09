/*
	APL Compiler

	code tree manipulations
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
# include <stdio.h>

extern char *funname;
extern int  indxorgin;

/*
	define various strings used to print out code trees
*/

static char *ctstrs[] = {"type", "rank", "shape", "value"};

static char *tfstrs[] = {"uck", "i", "i", "r", "c", "uck", "l"};

static char *caststrs[] = {"UCK", "int", "int", "double", "char",
	"UCK"};

static char *tystrs[] = {"UKTYPE", "BIT", "INT", "REAL", "CHAR", "ANY",
				"LABEL"};

extern int resinreg();

/* mkrktype - make sure a result register is of a given known type,
generating code to make it so if necessary */
mkrktype(node, ktype, resval)
struct node *node;
int ktype, resval;
{	int sreg, rt;

	if (node->info & TYPEKNOWN) {
		rt = rtype(node);
		if ((rt == BIT) && (ktype == INT))
			return;
		if (rt != ktype) {
			node->values.c = gcast(castop, node->values.c, ktype);
			}
		}
	else {
		sreg = resinreg(node, resval);
		printf("cktype(&res%d, %s, ", sreg, tystrs[ktype]);
		ctgen(node->type.c);
		rpseminl();
		node->values.c = gicn(resptr, sreg, ktype);
		}
}

/*
	test trees for various properties
*/

static struct codetree *lastttest = 0;
static struct codetree *lastrtest = 0;

testtype(tree, type)
struct codetree *tree;
int type;
{
	if (cteq(tree, lastttest))
		return;
	lastttest = tree;
	iflp();
	ctgen(tree);
	printf(" != %s) ", tystrs[type]);
	prerror("type error");
}

testrank(tree, rank)
struct codetree *tree;
int rank;
{
	if (cteq(tree, lastrtest))
		return;
	lastrtest = tree;
	iflp();
	ctgen(tree);
	printf(" != %d) ", rank);
	prerror("rank error");
}

/*
	cteq - test two trees for equality
*/
int cteq(t1, t2)
struct codetree *t1, *t2;
{
	if ((! t1) || (! t2))
		return(0);

	if (t1->cop != t2->cop)
		return(0);

	switch(t1->cop) {
		default: break;

		case icnst: case iptr: case resptr: 
		case tcnst:
			if (t1->c0.cindex == t2->c0.cindex)
				return(1);
			break;

		case idptr:
			if (strcmp(t1->c0.cident, t2->c0.cident) == 0)
				return(1);
			break;

		case trsptr:
			if ((t1->c0.cindex == t2->c0.cindex) &&
				(t1->c1.ctfield == t2->c1.ctfield) &&
				(t1->c2.ctype == t2->c2.ctype))
				return(1);
			break;
		}
	return(0);
}
/*ctdsop - generate code for a dyadic scalar op tree */


static char *ddsops[] = {"", 
"", "", " + ", " - ", " * ", " % ", " / ", "", "", "", "",
" & ", " | ", " & ", " | ", " < ", " <= ", " == ", " != ", " >= ", " > "};


static ctdsop(op, lefttree, righttree)
enum sops op;
struct codetree *lefttree, *righttree;
{
	switch(op) {
		case PLUS:
			if ((righttree->cop == icnst) && 
				(righttree->c0.cindex < 0)) {
					ctgen(lefttree);
					printf(" - %d", 
						- righttree->c0.cindex);
					break;
					}

		case MINUS: case TIMES: case DIVIDE:
		case AND: case OR:
		case LT: case LE: case EQ: case NE: case GE: case GT:
			ctgen(lefttree);
			fputs(ddsops[(int) op], stdout);
			ctgen(righttree);
			break;

		case ABS:
			ctgen(righttree);
			fputs(ddsops[(int) op], stdout);
			ctgen(lefttree);
			break;

		case NAND: case NOR:
			fputs("!(", stdout);
			ctgen(lefttree);
			fputs(ddsops[(int) op], stdout);
			ctgen(righttree);
			rp();
			break;
	}
}

/*ctmsop - generate code for monadic scalar functions */

static char *dmsops[] = {"! ",
"(int) floor(", "(int) ceil(", "", "- ", "", "", "(1.0 / ", 
"exp(", "log(", "(PI * ", ""};

static ctmsop(op, child)
enum sops op;
struct codetree *child;
{
	switch(op) {
		default: error("unimplemented ctmsop");

		case NOT: case MINUS:
			fputs(dmsops[(int) op], stdout);
			ctgen(child);
			break;

		case FLOOR: case CEIL: case DIVIDE:
		case EXP: case LOG: case CIRCLE:
			fputs(dmsops[(int) op], stdout);
			ctgen(child);
			fputs(")", stdout);
			break;
	}
}

/*ctgen - generate code for a code tree */
ctgen(tree)
struct codetree *tree;
{
	switch(tree->cop) {
		default:
			fprintf(stderr,"unknown field in ctgen = %d\n",
					tree->cop);
			break;

		case asgn:
			lp();
			ctgen(tree->c0.cleft);
			printf(" = ");
			ctgen(tree->c1.cright);
			rp();
			break;

		case cnst:
			printf("&%s_%s[", tfstrs[tree->c2.ctype], funname);
			ctgen(tree->c0.cleft);
			printf("]");
			break;

		case castop:
			printf("((%s) ", caststrs[tree->c2.ctype]);
			ctgen(tree->c0.cleft);
			rp();
			break;

		case condop:
			lp();
			ctgen(tree->c0.cleft);
			printf("?");
			ctgen(tree->c2.cmiddle);
			printf(":");
			ctgen(tree->c1.cright);
			rp();
			break;

		case deref:
			if ((tree->c0.cleft)->cop == cnst) {
				tree = tree->c0.cleft;
				printf("%s_%s[", tfstrs[tree->c2.ctype],
					funname);
				ctgen(tree->c0.cleft);
				printf("]");
				break;
				}
			printf("*");
			ctgen(tree->c0.cleft);
			break;

		case dsop:
			lp();
			ctdsop(tree->c2.csop, tree->c0.cleft, tree->c1.cright);
			rp();
			break;

		case icnst:
			printf("%d", tree->c0.cindex);
			break;

		case idptr:
			printf("%s%s%s", tree->c0.cident, 
				(is_parm(tree->c0.cident) ? "->" : "."),
				ctstrs[(int) tree->c1.ctfield]);
			if ((tree->c1.ctfield == cvalfield) && 
				(tree->c2.ctype != UKTYPE))
				printf(".%sp", tfstrs[tree->c2.ctype]);
			break;

		case iptr:
			printf("i%d", tree->c0.cindex);
			break;

		case ixorgin:
			printf("_ixorg");
			break;

		case memptr:
			if (tree->c2.ctype != UKTYPE)
				printf("mp%d.%sp", tree->c0.cindex, 
					tfstrs[tree->c2.ctype]);
			else
				printf("mp%d", tree->c0.cindex);
			break;

		case msop:
			lp();
			ctmsop(tree->c2.csop, tree->c0.cleft);
			rp();
			break;

		case postinc:
			ctgen(tree->c0.cleft);
			printf("++ ");
			break;

		case resptr:
			printf("res%d.%s", tree->c0.cindex, 
				tfstrs[tree->c2.ctype]);
			break;

		case tcnst:
			printf("%s", tystrs[tree->c0.cindex]);
			break;

		case trsptr:
			printf("trs%d.%s", tree->c0.cindex, 
				ctstrs[(int) tree->c1.ctfield]);
			if ((tree->c1.ctfield == cvalfield) && 
				(tree->c2.ctype != UKTYPE))
				printf(".%sp", tfstrs[tree->c2.ctype]);
			break;

		case trsvptr:
			printf("trs%d.value.%sp", tree->c0.cindex,
				tfstrs[tree->c2.ctype]);
			break;
		}
}

/* gcnode - generate a new code tree node */
struct codetree *gcnode(cop)
enum codeops cop;
{	struct codetree *x;

	x = structalloc(codetree);
	x->cop = cop;
	return(x);
}

/* gicn - generate an integer constant type of node */
struct codetree *gicn(cop, val, typ)
enum codeops cop;
int val, typ;
{	struct codetree *x;

	x = gcnode(cop);
	x->c0.cindex = val;
	x->c2.ctype = typ;
	return(x);
}

/* gixorg - generate the index origin */
struct codetree *gixorg()
{
	if (indxorgin == DEFAULTINDEX)
		return(gicn(ixorgin, 1, INT));
	else
		return(gicn(icnst, indxorgin, INT));
}

/* gcast - generate a cast-like node */
struct codetree *gcast(cop, cnode, ctype)
enum codeops cop;
struct codetree *cnode;
int ctype;
{	struct codetree *x;

	x = gmon(cop, cnode);
	x->c2.ctype = ctype;
	return(x);
}

/* gbin - generate a node with two subtrees */
struct codetree *gbin(cop, cnode1, cnode2)
enum codeops cop;
struct codetree *cnode1, *cnode2;
{	struct codetree *x;

	x = gcnode(cop);
	x->c0.cleft = cnode1;
	x->c1.cright = cnode2;
	return(x);
}

/* gtrs - generate a trs type node */
struct codetree *gtrs(trsval, tfield, ttype)
int trsval, ttype;
enum tfields tfield;
{	struct codetree *x;

	x = gcnode(trsptr);
	x->c0.cindex = trsval;
	x->c1.ctfield = tfield;
	x->c2.ctype = ttype;
	return(x);
}

struct codetree *gident(name, field, ttype)
char *name;
enum tfields field;
int ttype;
{	struct codetree *x;

	x = gcnode(idptr);
	x->c0.cident = name;
	x->c1.ctfield = field;
	x->c2.ctype = ttype;
	return(x);
}

/* gcond - generate a conditional tree */
struct codetree *gcond(cond, left, right)
struct codetree *cond, *left, *right;
{	struct codetree *x;
	int val;

	if (cond->cop == icnst) {
		val = cond->c0.cindex;
		if (val)
			return(left);
		else
			return(right);
		}
	x = gcnode(condop);
	x->c0.cleft = cond;
	x->c2.cmiddle = left;
	x->c1.cright = right;
	return(x);
}

struct codetree *gmop(mop, child)
struct codetree *child;
enum sops mop;
{	struct codetree *x;

	x = gmon(msop, child);
	x->c2.csop = mop;
	return(x);
}

/*
	gsop - generate a scalar operator type of node */
struct codetree *gsop(sop, cnode1, cnode2)
enum sops sop;
struct codetree *cnode1, *cnode2;
{	struct codetree *x, *l;
	int i, j;

	if ((cnode1->cop == icnst) && (cnode2->cop == icnst)) {
		i = cnode1->c0.cindex;
		j = cnode2->c0.cindex;
		switch (sop) {
			case PLUS:  return(gicn(icnst, i+j, INT));
			case MINUS: return(gicn(icnst, i-j, INT));
			case TIMES: return(gicn(icnst, i*j, INT));
			}
		}
	if ((sop == PLUS) || (sop == MINUS)) {
		if ((cnode2->cop == icnst) && (cnode2->c0.cindex == 0))
			return(cnode1);
		if (cnode1->cop == cnst) {
			return(gcast(cnst, 
				gsop(sop, cnode1->c0.cleft, cnode2),
				cnode1->c2.ctype));
			}
		}
	if ((sop == MINUS) && (cnode2->cop == icnst))
		return(gsop(PLUS, cnode1, 
			gicn(icnst, - cnode2->c0.cindex, INT)));
	if (sop == PLUS) {
		if (cnode1->cop == icnst)
			return(gsop(PLUS, cnode2, cnode1));
		if ((cnode1->cop == dsop) && (cnode1->c2.csop == PLUS)) {
			if ((cnode1->c1.cright)->cop == icnst) {
				i = (cnode1->c1.cright)->c0.cindex;
				l = cnode1->c0.cleft;
				if (cnode2->cop == icnst)
					return(gsop(PLUS, l, 
						gicn(icnst, 
						i + cnode2->c0.cindex, INT)));
				return(gsop(PLUS,
					gsop(PLUS, l, cnode2),
					gicn(icnst, i, INT)));
				}
			}
		else if ((cnode2->cop == dsop) && (cnode2->c2.csop == PLUS))
			return(gsop(PLUS, cnode2, cnode1));
		}
	if (sop == TIMES) {
		if (cnode1->cop == icnst)
			return(gsop(TIMES, cnode2, cnode1));
		if (cnode2->cop == icnst) {
			i = cnode2->c0.cindex;
			if (i == 1)
				return(cnode1);
			if ((cnode1->cop == dsop) &&
				(cnode1->c2.csop == PLUS) &&
				((cnode1->c1.cright)->cop == icnst)) {
				j = (cnode1->c1.cright)->c0.cindex;
				return(gsop(PLUS, 
					gsop(TIMES, cnode1->c0.cleft, cnode2),
					gicn(icnst, j * i, INT)));
				}
			}
		}
	x = gbin(dsop, cnode1, cnode2);
	x->c2.csop = sop;
	return(x);
}

/*
	gmon - generate a monadic operator type of node */
struct codetree *gmon(cop, cnode)
enum codeops cop;
struct codetree *cnode;
{	struct codetree *x;

	if ((cop == deref) && (cnode->cop == cnst))
		if ((cnode->c2.ctype == 1) || (cnode->c2.ctype == 2))
		 if ((cnode->c0.cleft)->cop == icnst) {
		return(gicn(icnst, iconsts[(cnode->c0.cleft)->c0.cindex],
				INT));
		}
	x = gcnode(cop);
	x->c0.cleft = cnode;
	return(x);
}

