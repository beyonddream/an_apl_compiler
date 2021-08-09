/*
	APL compiler
		type / rank / shape inferencer
		timothy a. budd
*/

# include <math.h>
# include "parse.h"
# include "y.tab.h"
# include <stdio.h>

# define lrinf(left, right) {doinf(left, 0); doinf(right, 0);}
# define is_scalar(node) ((node->info & RANKKNOWN) && (node->rank.n == 0))

extern int *iconsts;
extern double *rconsts;
extern int ictop;

extern int stmtno;
extern int indxorgin;

/* passinit - initialize this pass (do nothing) */
passinit()
{;}

/* glbvar - process a global variable name */
glbvar(name)
char *name;
{;}

# ifndef INTRA
/* doprog - process function */
doprog(head, syms, code)
struct headnode *head;
struct symnode *syms;
struct statenode *code;
{  
	struct statenode *p;

	stmtno = 0;
	for (p = code ; p != NILSTATE; p = p->nextstate) {
		stmtno++;
		doinf(p->code, 1);
	}
	fresyms(syms);
	frecode(code);
}
# endif
/*
	typemake
		can an object of fromtype be made into an object of
		totype
*/
static int typemake(totype, fromtype)
int totype, fromtype;
{  

	if (totype == UKTYPE || totype == ANY) return(1);
	if (fromtype == UKTYPE || fromtype == ANY) return(1);
	if (totype == fromtype) return(1);

switch(totype) {
	case BIT: 
		break;
	case INT: 
		if (fromtype == BIT) return(1); 
		break;
	case REAL: 
		if (fromtype == BIT || fromtype == INT) return(1); 
		break;
	case CHAR: 
		break;
	case ANY:  
		return(1);
	case UKTYPE: 
		return(1);
	case LABEL: 
		break;
	}
	return(0);
}

/* ranktest - see if a node is a given rank */
static int ranktest(node, rank)
struct node *node;
int rank;
{
	if ((node->info & RANKKNOWN) && (node->rank.n == rank))
		return(1);
	return(0);
}

/*
	dsrtype
		type checking for dyadic scalar ops
*/
static dsrtype(op, node, left, right)
enum sops op;
struct node *node, *left, *right;
{	int ltype, rtype;

	if (left->info & TYPEKNOWN)
		ltype = left->type.n;
	else
		ltype = UKTYPE;
	if (right->info & TYPEKNOWN)
		rtype = right->type.n;
	else
		rtype = UKTYPE;

	switch(op) {
		case AND: case OR: case NAND: case NOR:
			if ((! typemake(BIT, ltype)) &&
				(! typemake(BIT, rtype)))
				error("illegal types for logical operator");
			node->info |= TYPEKNOWN;
			node->type.n = BIT;
			break;

		case LT: case LE: case EQ: case NE: case GE: case GT:
			if ((! typemake(ltype, rtype)) &&
				(! typemake(rtype, ltype))) {
				error("illegal types for comparison operator");
				}
			node->info |= TYPEKNOWN;
			node->type.n = BIT;
			break;

		case NOT:   /* used to represent catenation */
		case PLUS: case MINUS: case TIMES: case ABS: case CEIL:
		case FLOOR:
			if ((! typemake(ltype, rtype)) &&
				(! typemake(rtype, ltype))) {
				fprintf(stderr,"types %d %d\n", ltype, rtype);
				error("illegal types for arithmetic operator");
				}
			if ((ltype != UKTYPE) && (rtype != UKTYPE)) {
				node->type.n = maxtype(ltype, rtype);
				node->info |= TYPEKNOWN;
				if (op == NOT) break;
				if (node->type.n == BIT)
					node->type.n = INT;
				}
			break;

		case DIVIDE: case EXP: case LOG: case FACT: case CIRCLE:
			if ((! typemake(REAL, ltype)) &&
				(! typemake(REAL, rtype)))
				error("illegal types for logical operator");
			node->info |= TYPEKNOWN;
			node->type.n = REAL;
			break;

		default:
			fprintf(stderr,"dyadic scalar operator %d not done\n",
				op);
			exit(1);
	}
}

/*
	infaxis - initilize axis node */

static infaxis(node)
struct node *node;
{	struct node *axis;
	int val;

	if ((node->info & FIRSTAXIS) || (node->info & LASTAXIS))
		;
	else {
		axis = node->a.axis;
		/* the fact that it is a cscalar will fix things */
		doinf(axis, 0);
		/* if we know the axis, perhaps we can do thing better */
		if ((indxorgin != DEFAULTINDEX) && 
			(axis->info & HAVEVALUE)) {
			val = iconsts[axis->values.n] - indxorgin;
			if ((node->info & RANKKNOWN) &&
				(val == node->rank.n))
				node->info |= LASTAXIS;
			else if (val == 0)
				node->info |= FIRSTAXIS;
			}
		}
}

/*
	copychild - copy selected attributes from another node
*/
static copychild(node, child, type, rank, shape, value)
struct node *node, *child;
int type, rank, shape, value;
{
	if (type && (child->info & TYPEKNOWN)) {
		node->info |= TYPEKNOWN;
		node->type.n = child->type.n;
		}
	if (rank && (child->info & RANKKNOWN)) {
		node->info |= RANKKNOWN;
		node->rank.n = child->rank.n;
		}
	if (shape && (child->info & SHAPEKNOWN)) {
		node->info |= SHAPEKNOWN;
		node->shape.n = child->shape.n;
		}
	if (value && (child->info & VALUESKNOWN)) {
		node->info |= VALUESKNOWN;
		node->values.n = child->values.n;
		}
}

/*
	doinf - node inferencer
	
*/
doinf(node, top)
struct node *node;
int top;
{
	int cksrtype(), maxtype();
	enum sysvars optype;
	int i, j;
	int sh, rk, rt, c;
# ifdef INTRA
	struct symnode *s, *getsinfo();
	int mrgtype();
# endif

	switch (node->nodetype) {
	default: 
		caserr("doinf",node->nodetype);

	case ASSIGN:
		doinf(RIGHT, 0);
		if ((LEFT->info & TYPEDECL) || (RIGHT->info & TYPEKNOWN)) {
			if (LEFT->info & TYPEDECL) {
				node->type.n = LEFT->type.n;
				if (! typemake(node->type.n, RIGHT->type.n))
					error("assignment type error");
				}
			else
				node->type.n = RIGHT->type.n;
			node->info |= TYPEKNOWN;
			}
		if ((LEFT->info & RANKDECL) || (RIGHT->info & RANKKNOWN)) {
			if (LEFT->info & RANKDECL) {
				node->rank.n = LEFT->rank.n;
				if ((RIGHT->info & RANKKNOWN) &&
					(RIGHT->rank.n != node->rank.n))
					error("assignment rank error");
				}
			else
				node->rank.n = RIGHT->rank.n;
			node->info |= RANKKNOWN;
			}
		if (RIGHT->info & SHAPEKNOWN) {
			/* since ranks agree, this must be ok */
			node->shape.n = RIGHT->shape.n;
			node->info |= SHAPEKNOWN;
			}
# ifdef INTRA
		doassign(LEFT->a.namep, node);
# endif
		break;

	case AVEC:
		node->type.n = CHAR;
		node->rank.n = 1;
		node->shape.n = addicon(256);
		node->info |= (TYPEKNOWN | RANKKNOWN | SHAPEKNOWN);
		break;

	case BCON: 
	case ICON: 
	case RCON: 
	case SCON:
		if (is_scalar(node))
			node->shape.n = 1;
		break;

	case BOX: 
	case QQUAD: 
	case DBOX: 
	case DQQUAD:
		if (RIGHT != NILP) {
			doinf(RIGHT,0);
		}
		node->type.n = ANY;
		break;

	case BOXASSIGN: 
	case DBOXAS: 
	case QBOXAS: 
	case DQBOXAS:
		if (LEFT != NILP) {     /* file name */
			doinf(LEFT,0);
		}
		doinf(RIGHT,0);
		node->type.n = RIGHT->type.n;
		node->rank.n = RIGHT->rank.n;
		node->shape.n = RIGHT->shape.n;
		if (node->nodetype == QBOXAS || node->nodetype == DQBOXAS)
			node->ptr2 = 0;
		else
			node->ptr2 = 1;
		break;

	case CAT:
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		infaxis(node);
		dsrtype(NOT, node, LEFT, RIGHT);
		catshape(node);
		break;

	case COLLECT:
		doinf(RIGHT, 0);
		copychild(node, RIGHT, 1, 1, 1, 1);
		break;

	case COMMENT:
		break;

	case COMPRESS:
	case EXPAND:
		infaxis(node);
		doinf(LEFT, 0);
		doinf(RIGHT, 0);

		if ((LEFT->info & TYPEKNOWN) && ! typemake(LEFT->type.n, BIT))
			error("bit type required for expansion or compression");

		if ((LEFT->info & RANKKNOWN) && (LEFT->rank.n > 1))
			error("vector required for expansion or compression");

		if (RIGHT->info & TYPEKNOWN) {
			node->type.n = RIGHT->type.n;
			node->info |= TYPEKNOWN;
			}

		if (RIGHT->info & RANKKNOWN) {
			node->rank.n = RIGHT->rank.n;
			if (node->rank.n == 0)
				node->rank.n = 1;
			node->info |= RANKKNOWN;
			}

		if ((LEFT->info & VALUESKNOWN) && (RIGHT->info & SHAPEKNOWN)) {
			i = LEFT->values.n;
			c = 0;
			sh = iconsts[LEFT->shape.n];
			rk = RIGHT->rank.n;
			for (j = sh - 1; j >= 0; j--) 
				if (iconsts[i + j]) c++;
			node->shape.n = cpivec(RIGHT->shape.n, rk, -1);
			if (node->nodetype == COMPRESS) {
				if (node->info & FIRSTAXIS) {
					if (sh != iconsts[node->shape.n])
						error("compress error");
					iconsts[node->shape.n] = c;
					node->info |= SHAPEKNOWN;
					}
				else if (node->info & LASTAXIS) {
					if (sh != iconsts[node->shape.n+rk-1])
						error("compress error");
					iconsts[node->shape.n + rk -1] = c;
					node->info |= SHAPEKNOWN;
					}
				}
			else {
				if (node->info & FIRSTAXIS) {
					if (c != iconsts[node->shape.n])
						error("expand conformability error");
					iconsts[node->shape.n] = sh;
					node->info |= SHAPEKNOWN;
					}
				else if (node->info & LASTAXIS) {
					if (c != iconsts[node->shape.n+rk-1])
						error("expand conformability error");
					iconsts[node->shape.n + rk -1] = sh;
					node->info |= SHAPEKNOWN;
					}
				}
			}
		break;

	case CVEC:
		doinf(RIGHT, 0);
		if (RIGHT->info & TYPEKNOWN) {
			node->type.n = RIGHT->type.n;
			node->info |= TYPEKNOWN;
			}
		if (RIGHT->info & RANKKNOWN)
			if (! (ranktest(RIGHT, 0) || ranktest(RIGHT, 1)))
				error("vector required");
		node->rank.n = 1;
		node->info |= RANKKNOWN;
		copychild(node, RIGHT, 0, 0, 1, 1);
		break;

	case CIVEC:
		doinf(RIGHT,0);
		if ((RIGHT->info & TYPEKNOWN) && 
			(! typemake(INT, RIGHT->type.n)))
			error("integer vector required (1)");
		if ((RIGHT->info & RANKKNOWN) &&
		   (! (ranktest(RIGHT, 0) || ranktest(RIGHT, 1))))
			error("integer vector required (2)");
		node->type.n = INT;
		node->rank.n = 1;
		node->info |= (TYPEKNOWN | RANKKNOWN);
		copychild(node, RIGHT, 0, 0, 1, 1);
		break;

	case CSCALAR:
		doinf(RIGHT, 0);
		if ((RIGHT->info & TYPEKNOWN) &&
			(! typemake(INT, RIGHT->type.n)))
			error("integer scalar required (1)");
		if ((RIGHT->info & RANKKNOWN) &&
			(! ranktest(RIGHT, 0)))
			error("integer scalar required (2)");
		node->type.n = INT;
		node->rank.n = 0;
		node->shape.n = 0;
		node->info |= (TYPEKNOWN | RANKKNOWN | SHAPEKNOWN);
		if (RIGHT->info & VALUESKNOWN) {
			node->values.n = RIGHT->values.n;
			node->info |= VALUESKNOWN;
			}
		break;

	case DEAL:
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		node->type.n = INT;
		node->rank.n = 1;
		node->info |= (TYPEKNOWN | RANKKNOWN);
		if (LEFT->info & VALUESKNOWN) {
			node->shape.n = LEFT->values.n;
			node->info |= SHAPEKNOWN;
			}
		break;

	case DECODE:
	case INNERCHILD:
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		dsrtype(node->optype, node, LEFT, RIGHT);
		if ((RIGHT->info & RANKKNOWN) && (LEFT->info & RANKKNOWN)) {
			i = LEFT->rank.n;
			j = RIGHT->rank.n;
			node->rank.n = (i ? i-1 : 0) + (j ? j-1 : 0);
			node->info |= RANKKNOWN;
			}
		if ((RIGHT->info & SHAPEKNOWN) && (LEFT->info & SHAPEKNOWN)) {
			if ((i > 0) && (j > 0)) {
				if (iconsts[LEFT->shape.n + i - 1]
					!= iconsts[RIGHT->shape.n])
				error("inner product shape conformability");
				}
			node->shape.n = 
				cpivec(LEFT->shape.n, (i ? i-1 : 0), -1);
			cpivec(RIGHT->shape.n+1, (j ? j-1 : 0), -1);
			node->info |= SHAPEKNOWN;
			}
		break;

	case DROP:
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		copychild(node, RIGHT, 1, 1, 0, 0);
		if (is_scalar(RIGHT))
			error("domain error - drop");
		if ((RIGHT->info & SHAPEKNOWN) && (LEFT->info & VALUESKNOWN)) {
			if (RIGHT->rank.n != iconsts[LEFT->shape.n])
				error("conformability - drop");
			sh = RIGHT->shape.n;
			rt = LEFT->values.n;
			node->shape.n = ictop;
			for (i = 0; i < node->rank.n; i++) {
				j = iconsts[sh + i] - abs(iconsts[rt + i]);
				j = (j < 0) ? 0 : j;
				addicon(j);
			}
			node->info |= SHAPEKNOWN;
		}
		break;

	case DSOP:
		doinf(RIGHT, 0);
		doinf(LEFT, 0);

		dsrtype(node->optype, node, LEFT, RIGHT);

		if (is_scalar(LEFT))
			copychild(node, RIGHT, 0, 1, 1, 0);
		else if (is_scalar(RIGHT))
			copychild(node, LEFT, 0, 1, 1, 0);
		else if ((RIGHT->info & RANKKNOWN) && (LEFT->info & RANKKNOWN)) {
			i = LEFT->rank.n;
			j = RIGHT->rank.n;
			if (i != j)
				error("dsop rank error");
			node->rank.n = RIGHT->rank.n;
			node->info |= RANKKNOWN;
			if ((LEFT->info & SHAPEKNOWN) 
				&& (RIGHT->info & SHAPEKNOWN)) {
				for (i--; i >- 0; i--)
					if (iconsts[LEFT->shape.n + i] !=
						iconsts[RIGHT->shape.n + i])
					error("dsop shape error");
				node->shape.n = RIGHT->shape.n;
				node->info |= SHAPEKNOWN;
				}
			}

		break;

	case DSYSFUN: 
	case MSYSFUN:
		if (node->nodetype == MSYSFUN)
			doinf(RIGHT,0);
		else
			lrinf(LEFT,RIGHT);
		optype = (enum sysvars) node->optype;
		if (optype == CL || optype == OP || optype == EX) {
			node->rank.n = 0;
			node->type.n = INT;
		}
		else
			node->type.n = ANY;
		break;

	case DTRANS:
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		copychild(node, RIGHT, 1, 0, 0, 0);
		if (is_scalar(RIGHT))
			error("conformability - dyadic transpose");
		if (LEFT->info & VALUESKNOWN)
			dytrans(node);
		break;

	case EMPTSEMI:
		node->type.n = INT;
		node->rank.n = 1;
		node->info |= (TYPEKNOWN | RANKKNOWN);
		if ((node->a.axis)->info & SHAPEKNOWN) {
			node->shape.n = (node->a.axis)->shape.n + node->ptr1;
			node->info |= SHAPEKNOWN;
			}
		break;

	case ENCODE:
		lrinf(LEFT,RIGHT);
		node->type.n = INT;
		if (LEFT->rank.n != 0 && RIGHT->rank.n != 0)
			node->rank.n = LEFT->rank.n + RIGHT->rank.n;
		if (LEFT->shape.n != 0 && RIGHT->shape.n != 0) {
			node->shape.n = cpivec(LEFT->shape.n, LEFT->rank.n, -1);
			cpivec(RIGHT->shape.n, RIGHT->rank.n, -1);
		}
		break;

	case EPSILON:
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		node->type.n = BIT;
		node->info |= TYPEKNOWN;
		copychild(node, LEFT, 0, 1, 1, 0);
		break;

	case FIDENT:
		if (LEFT != NILP)
			doinf(LEFT,0);
		if (RIGHT != NILP)
			doinf(RIGHT,0);
		break;

	case FORMAT:
		doinf(RIGHT, 0);
		node->type.n = CHAR;
		if (RIGHT->rank.n != NORANK)
			node->rank.n = (RIGHT->rank.n) ? RIGHT->rank.n : 1;
		if (RIGHT->type.n == CHAR)
			node->shape.n = RIGHT->shape.n;
		break;

	case GO:
		if (!top)
			cant("non-top level branch");
		doinf(RIGHT,0);
		if (RIGHT->rank.n > 1)
			error("scalar required");
		if (RIGHT->nodetype == LCON) {
			node->values.n = RIGHT->values.n;
		}
		break;

	case IDENT:
# ifdef INTRA
		s = getsinfo(node->a.namep);
		if (s) {
			if (node->info & TYPEKNOWN) {
				if (s->type != UKTYPE) {
					node->type.n = 
						mrgtype(node->type.n, s->type);
/*fprintf(stderr,"improving the type for %s to %d\n", node->a.namep,
node->type.n);*/
					}
				}
			else
				if (s->type != UKTYPE) {
					node->type.n = s->type;
					node->info |= TYPEKNOWN;
/*fprintf(stderr,"inferring the type for %s to %d\n", node->a.namep,
node->type.n);*/
					}
			if (node->info & RANKKNOWN) {
				if (s->vrank != NORANK)
					if (s->vrank > node->rank.n) {
						node->rank.n = s->vrank;
/*fprintf(stderr,"improving the rank for %s to %d\n", node->a.namep,
node->rank.n);*/
						}
				}
			else
				if (s->vrank != NORANK) {
					node->rank.n = s->vrank;
					node->info |= RANKKNOWN;
/*fprintf(stderr,"inferring the rank for %s to %d\n", node->a.namep,
node->rank.n);*/
					}
		}
# endif
		break;

	case INDEX:
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		node->type.n = INT;
		node->info |= TYPEKNOWN;
		copychild(node, RIGHT, 0, 1, 1, 0);
		break;

	case INNER:
		doinf(RIGHT, 0);
		dsrtype(node->optype, node, RIGHT, RIGHT);
		copychild(node, RIGHT, 0, 1, 1, 0);
		break;

	case IOTA:
		doinf(RIGHT,0);
		if ((RIGHT->info & TYPEKNOWN) && 
			(! typemake(INT, RIGHT->type.n)))
			error("iota on non-integer");
		if ((RIGHT->info & RANKKNOWN) && (RIGHT->rank.n > 0))
			error("iota on non-scalar");
		if (RIGHT->info & VALUESKNOWN) {
			if (iconsts[RIGHT->values.n] < 0)
				error ("iota on negative value");
			node->shape.n = RIGHT->values.n;
			node->info |= SHAPEKNOWN;
		}
		node->type.n = INT;
		node->rank.n = 1;
		node->info |= (TYPEKNOWN | RANKKNOWN);
		break;

	case LCON:
		break;

	case LAM:
		error("laminate not implemented yet");

	case MSOP:
		doinf(RIGHT, 0);
		rt = UKTYPE;
		switch(node->optype) {
			case NOT:
				rt = BIT; break;
			case PLUS: case MINUS: case ABS: case FACT:
				if (RIGHT->info & TYPEKNOWN)
					rt = RIGHT->type.n;
				break;
			case TIMES: case FLOOR: case CEIL:
				rt = INT; break;
			case DIVIDE: case EXP: case LOG: case CIRCLE:
				rt = REAL; break;
		}
		if (rt != UKTYPE) {
			node->type.n = rt;
			node->info |= TYPEKNOWN;
			}
		copychild(node, RIGHT, 0, 1, 1, 0);
		break;

	case OUTER:
		doinf(RIGHT, 0);
		doinf(LEFT, 0);

		dsrtype(node->optype, node, LEFT, RIGHT);

		outershape(node);
		break;

	case RAVEL:
		doinf(RIGHT,0);
		if (RIGHT->info & TYPEKNOWN) {
			node->type.n = RIGHT->type.n;
			node->info |= TYPEKNOWN;
			}
		node->rank.n = 1;
		node->info |= RANKKNOWN;
		if (RIGHT->info & SHAPEKNOWN) {
			node->shape.n = addicon(numin(RIGHT));
			node->info |= SHAPEKNOWN;
			}
		if (RIGHT->info & VALUESKNOWN)
			node->info |= VALUESKNOWN;
		break;

	case REDUCE:
		infaxis(node);

		doinf(RIGHT, 0);

		dsrtype(node->optype, node, RIGHT, RIGHT);

		if (RIGHT->info & RANKKNOWN) {
			if (RIGHT->rank.n <= 0)
				error("non-scalar required");
			node->rank.n = RIGHT->rank.n - 1;
			node->info |= RANKKNOWN;
			if (node->rank.n == 0) {
				node->shape.n = 1;
				node->info |= SHAPEKNOWN;
				}
			}

		if (RIGHT->info & SHAPEKNOWN) {
			if (RIGHT->rank.n == 1)
				; /* already did it above */
			else if (node->info & FIRSTAXIS) {
				node->shape.n = RIGHT->shape.n + 1;
				node->info |= SHAPEKNOWN;
				}
			else if (node->info & LASTAXIS) {
				node->shape.n = RIGHT->shape.n;
				node->info |= SHAPEKNOWN;
				}
			}
		break;

	case REVERSE:
		infaxis(node);
		doinf(RIGHT, 0);
		copychild(node, RIGHT, 1, 1, 1, 0);
		break;

	case RESHAPE:
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		if (RIGHT->info & TYPEKNOWN) {
			node->type.n = RIGHT->type.n;
			node->info |= TYPEKNOWN;
			}
		if (LEFT->info & SHAPEKNOWN) {
			node->rank.n = iconsts[LEFT->shape.n];
			node->info |= RANKKNOWN;
			}
		if (LEFT->info & VALUESKNOWN) {
			node->shape.n = LEFT->values.n;
			node->info |= SHAPEKNOWN;
			if (RIGHT->info & SHAPEKNOWN)
				if ((numin(RIGHT) >= numin(node)) &&
					(RIGHT->info & VALUESKNOWN)) {
					node->values.n = RIGHT->values.n;
					node->info |= VALUESKNOWN;
					}
			}
		break;

	case RHO:
		node->type.n = INT;
		node->rank.n = 1;
		node->info |= (TYPEKNOWN | RANKKNOWN);
		doinf(RIGHT,0);
		if (RIGHT->info & RANKKNOWN) {
			node->shape.n = addicon(RIGHT->rank.n);
			node->info |= SHAPEKNOWN;
			}
		if (RIGHT->info & SHAPEKNOWN) {
			node->values.n = RIGHT->shape.n;
			node->info |= VALUESKNOWN;
			}
		break;

	case RHORHO:
		node->type.n = INT;
		node->rank.n = 0;
		node->shape.n = 1;
		node->info |= (TYPEKNOWN | RANKKNOWN | SHAPEKNOWN);
		doinf(RIGHT,0);
		if (RIGHT->info & RANKKNOWN) {
			node->values.n = addicon(RIGHT->rank.n);
			node->info |= VALUESKNOWN;
			}
		break;

	case ROLL:
		doinf(RIGHT, 0);
		if (! typemake(INT, RIGHT->type.n))
			error("integer value required");
		node->type.n = INT;
		node->info |= TYPEKNOWN;
		copychild(node, RIGHT, 0, 1, 1, 0);
		break;

	case ROTATE:
		infaxis(node);
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		copychild(node, RIGHT, 1, 1, 1, 0);
		break;

	case SCAN:
		infaxis(node);
		doinf(RIGHT, 0);
		dsrtype(node->optype, node, RIGHT, RIGHT);
		copychild(node, RIGHT, 0, 1, 1, 0);
		break;

	case SM:
		if (LEFT != NILP) {
			LEFT->a.axis = node->a.axis;
			doinf(LEFT, 0);
			}
		if (RIGHT->nodetype == EMPTSEMI) {
			RIGHT->a.axis = node->a.axis;
			RIGHT->ptr1 = node->ptr1;
			}
		doinf(RIGHT, 0);
		if (LEFT == NILP)
			copychild(node, RIGHT, 0, 1, 1, 0);
		else
			outershape(node);
		break;

	case SORT:
		doinf(RIGHT, 0);
		node->type.n = INT;
		node->rank.n = 1;
		node->info |= (TYPEKNOWN | RANKKNOWN);
		copychild(node, RIGHT, 0, 0, 1, 0);
		break;

	case SUB:
		doinf(LEFT, 0);
		RIGHT->a.axis = LEFT;
		doinf(RIGHT, 0);
		copychild(node, LEFT, 1, 0, 0, 0);
		copychild(node, RIGHT, 0, 1, 1, 0);
		break;

	case SUBASSIGN:
		if (! top)
			cant("non-top subscipted assignment");
		doinf(RIGHT,0);
		doinf(AXIS,1);
		if (LEFT->rank.n != NORANK && LEFT->rank.n != AXIS->ptr3 + 1)
			error("subscript conformability error");
		break;

	case SYSVAR:
		if (RIGHT != NILP)
			doinf(RIGHT,0);
		optype = (enum sysvars) node->optype;
		if (optype == PP || optype == PW || optype == RL ||
		    optype == IO) {
			node->type.n = INT;
			node->rank.n = 0;
			node->shape.n = 1;
		}
		else if (optype == TS) {
			node->type.n = INT;
			node->rank.n = 1;
			node->shape.n = addicon(7);
		}
		else
			node->type.n = ANY;
		break;

	case TAKE:
		doinf(LEFT, 0);
		doinf(RIGHT, 0);
		copychild(node, RIGHT, 1, 1, 0, 0);
		/* will worry about scalar extension later */
		if (LEFT->info & VALUESKNOWN) {
			node->rank.n = iconsts[LEFT->shape.n];
			node->shape.n = ictop;
			rt = LEFT->values.n;
			for (i = 0; i < node->rank.n; i++)
				if (iconsts[rt + i] < 0)
					addicon(-iconsts[rt + i]);
				else
					addicon(iconsts[rt + i]);
			node->info |= SHAPEKNOWN;
		}
		break;

	case TRANS:
		doinf(RIGHT, 0);
		copychild(node, RIGHT, 1, 1, 0, 0);
		if (RIGHT->info & SHAPEKNOWN) {
			node->shape.n = ictop;
			sh = RIGHT->shape.n;
			rk = RIGHT->rank.n;
			for (i = 1; i <= node->rank.n; i++)
				addicon(iconsts[sh + rk - i]);
			node->info |= SHAPEKNOWN;
			}
		break;
	}

	/* just in case we haven't caught it already, catch scalars
	here */
	if (is_scalar(node)) {
		node->shape.n = 1;
		node->info |= SHAPEKNOWN;
		}
}


/* cpivec - copy an integer vector, skipping a position */
int cpivec(start, size, pos)
int start, size, pos;
{  
	int i, k;

	k = ictop;
	for (i = 0; i < size; i++)
		if (i + 1 != pos)
			addicon(iconsts[start+i]);
	return(k);
}

/* catshape - determine shape and rank information for catenate */
catshape(node)
struct node *node;
{	int i, j, ls, rs;

if ((LEFT->info & RANKKNOWN) && (RIGHT->info & RANKKNOWN)) {
	i = LEFT->rank.n;
	j = RIGHT->rank.n;
	if (i == 0)
		if (j == 0) {
			node->rank.n = 1;
			node->shape.n = addicon(2);
			node->info |= SHAPEKNOWN;
			}
		else
			node->rank.n = RIGHT->rank.n;
	else if ((j == 0) || (i == j) || (i == j + 1))
		node->rank.n = LEFT->rank.n;
	else if (i + 1 == j)
		node->rank.n = RIGHT->rank.n;
	else
		error("catenate rank error");
	node->info |= RANKKNOWN;
	}
if ((LEFT->info & SHAPEKNOWN) && (RIGHT->info & SHAPEKNOWN)) {
	ls = LEFT->shape.n;
	rs = RIGHT->shape.n;
	if (node->info & LASTAXIS) {
		if (i == 0)	/* i and j are still valid */
			if (j == 0)
				; /* did it already above */
			else {
				node->shape.n = cpivec(rs, j, -1);
				iconsts[node->shape.n+j-1]++;
				node->info |= SHAPEKNOWN;
				}
		else if (j == 0) {
			node->shape.n = cpivec(ls, i, -1);
			iconsts[node->shape.n+i-1]++;
			node->info |= SHAPEKNOWN;
			}
		else if (i == j) {
			node->shape.n = cpivec(ls, i, -1);
			iconsts[node->shape.n+i-1] += iconsts[rs+i-1];
			node->info |= SHAPEKNOWN;
			/* warning, last axis assumed */
			for (i -= 2; i >= 0; i--)
				if (iconsts[ls + i] != iconsts[rs + i])
					error("catenate conformability (1)");
			}
		else if (i+1 == j) {
			node->shape.n = cpivec(ls, i, -1);
			iconsts[node->shape.n+i-1]++;
			node->info |= SHAPEKNOWN;
			for (j -= 2; j >= 0; j--)
				if (iconsts[ls + j] != iconsts[rs + j])
					error("catenate conformability (2)");
			}
		else if (i == j+1) {
			node->shape.n = cpivec(rs, j, -1);
			iconsts[node->shape.n+j-1]++;
			node->info |= SHAPEKNOWN;
			for (i -= 2; i >= 0; i--)
				if (iconsts[ls + i] != iconsts[rs + i])
					error("catenate conformability (3)");
			}
		else
			error("catenate conformability (4)");
		}
	}

}

/* dytrans - compute rank and shape for dyadic transpose */
dytrans(node)
struct node *node;
{
	int size, c, s, k, i, j, *vec, *shape;

	size = iconsts[LEFT->shape.n];
	vec = &iconsts[LEFT->values.n];
	c = 0;
	s = 1;
	for (i = 1; i <= size && s == 1; i++) {
		s = 0;
		for (j = 0; j < size; j++)
			if (vec[j] == i) {
				node->rank.n = i;
				c++;
				s = 1;
			}
	}
	node->info |= RANKKNOWN;
	if (c != size)
		error("dyadic transpose value error");
	if (! (RIGHT->info & SHAPEKNOWN))
		return;
	shape = &iconsts[RIGHT->shape.n];
	node->shape.n = ictop;
	for (i = 0; i < node->rank.n; i++) {
		k = 15000;
		for (j = 0; j < size; j++)
			if (vec[j] == i+1 && shape[j] < k)
				k = shape[j];
		addicon(k);
	}
	node->info |= SHAPEKNOWN;
}

/* outershape - generate shape information for outer product and for
semicolons */
outershape(node)
struct node *node;
{
	if ((LEFT->info & RANKKNOWN) && (RIGHT->info & RANKKNOWN)) {
		node->rank.n = LEFT->rank.n + RIGHT->rank.n;
		node->info |= RANKKNOWN;
		}

	if ((LEFT->info & SHAPEKNOWN) && (RIGHT->info & SHAPEKNOWN)) {
		node->shape.n = cpivec(LEFT->shape.n, LEFT->rank.n, -1);
		cpivec(RIGHT->shape.n, RIGHT->rank.n, -1);
		node->info |= SHAPEKNOWN;
		}
}
