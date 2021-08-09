/* structures and definitions used by the APL compiler */

# define PARSER
# include "aplc.h"
# include "ctree.h"

/* structalloc - malloc a structure */
# define structalloc(type) (struct type *) malloc(sizeof(struct type ))

# define MAXCONSTS 500

/* parse tree i/o constants */
# define GLSYM  01
# define MNPROG 02
# define PROG   03
# define CONSTS 04
# define STMT   05
# define ESTMT  06
# define SYMS   07
# define ESYMS 010

/* a rank of -1 means rank is not known */
# define NORANK -1

/* information bits */
# define TYPEDECL	01	/* type was declared by user */
# define TYPEKNOWN	02	/* type is known (may have been inferred) */
# define RANKDECL	04	/* rank was declared by user */
# define RANKKNOWN     010	/* rank is known */
# define SHAPEKNOWN    020
# define VALUESKNOWN   040
# define FIRSTAXIS    0100
# define LASTAXIS     0200
# define SEQUENTIAL  01000	/* will access in sequential order */
# define HAVEVALUE   02000	/* will have value at end of shape phase */
# define NOINDEX     04000	/* no index register is needed for expr */
# define MERGED     010000	/* node has been merge with one higher up */

# define LEFT  (node->left)
# define RIGHT (node->right)
# define AXIS  (node->a.axis)
# define NILP (struct node *) 0

/* int or tree - either an int (if known at compile time) or 
	a code tree (if generated at run time) node */

union intortree {
	int n;
	struct codetree *c;
	} ;

/* node - a parse tree node */
struct node {
	int nodetype;                /* node type */
	int info;			/* information associated with node */
	struct node *right, *left;   /* right and left children nodes */
	union intortree type;	/* type of result of node */
	union intortree rank;	/* rank of result */
	union intortree shape;	/* shape of result */
	union intortree values;	/* values in result */
	union {
		struct node *axis;         /* axis node */
		struct symnode *symp;      /* symbol table pointer */
		char   *namep;             /* name pointer, for vars and funs */
		int    ptr0;		   /* an extra useful pointer */
		} a;
	enum sops optype;            /* operator (for reduce, outer, etc) */
	int index;                   /* index pointer number */
	short ptr1;		     /* pointers with various meanings */
	short ptr2;
	short ptr3;
	short ptr4;
	short ptr5;
	short ptr6;
	short ptr7;
	short ptr8;
	short ptr9;
	short ptr10;
	short ptr11;
	} ;

# define axisgiven(n) (n != NILP)

struct symnode {    /* symbol table node */
	char *name;
	int type;
	int  vrank;
	struct symnode *next;
	} ;

# define NILSYM (struct symnode *) 0

struct headnode {   /* function header information */
	char *fname; 		/* function name */
	char *asvar;		/* name of assigned to variable */
	char *parm1;		/* name of first parameter */
	char *parm2;		/* name of second parameter */
	int  maxtrs;		/* number of trs registers used */
	int  maxmp;		/* number of mp registers used */
	int  maxres;		/* number of res registers used */
	int  maxi;		/* number of index registers used */
	} ;

# define NILHEAD (struct headnode *) 0

struct statenode {      /* statement node */
	char *label;
	struct node *code;
	struct statenode *nextstate;
	struct symnode *list
	} ;

# define NILSTATE (struct statenode *) 0

union label_struct {
	char *label_name;
	int  label_num;
	};

extern char *malloc();

# define NILCHAR (char *) 0

extern int stmtno;

# define DEFAULTINDEX 999
