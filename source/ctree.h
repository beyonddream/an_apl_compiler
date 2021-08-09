/*
	APL compiler

	structures and definitions for code trees.
	tim budd
*/

/* parse tree processing modes */
enum pmodes {SHAPE, COMBINE, VALUE, FINISH};

/* types of operations for the code trees */

enum codeops {
	asgn,		/* assignment */
	castop,		/* cast operator */
	cnst,		/* constants - index into constant tables */
	condop,		/* conditional operation */
	dsop,		/* dyadic scalar ops */
	deref, 		/* pointer dereference */
	icnst,		/* actual integer constant (not index) */
	idptr,		/* pointer to an identifier, with fields */
	iptr,		/* index register value */
	ixorgin,	/* index origin */
	memptr,		/* memory pointer */
	msop,		/* monadic scalar ops */
	postinc,	/* postincrement (for pointers) */
	resptr,		/* result register pointer */
	tcnst,		/* type constant */
	trsptr,		/* type/rank/shape register pointer */
	trsvptr,	/* trs.value pointer */
	};

enum tfields { ctypefield, crankfield, cshapefield, cvalfield };

struct codetree {
	enum codeops cop;
	union {
		struct codetree *cleft;
		int cindex;
		char *cident;
		} c0;
	union {
		struct codetree *cright;
		enum tfields ctfield;
		} c1;
	union {
		int ctype;
		enum sops csop;
		struct codetree *cmiddle;
		} c2;
	};


extern int *iconsts;

extern int is_scalar();

extern struct codetree *gicn();
extern struct codetree *gmon();
extern struct codetree *gcast();
extern struct codetree *gbin();
extern struct codetree *gsop();
extern struct codetree *gtrs();
extern struct codetree *gident();
extern struct codetree *gcond();
extern struct codetree *gmop();
extern struct codetree *gixorg();
