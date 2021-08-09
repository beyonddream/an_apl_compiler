/* 
	APL Compiler

	structures and definitions used by the apl compiler,
	both by the compiler and by compiled aplc programs
*/

/* value types */
# define UKTYPE 0
# define BIT	1
# define INT	2
# define REAL	3
# define CHAR	4
# define ANY	5
# define LABEL	6

/* identifier class types */
enum classes {NOCLASS, GLOBAL, PARAM, LOCAL, FUNCTION, LABCLASS};

/* scalar ops   first line - monadic only
		second line - monadic and dyadic, 
		third line - dyadic only */
enum sops {NOT,
	   FLOOR,CEIL,PLUS,MINUS,TIMES,ABS,DIVIDE,EXP,LOG,CIRCLE,FACT,
           AND,OR,NAND,NOR,LT,LE,EQ,NE,GE,GT};

/* system variables, constants */
enum sysvars {PP, PW, RL, IO, TS, FM, PF, OP, EX, CL, AG, MD};

 /*  PP - printing precision */
 /*  PW - print width */
 /*  RL - random link */
 /*  IO - index origin */
 /*  TS - time stamp */
 /*  OP - open file */
 /*  EX - do unix system execute */
 /*  CL - close file */
 /*  AG - get command arguments */
 /*  MD - matrix inverse & division */

/*
	the following definitions are used only by the run-time system,
	not by the parser itself
*/

# ifndef PARSER

# include <stdio.h>
# include <math.h>

/* use NOT for finding the type of the catenation function */

# define CAT NOT

 /* mp_struct - pointers to memory */
 union mp_struct {
	int	*ip;	/* integer pointers */
	double	*rp;	/* real pointers */
	char	*cp;	/* character pointers */
	};

/* trs - type/rank/shape pointers */
struct trs_struct {
	int	type;	/* type of object */
	int	rank;
	int	*shape;
	union mp_struct value;
	};

/* res_struct - structures for results */
union res_struct {
	int	i;
	double	r;
	char	c;
	};

extern int stmtno;		/* statement being executed */
extern int _ixorg;		/* index origin */

extern int bmprqv();		/* bump a request vector by one position */
extern int vsize();		/* size of a variable */
extern int talloc();		/* trs allocation */
extern int esubi();		/* expansion vector value */
extern int dsopt();		/* value of dyadic scalar function */
extern int msopt();		/* value of monadic scalar function */
extern int dsops();		/* dyadic scalar function shape */
extern int outshape();		/* outer product shape */
extern int innershape();	/* decode and inner product shape */
extern int catshape();		/* catenate shape */

# define memfree(x) ;

# define PI 3.1415926

# ifdef TRACE
#define trace(name,stmt) fprintf(stderr,"trace %s statement %d\n", name, stmt)
# endif

#ifndef TRACE
# define trace(name,stmt)
# endif

# endif
