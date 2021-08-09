/*
	APL compiler
		driver for internal passes
		timothy a. budd

	recognizes the following flags
		-n 	don't print out processed parse tree
				(used only on last pass)

		-idigit	set index origin to digit
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

/* global variables */

int    *iconsts,ic[MAXCONSTS];          /* integer constants */
double *rconsts,rc[MAXCONSTS];          /* real constants */
char   *sconsts,sc[MAXCONSTS];          /* string constants */
union  label_struct lconsts[MAXCONSTS];	/* label constants */
int    ictop, rctop, sctop, lctop;      /* top of arrays */
int	indxorgin;			/* index origin */
struct headnode head;			/* header for current function */
extern char   *funname;			/* function name */

/* read parse tree in, process it, print it back out */
main(argc, argv) 
int argc;
char *argv[];
{
	int    c;
	char   name[120], asvar[120], parm1[120], parm2[120];
	struct statenode *code, *rdcode();
	struct symnode *syms, *getsyms();
	int    i, output;

	output = 1;
	indxorgin = DEFAULTINDEX;
	for (i = 1; i <= argc; i++)
		if (argv[i][0] == '-')
			switch(argv[i][1]) {
			case 'n': 
				output = 0;
				break;
			case 'i':
				indxorgin = argv[i][2] - '0';
				break;
			}

	passinit();

	iconsts = ic;       /* this kludge necessary in order to   */
	rconsts = rc;       /* insure conformable declarations for */
	sconsts = sc;       /* constant arrays                     */
	while ((c = getchar()) != EOF) {
		switch (c) {
		case GLSYM:  
			(void) gets(name);
			glbvar(name);
			if (output)
				printf("%c%s\n", GLSYM, name);
			break;

		case MNPROG: 
			gethead(&head, name, asvar, parm1, parm2);
			funname = "main";
			syms = getsyms();
			rdconsts();
			code = rdcode();
			doprog(&head, syms, code);
			if (output) {
				putchar(MNPROG);
				putheader(&head);
				putsyms(syms);
				putconsts();
				putcode(code);
			}
			break;

		case PROG:   
			gethead(&head, name, asvar, parm1, parm2);
			funname = head.fname;
			syms = getsyms();
			rdconsts();
			code = rdcode();
			doprog(&head, syms, code);
			if (output) {
				putchar(PROG);
				putheader(&head);
				putsyms(syms);
				putconsts();
				putcode(code);
			}
			break;

		default:     
			fprintf(stderr,"internal pass:unknown character read %d %c\n",c,c);
			exit(1);
		}
	}
	exit(0);
}
