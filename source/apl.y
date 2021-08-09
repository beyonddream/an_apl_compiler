/*
	apl compiler
		parser
			timothy a. budd
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

/*  tokens  */

/*  identifier attributes */
%token CLASS TYPE RANK

/*  special symbols  */
%token DEL GO COMMENT

/*  internally generated operators (collections and the like) */
%token COLLECT CVEC CIVEC CSCALAR

/*  assignments, input/output */
%token ASSIGN BOXASSIGN SUBASSIGN
%token BOX DBOX QQUAD DQQUAD DBOXAS QBOXAS DQBOXAS

/*  identifiers (functions, undefined vars) */
%token IDENT FIDENT UIDENT

/*  constants */
%token LCON SCON BCON ICON RCON

/*  little symbols   */
%token NL LP RP LB RB CM SM COLON DOT

/*  scalar ops       */
%token MSOP DSOP 

/* inner and outer products */
%token OUTER INNER INNERCHILD DECODE

/*  slash ops (expand, compress) */
%token SLASH BSLASH REDUCE EXPAND COMPRESS SCAN

/* sorts and operations using sorts */
%token SORT GRADEUP GRADEDOWN EPSILON INDEX

/* trasposes, reversals - other mergeable functions */
%token TRANS DTRANS REVERSE ROTATE TAKE DROP

/* shape and reshape */
%token RHO RHORHO RESHAPE

/* subscripts */
%token SUB EMPTSEMI

/* misc ops */
%token IOTA RAVEL CAT LAM
%token ROLL DEAL 
%token ENCODE FORMAT

/* system variables, functions */
%token AVEC
%token ASYSVAR SYSVAR DSYSFUN ESYSFUN MSYSFUN

/* alpha & omega for direct definition */
%token ALPHA OMEGA

/* idioms */
%token CGOTO

%start file

%{

# include "parse.h"

# define NIL 0

# define setattributes {dclclass = lexscope; dcltype = UKTYPE; dclrank = -1;}

int linenum;                    /* line number of line being parsed */
static enum classes lexscope;   /* current lexical scope */
static struct statenode *gstmts; /* global statements */
static int errflag;             /* error indicatation flag */

static enum classes dclclass;   /* attributes given in declaration header */
static int          dcltype;
static int          dclrank;

extern struct statenode *addstmt(), *newstate();
extern struct symnode *enterid();
extern char *newid();
extern struct headnode *newhead();
extern struct node *pttop(), *ptsub(), *ptfun();
extern struct node *ptmrho(), *ptvar(), *ptsort(), *ptsemi();
extern struct node *pt1(), *pt1o(), *pt1a(), *pt1ao();
extern struct node *pt2(), *pt2o(), *pt2a(), *pt2ao();
extern struct node *sysparm(), *newnode(), *ptsvec(), *ptvec();
extern struct node *ptval(), *aptval();
%}

%union {
	char *c;
	double d;
	struct headnode *h;
	int  i;
	enum classes l;
	struct node *n;
	enum sops o;
	struct symnode *s;
	struct statenode *t;
	enum sysvars v;
	}

%type <h> heading params parampart
%type <t> statelist lstatement
%type <c> ident uident fident lterm
%type <s> IDENT FIDENT UIDENT LCON
%type <n> progid subid
%type <n> ulstatement expression axis aterm sub term sterm 
%type <n> simplestatement
%type <n> bvec ivec rvec lvec sublist subele 
%type <i> REVERSE SLASH BSLASH
%type <i> RANK SORT ICON BCON
%type <v> ASYSVAR SYSVAR MSYSFUN DSYSFUN ESYSFUN
%type <i> TYPE
%type <l> CLASS
%type <o> DSOP MSOP sop
%type <d> RCON

%%

file:              globals
                        {if (gstmts != NILSTATE && ! errflag) 
		 		mnbody(gstmts);}
                ;

globals:           initialize
                |  globals object
			{lexscope = GLOBAL; setattributes;}
                ;

initialize:        /* empty - just perform initialization */

                        {lexscope = GLOBAL;
                         setattributes;
                         linenum = 1;
                         gstmts = NILSTATE;
			 resetconsts();
                         addicon(0);
                         addicon(1);
                         errflag = 0;
                        }
                ;

object:            simplestatement
			{gstmts = addstmt(gstmts, newstate(NILCHAR, $1)); }
                |  dclstmt
                |  function
		|  fident COLON expression NL
				{direct($1, $3);}
		|  uident COLON expression NL
				{direct($1, $3);}
                ;

function:          heading dcls statelist DEL NL
                                        {if (! errflag)
                                            prog($1, $3);
					 resetconsts();
					 reinitsymtab();}
                ;

heading:           DEL params NL        {lexscope = LOCAL; $$ = $2;}
                ;

params:            ident ASSIGN parampart
                                        {$3->asvar = $1;
                                         enterid($1, LOCAL, UKTYPE, NORANK);
                                         $$ = $3;}
                |  parampart            {$$ = $1;}
                ;

ident:             IDENT                {$$ = $1->name;}
                |  uident
                ;

uident:            UIDENT               {$$ = newid(yytext);}
                ;

fident:            FIDENT               {$$ = $1->name;}
                ;

parampart:         ident  ident ident   {$$ = newhead($2, $1, $3);}
                |  ident fident ident   {$$ = newhead($2, $1, $3);}
                |  ident  ident {$$ = newhead($1, NILCHAR, $2);}
                |  fident ident {$$ = newhead($1, NILCHAR, $2);}
                ;

dcls:              dcllist              {setlocalcons();
                                         addicon(0);
                                         addicon(1);}
                ;

dcllist:           /* empty */        {setattributes;}
                |  dcllist dclstmt    {setattributes;}
                ;

dclstmt:           attributes namelist NL
                ;

attributes:	  anattribute
		| attributes anattribute
		;

anattribute:	  CLASS			{dclclass = $1;}
		| TYPE			{dcltype  = $1;}
		| RANK			{dclrank  = $1;}
		;

namelist:          ident                {enterid($1, dclclass, dcltype, dclrank);}
                |  fident               {enterid($1, FUNCTION, dcltype, dclrank);}
                |  namelist CM ident    {enterid($3, dclclass, dcltype, dclrank);}
                |  namelist CM fident   {enterid($3, FUNCTION, dcltype, dclrank);}
                ;

statelist:         lstatement           {$$ = $1;}
                |  statelist lstatement {$$ = addstmt($1, $2);}
                ;

lstatement:        error '\n'              {yyerrok;}
                |  lterm COLON ulstatement {$$ = newstate($1, $3);}
                |  ulstatement             {$$ = newstate(NILCHAR, $1);}
                ;

ulstatement:	   GO expression NL	{$$ = pt1(GO, $2);}
		|  simplestatement	{$$ = $1;}
		;

simplestatement:   expression NL                {$$ = pttop($1);}
                |  sub ASSIGN expression NL     {$$ = ptsub($1, $3);}
                |  COMMENT NL     {$$ = pt1(COMMENT, (struct node *) 0);}
                |  NL             {$$ = pt1(COMMENT, (struct node *) 0);}
                ;

expression: MSOP   expression       {$$ = pt1o(MSOP, $1, $2);}
        |  RHO     expression       {$$ = ptmrho($2);}
        |  CM      expression       {$$ = pt1(RAVEL, $2);}
        |  IOTA    expression       {$$ = pt1(IOTA, pt1(CSCALAR, $2));}
        |  ROLL    expression       {$$ = pt1(COLLECT, pt1(ROLL, $2));}
        |  SORT    expression       {$$ = ptsort($1, pt1(CVEC, $2));}
        |  TRANS   expression       {$$ = pt1(TRANS, $2);}
        |  FORMAT  expression       {$$ = pt1(FORMAT, $2);}
        |  REVERSE axis expression  {$$ = pt1a(REVERSE, $2, $1, $3);}
        |  sop SLASH axis expression
                                    {$$ = pt1ao(REDUCE, $1, $3, $4, $2);}
        |  sop BSLASH axis expression
                                    {$$ = pt1ao(SCAN, $1, $3, $4, $2);}
        |  BOX ASSIGN   expression  {$$ = pt1(BOXASSIGN, $3);}
        |  QQUAD ASSIGN expression  {$$ = pt1(QBOXAS, $3);}
        |  aterm ASSIGN expression  {$$ = pt2(ASSIGN, $1, $3);}
        |  ASYSVAR ASSIGN expression
                          {$$ = pt1o(SYSVAR, $1, pt1(COLLECT, $3));}
        |  term BOX ASSIGN expression
                                    {$$ = pt2(DBOXAS, $1, $4);}
        |  term QQUAD ASSIGN expression
                                    {$$ = pt2(DQBOXAS, $1, $4);}
        |  term sop     expression  {$$ = pt2o(DSOP, $2, $1, $3);}
        |  term ROLL    expression  
			{$$ = pt2(DEAL, pt1(CSCALAR, $1), pt1(CSCALAR, $3));}
        |  term EPSILON expression
      				{$$ = pt2(EPSILON, $1, 
				ptsort(1, pt1(CVEC, pt1(RAVEL, $3))));}
        |  term IOTA    expression
      				{$$ = pt2(INDEX, 
				ptsort(1, pt1(CVEC, $1)), $3);}
        |  term TRANS   expression  
				{$$ = pt2(DTRANS, pt1(CIVEC, $1), $3);}
        |  term REVERSE axis expression  
	     			    {$$ = pt2a(ROTATE, $3, $2, $1, $4);}
        |  term TAKE    expression  {$$ = pt2(TAKE, pt1(CIVEC, $1), $3);}
        |  term DROP    expression  {$$ = pt2(DROP, pt1(CIVEC, $1), $3);}
        |  term RHO     expression  {$$ = pt2(RESHAPE, pt1(CIVEC, $1), $3);}
        |  term DECODE  expression
			{$$ = pt1ao(INNER, PLUS, NILP,
			pt2ao(DECODE, TIMES, NILP, FIRSTAXIS, $1, $3),
				LASTAXIS);}
        |  term ENCODE  expression  {$$ = pt2(ENCODE, $1, $3);}
        |  term CM axis expression  {$$ = pt2a(CAT, $3, LASTAXIS, $1, $4);}
        |  term SLASH axis expression
                                    {$$ = pt2a(COMPRESS, $3, $2, $1, $4);}
        |  term BSLASH axis expression
                                    {$$ = pt2a(EXPAND, $3, $2, $1, $4);}
        |  term sop DOT sop expression
			{$$ = pt1ao(INNER, $2, NILP,
			pt2ao(INNERCHILD, $4, NILP, FIRSTAXIS, $1, $5),
				LASTAXIS);}
        |  term OUTER DOT sop expression
                                    {$$ = pt2o(OUTER, $4, $1, $5);}
        |  term FIDENT expression
           {$$ = ptfun($2, pt1(COLLECT, $1), pt1(COLLECT, $3));}
        |  term DSYSFUN expression
           {$$ = pt2o(SYSVAR, $2, pt1(COLLECT, $1), pt1(COLLECT, $3));}
        |  term ESYSFUN expression
           {$$ = pt2o(SYSVAR, $2, pt1(COLLECT, $1), pt1(COLLECT, $3));}
        |  FIDENT  expression   
	   {$$ = ptfun($1, (struct node *) NIL, pt1(COLLECT, $2));}
        |  MSYSFUN expression   {$$ = pt1o(SYSVAR, $1, pt1(COLLECT, $2));}
        |  ESYSFUN expression   {$$ = pt1o(SYSVAR, $1, pt1(COLLECT, $2));}
        |  term                     {$$ = $1;}
	|  error			{expect("expression");}
        ;

axis:               /* empty */         {$$ = 0;}
                | LB expression RB      {$$ = $2;}
                ;

sop:               MSOP
                |  DSOP
                ;

aterm:             progid               {$$ = $1;}
                |  uident               
			{$$ = ptvar(enterid($1, dclclass, UKTYPE, NORANK));}
                ;

progid:            IDENT                {$$ = ptvar($1);}
                |  ALPHA                {$$ = sysparm("_alpha");}
                |  OMEGA                {$$ = sysparm("_omega");}
                ;

sub:               subid LB sublist RB  {$$ = pt2(SUB, $1, $3);}
                ;

subid:             progid               {$$ = $1;}
                ;

term:              progid               {$$ = $1;}
                |  sterm                {$$ = $1;}
                |  sub                  {$$ = $1;}
                ;

sterm:             sterm LB sublist RB  {$$ = pt2(SUB, $1, $3);}
                |  BOX                  {$$ = newnode(BOX);}
                |  term BOX             {$$ = pt1(DBOX, $1);}
                |  QQUAD                {$$ = newnode(QQUAD);}
                |  term QQUAD           {$$ = pt1(DQQUAD, $1);}
                |  LP expression RP     {$$ = $2;}
                |  ASYSVAR              {$$ = pt1o(SYSVAR, $1, NILP);}
                |  SYSVAR               {$$ = pt1o(SYSVAR, $1, NILP);}
                |  AVEC                 {$$ = newnode(AVEC);}
                |  SCON                 {$$ = ptsvec(yytext);}
                |  bvec                 {$$ = ptvec($1, BCON, BIT);}
                |  ivec                 {$$ = ptvec($1, ICON, INT);}
                |  rvec                 {$$ = ptvec($1, RCON, REAL);}
                |  lvec                 {$$ = ptvec($1, LCON, INT);}
                ;

bvec:              BCON         {$$ = ptval(BCON, addicon($1));}
                |  bvec BCON    {addicon($2); $$ = aptval($1);}
                ;

ivec:              ICON         {$$ = ptval(ICON, addicon($1));}
                |  bvec ICON    {addicon($2); $$ = aptval($1);}
                |  ivec ICON    {addicon($2); $$ = aptval($1);}
                |  ivec BCON    {addicon($2); $$ = aptval($1);}
                ;

rvec:              RCON         {$$ = ptval(RCON, addrcon($1));}
                |  bvec RCON    {expanivec($1, $2);}
                |  ivec RCON    {expanivec($1, $2);}
                |  rvec RCON    {addrcon($2); $$ = aptval($1);}
                |  rvec ICON    {addrcon((double) $2); $$ = aptval($1);}
                |  rvec BCON    {addrcon((double) $2); $$ = aptval($1);}
                ;

lvec:              lterm        {$$ = ptval(LCON, addlcon($1));}
                |  lvec lterm   {addlcon($2); $$ = aptval($1);}
                ;

lterm:             LCON                 {$$ = ((struct symnode *) $$)->name;}
                |  uident		{$$ = $1;}
                ;

sublist:           subele               {$$ = ptsemi(NILP, $1);}
                |  sublist SM subele    {$$ = ptsemi($1, $3);}
                ;

subele:            /* empty */          {$$ = newnode(EMPTSEMI);}
                |  expression           {$$ = $1;}
                ;
%%
# include "lex.yy.c"

main() {
   yyparse();
   if (! errflag)
      exit(0);
   else
      exit(1);
}

/* yyerror - print error messages */
yyerror(c)
char *c;
{ 
	error(c);
}

extern struct symnode *gsymtab, *symtab;

/* mnbody - output code for the main routine */
   mnbody(code)
   struct statenode *code;
{  struct headnode *mnhead;

   putchar(MNPROG);
   mnhead = newhead(NILCHAR, NILCHAR, NILCHAR);
   putheader(mnhead);
   putsyms(gsymtab);
   putconsts();
   putcode(code);
   fresyms(gsymtab);
   frecode(code);
}

extern union label_struct lconsts[];

/* prog - output code for procedure */
   prog(head, code)
   struct headnode *head;
   struct statenode *code;
{
   putchar(PROG);
   putheader(head);
   putsyms(symtab);
   resolvelabels(code, lconsts);
   putconsts();
   putcode(code);
   fresyms(symtab);
   frecode(code);
}

extern int lctop;

/* resolve labels */
   resolvelabels(code, lar)
   struct statenode *code;
   union  label_struct lar[];
{  int i, j;
   struct statenode *y;

   for (i = 0; i < lctop; i++) {
      j = 0;
      for (y = code; y != NILSTATE; y = y->nextstate) {
         j++;
         if ((y->label != NILCHAR) && 
		(strcmp(lar[i].label_name, y->label)) == 0) {
            		lar[i].label_num = j;
            		break;
            }
         }
      if (y == NILSTATE) {
         fprintf(stderr,"%s ",lar[i].label_name);
         yyerror("unresolved label");
         }
     }
}

expect(str)
char *str;
{  char buffer[100];

   sprintf(buffer, "expected %s found %s", str, yytext);
   yyerror(buffer);
}
