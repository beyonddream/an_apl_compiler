ws              [ \t]*
%%
[ \t]*          {;}
\n              {linenum = linenum + 1; return(NL);}
\\\n            {linenum = linenum + 1;}
\".*            {return(COMMENT);}
"global"        {yylval.l = GLOBAL;    return(CLASS);}
"fun"           {yylval.l = FUNCTION;  return(CLASS);}
"scalar"        {yylval.i = 0;         return(RANK);}
"vector"        {yylval.i = 1;         return(RANK);}
"var"           {yylval.i = UKTYPE;    return(TYPE);}
"char"          {yylval.i = CHAR;      return(TYPE);}
"bit"           {yylval.i = BIT;       return(TYPE);}
"int"           {yylval.i = INT;       return(TYPE);}
"real"          {yylval.i = REAL;      return(TYPE);}
[a-zA-Z][a-zA-Z0-9]*     {return(lexpid());}
"("             {return(LP);}
")"             {return(RP);}
"["             {return(LB);}
"]"             {return(RB);}
";"             {return(SM);}
","             {return(CM);}
":"             {return(COLON);}
"."             {return(DOT);}
"'"[^\n']*"'"   {if (lexnext('\'')) yymore(); else return(lexstr());}
(".ng"{ws})?[0-9]+\.[0-9]*([eE][+-]?[0-9]+)?    {return( lexnum(RCON));}
(".ng"{ws})?[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?    {return( lexnum(RCON));}
(".ng"{ws})?[0-9]+                              {return( lexnum(ICON)); }
"<"             {yylval.o = LT;    return(DSOP);}
"<="            {yylval.o = LE;    return(DSOP);}
"="             {yylval.o = EQ;    return(DSOP);}
"!="            {yylval.o = NE;    return(DSOP);}
"^="            {yylval.o = NE;    return(DSOP);}
">="            {yylval.o = GE;    return(DSOP);}
">"             {yylval.o = GT;    return(DSOP);}
"|"             {yylval.o = ABS;   return(MSOP);}
"&"             {yylval.o = AND;   return(DSOP);}
"<-"            {return(ASSIGN);}
"_"             {return(ASSIGN);}
".bx"           {return(BOX);}
".ce"           {yylval.o = CEIL;   return(MSOP);}
".lo"           {yylval.o = CIRCLE; return(MSOP);}
".dc"           {return(DECODE);}
".dl"           {return(DEL);}
"%"             {yylval.o = DIVIDE; return(MSOP);}
".da"           {return(DROP);}
".ec"           {return(ENCODE);}
".ep"           {return(EPSILON);}
"*"             {yylval.o = EXP;    return(MSOP);}
"!"             {yylval.o = FACT;   return(MSOP);}
".fl"           {yylval.o = FLOOR;  return(MSOP);}
".fm"           {return(FORMAT);}
".go"           {return(GO);}
"->"            {return(GO);}
".gu"           {yylval.i = 1;      return(SORT);}
".gd"           {yylval.i = -1;     return(SORT);}
".io"           {return(IOTA);}
".lg"           {yylval.o = LOG;    return(MSOP);}
".md"           {yylval.v = MD;     return(ESYSFUN);}
"-"             {yylval.o = MINUS;  return(MSOP);}
".nd"           {yylval.o = NAND;   return(DSOP);}
".no"           {yylval.o = NOR;    return(DSOP);}
"~"             {yylval.o = NOT;    return(MSOP);}
".or"           {yylval.o = OR;     return(DSOP);}
".so"           {return(OUTER);}
"+"             {yylval.o = PLUS;   return(MSOP);}
".qq"           {return(QQUAD);}
".rv"           {yylval.i = LASTAXIS;   return(REVERSE);}
".cr"           {yylval.i = FIRSTAXIS;  return(REVERSE);}
".ro"           {return(RHO);}
"?"             {return(ROLL);}
"/"             {yylval.i = LASTAXIS;   return(SLASH);}
".bs"           {yylval.i = FIRSTAXIS;  return(SLASH);}
"\\"            {yylval.i = LASTAXIS;   return(BSLASH);}
".fs"		{yylval.i = FIRSTAXIS;  return(BSLASH);}
".ua"           {return(TAKE);}
"#"             {yylval.o = TIMES;  return(MSOP);}
".tr"           {return(TRANS);}
".al"           {return(ALPHA);}
".om"           {return(OMEGA);}
".bxpp"         {yylval.v = PP;     return(ASYSVAR);}
".bxpw"         {yylval.v = PW;     return(ASYSVAR);}
".bxrl"         {yylval.v = RL;     return(ASYSVAR);}
".bxio"         {yylval.v = IO;     return(ASYSVAR);}
".bxav"         {return(AVEC);}
".bxts"         {yylval.v = TS;     return(SYSVAR);}
".bxarg"        {yylval.v = AG;     return(MSYSFUN);}
".bxex"         {yylval.v = EX;     return(MSYSFUN);}
".bxcl"         {yylval.v = CL;     return(MSYSFUN);}
".bxop"         {yylval.v = OP;     return(ESYSFUN);}
".bxfm"         {yylval.v = FM;     return(DSYSFUN);}
".bxpf"         {yylval.v = PF;     return(DSYSFUN);}
.		{error("unrecognized character");}
%%
int lexnext(c)
   char c;
{
   char d;
   unput(d = input());
   return(c == d);
}

int lexpid(){
   struct symnode *p;
   enum classes ic, idclass();

   ic = idclass(yytext, &p);
   yylval.s = p;
   switch(ic) {
      case GLOBAL: if (p->type == LABEL)
		      yyerror("cannot have global label");
		   return(IDENT);
      case PARAM:  return(IDENT);
      case LOCAL:  if (p->type == LABEL)
		      return(LCON);
		   else return(IDENT);
      case FUNCTION: return(FIDENT);
      case NOCLASS:  return(UIDENT);
      }
   return(UIDENT);
}

int lexnum(type)
   int type;
{
   char *p;
   double atof();

   if (yytext[1] == 'n') {
      yytext[0] = yytext[1] = yytext[2] = ' ';
      for (p = yytext; *(p+1) == ' '; p++)
         ;
      *p = '-';
      }
   if (type == RCON) {
      yylval.d = atof(yytext);
      return(RCON);
      }
   else {
      yylval.i = atoi(yytext);
      if (yylval.i == 0 | yylval.i == 1)
         return(BCON);
      return(ICON);
      }
}

int lexstr()
{
   char *p, *q;

   for (p = (q = yytext) + 1; *(p+1) != 0; q++ , p++) {
      *q = *p;
      if (*p == '\'')
         p++;
      }
   *q = 0;
   return(SCON);
}
