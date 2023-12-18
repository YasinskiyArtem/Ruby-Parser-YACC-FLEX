%{
  #include <stdio.h>
  #include "parser.h"
  extern int yychar;
  extern FILE* yyin;
  int yylex();
  void yyerror(const char *s);
  int isatty(int fd) { return 0; }
  int keyword_do_state = 0;
  int ignore_nl = 0;
  int ignore_except_eof = 0;
//  yypstate *ps;
int yycolumn = 1;

static void update_loc () {
    static int curr_line = 1;
    static int curr_col  = 1;

    yylloc.first_line   = curr_line;
    yylloc.first_column = curr_col;

    for (char *s = yytext; *s != '\0'; s++) {
      if (*s == '\n') {
        curr_line++;
        curr_col = 1;
      } else {
        curr_col++;
      }
    }

    yylloc.last_line   = curr_line;
    yylloc.last_column = curr_col-1;
  }

/*static int push_token(int token) {
  yychar = token;
  if ( ignore_except_eof && token) return YYPUSH_MORE;
  return yypush_parse(ps);
}

static int run() {
  int status = YYPUSH_MORE;
  while(status == YYPUSH_MORE) {
    status = push_token(yylex());
  }
}*/

#define YY_USER_ACTION { update_loc(); }

%}
%option nounistd yylineno outfile="scanner.c" header="scanner.h" 
%x Paren2

ESCAPED_CHAR (\\.|\\\n)
STRING_QUOTE1 ('([^\\'\n]|{ESCAPED_CHAR})*')
STRING_QUOTE2 (\"([^\\\"\n]|{ESCAPED_CHAR})*\")
INSIDE_LONG_QUOTE1 ([']{0,2}([^\\']|{ESCAPED_CHAR}))*
INSIDE_LONG_QUOTE2 ([\"]{0,2}([^\\\"]|{ESCAPED_CHAR}))*
LONG_STRING_QUOTE1 ('''{INSIDE_LONG_QUOTE1}''')
LONG_STRING_QUOTE2 (\"\"\"{INSIDE_LONG_QUOTE2}\"\"\")
FORMAT_STRING (\%w)
STRING_QUOTE ({STRING_QUOTE1}|{STRING_QUOTE2}|{LONG_STRING_QUOTE1}|{LONG_STRING_QUOTE2})

INT10 ([1-9](_?[0-9])*)|(0(_?0)*)
INT2 0[bB](_?[01])+
INT8 0[oO](_?[0-7])+
INT16 0[xX](_?[0-9a-fA-F])+
INT ({INT10}|{INT2}|{INT8}|{INT16})

DIGITS ([0-9](_?[0-9])*)
EXPONENT ([eE][+-]?{DIGITS})
FRACTION \.{DIGITS}
EMPTY_FRAC {DIGITS}\.
FLOAT {DIGITS}{FRACTION}{EXPONENT}|{DIGITS}{EXPONENT}|{DIGITS}{FRACTION}|{EMPTY_FRAC}

NUMBER ((-[ \t]*)?{INT}|{FLOAT})
tIDENTIFIER_INSIDE [a-zA-Z0-9_]|::
%%

\n*([ \t]*#.*\n)*[ \t]*#.* {
    // comment: ignore whitespaces
}
\(?\<\<\~EOF { ignore_except_eof = 1; }
EOF { ignore_except_eof = 0; ignore_nl--; }
class { return  keyword_class; }
module { return         keyword_module; }
def { return    keyword_def; }
undef { return  keyword_undef; }
begin { return  keyword_begin; }
BEGIN             {return keyword_lBEGIN;}
END               {return keyword_lEND;}
rescue { return         keyword_rescue; }
ensure { return         keyword_ensure; }
end { return    keyword_end; }
if { return     keyword_if; }
unless { return         keyword_unless; }
then { return   keyword_then; }
elsif { return  keyword_elsif; }
else { return   keyword_else; }
case { return   keyword_case; }
when { return   keyword_when; }
while { keyword_do_state = 1; return  keyword_while; }
until { keyword_do_state = 1; return  keyword_until; }
for { keyword_do_state = 1; return    keyword_for; }
break { return  keyword_break; }
next { return   keyword_next; }
redo { return   keyword_redo; }
retry { return  keyword_retry; }
in { return     keyword_in; }
do { return     keyword_do_state == 0 ? keyword_do : keyword_do_loop; }
return { return         keyword_return; }
yield { return  keyword_yield; }
super { return  keyword_super; }
and { return    keyword_and; }
or { return     keyword_or; }
not { return    keyword_not; }
alias { return  keyword_alias; }



self { return  keyword_self; }
true { return keyword_true; }
false { return keyword_false;}
nil { return keyword_nil;}



"[" {return tLBRACK;}
"]" {return tRBRACK;}
"{" {return tLCURLY;}
"}" {return tRCURLY;}
","  {return tCOMMA; tCOMMA2;}
"**" { return tPOW; return tDSTAR; } 
"<=>" { return tCMP; } 
"==" { return tEQ; } 
"===" { return tEQQ; } 
"!=" { return tNEQ; } 
">=" { return tGEQ; } 
"<=" { return tLEQ; } 
"&&" { return tANDOP; } 
"||" { return tOROP; } 
"=~" { return tMATCH; } 
"!~" { return tNMATCH; } 
".." { return tDOT2; } 
"..." { return tDOT3; return tBDOT3; } 
"<<" { return tLSHFT; } 
">>" { return tRSHFT; } 
"&." { return tANDDOT; } 

"("             { return tLPAREN; }
<Paren2>"("     { BEGIN(INITIAL); return tLPAREN2; }

")" { return tRPAREN; }
"=>" { return tASSOC; } 
"="  { return tEQL; }  
"&" { return tAMPER; } 
"*" { return tSTAR; } 
"~" {return tTILDE;}
"!"               {return tBANG;}
"<"               {return tLT;}
">"               {return tGT;}
";"               {return tSEMI;}
"."               {return tDOT;}
"::"              {return tCOLON2;}
":"               {return tCOLON;}
"?"               {return tEH;}
"|"               {return tPIPE;}

lable { return keyword_tLABLE; } 

"+="|"-="|"*="|"/="|"%="|"**="|"&="|"|="|"^="|"<<="|">>="|"&&="|"||=" {
    return tOP_ASGN;
}

gsub!  {
    return tFID;
}

(-[ \t]*)?[:]?[&]?[@$a-zA-Z_]{tIDENTIFIER_INSIDE}*[\?!]? { //[*]?[:&*]?[.]? убрали ВЕРНУЛ [:]?[&]?
    return tIDENTIFIER;
}


{NUMBER}|({STRING_QUOTE}[ \t]*)+ {
    return tCONSTANT;
}


"%w" { return keyword_format; }

,[ \t]*\n { return ','; }
=[ \t]*\n { return '='; }
\\\n { }

\{[ \t]*\} { return tCONSTANT; } 
[ \r\t]+ { }

[ \r\t\n]*\n {
  if (!ignore_nl){
    return '\n';
  }
}

. {
  return yytext[0];
}


%%

int main(int argc, char** argv) {
  yyin = fopen(argv[1], "r");
  if (yyin == NULL)
    {
          printf("File wasn't found\n");
          return -1;
      }
      yyparse();
  printf("Access. Code is correct\n");
  return 0;

}
int yywrap(void)
{
	return 1;
}

void yyerror(const char *s) {
  printf("Error [%d,%d]: %s\n", yylloc.last_line, yylloc.last_column, s);
  exit(1);
}