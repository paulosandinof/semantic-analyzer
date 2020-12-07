%{
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#include "y.tab.h"

entry_t** constant_table;

int cmnt_strt = 0;
%}

letter [a-zA-Z]
digit [0-9]
ws  [ \t\r\f\v]+
identifier (_|{letter})({letter}|{digit}|_){0,31}
hex [0-9a-f]


%x CMNT

%%
"int"                             {fprintf(yyout,"%s",yytext);  return INT;}
"char"                            {fprintf(yyout,"%s",yytext);  return CHAR;}
"float"                           {fprintf(yyout,"%s",yytext);  return FLOAT;}
"void"                            {fprintf(yyout,"%s",yytext);  return VOID;}
"long"                            {fprintf(yyout,"%s",yytext);  return LONG;}
"long long"                       {fprintf(yyout,"%s",yytext);  return LONG_LONG;}
"short"                           {fprintf(yyout,"%s",yytext);  return SHORT;}
"signed"                          {fprintf(yyout,"%s",yytext);  return SIGNED;}
"unsigned"                        {fprintf(yyout,"%s",yytext);  return UNSIGNED;}
"for"                             {fprintf(yyout,"%s",yytext);  return FOR;}
"while"                           {fprintf(yyout,"%s",yytext);  return WHILE;}
"if"                              {fprintf(yyout,"%s",yytext);  return IF;}
"else"                            {fprintf(yyout,"%s",yytext);  return ELSE;}
"return"                          {fprintf(yyout,"%s",yytext);  return RETURN;}
"{"                               {fprintf(yyout,"%s",yytext);  return (LBRACE);}
"}"                               {fprintf(yyout,"%s",yytext);  return (RBRACE);}
"("	                              {fprintf(yyout,"%s",yytext);  return (LPAREN);}
")"	                              {fprintf(yyout,"%s",yytext);  return (RPAREN);}
"["                               {fprintf(yyout,"%s",yytext);  return (LBRACKET);}
"]"                               {fprintf(yyout,"%s",yytext);  return (RBRACKET);}
","                               {fprintf(yyout,"%s",yytext);  return (COMMA);}
";"                               {fprintf(yyout,"%s",yytext);  return (SEMICOLON);}
"+"                               {fprintf(yyout,"%s",yytext);  return (PLUS);}
"-"                               {fprintf(yyout,"%s",yytext);  return (MINUS);}
"*"                               {fprintf(yyout,"%s",yytext);  return (MULT);}
"/"                               {fprintf(yyout,"%s",yytext);  return (DIV);}
"%"                               {fprintf(yyout,"%s",yytext);  return (MOD);}
"="                               {fprintf(yyout,"%s",yytext);  return (ASSIGN);}

print                           {fprintf(yyout,"%s",yytext); fprintf(yyout,"%s","f");  return PRINT;}
scan                            {fprintf(yyout,"%s",yytext); fprintf(yyout,"%s","f"); return SCAN;}

{identifier}                      {fprintf(yyout," %s ",yytext);  return IDENTIFIER;}
{ws}                              ;
[+\-]?[0][x|X]{hex}+[lLuU]?       { fprintf(yyout," %s ",yytext);yylval.entry = insert(constant_table,yytext,(int)strtol(yytext, NULL, 16),INT); return  HEX_CONSTANT;}
'({letter}|{digit})'              { fprintf(yyout," %s ",yytext);yylval.entry = insert(constant_table,yytext,yytext[1],CHAR); return CHAR_CONSTANT;}
[+\-]?{digit}+[lLuU]?             { fprintf(yyout," %s ",yytext);yylval.entry = insert(constant_table,yytext,atoi(yytext),INT); return  DEC_CONSTANT;}
[+\-]?{digit}*\.{digit}+          { fprintf(yyout," %s ",yytext);yylval.entry = insert(constant_table,yytext,atof(yytext),FLOAT); return FLOAT_CONSTANT;}

"/*"                              {cmnt_strt = yylineno; BEGIN CMNT;}
<CMNT>.|{ws}                      ;
<CMNT>\n                          {yylineno++;}
<CMNT>"*/"                        {BEGIN INITIAL;}
<CMNT>"/*"                        {printf("Line %3d: Nested comments are not valid!\n",yylineno);}
<CMNT><<EOF>>                     {printf("Line %3d: Unterminated comment\n", cmnt_strt); yyterminate();}

"//".*                            ;

\"[^\"\n]*\"                      {
                                    if(yytext[yyleng-2]=='\\')
                                    {
                                      yyless(yyleng-1);
                                      yymore();
                                    }
                                    else
                                    {
                                      fprintf(yyout," %s ",yytext);
                                      insert(constant_table,yytext,INT_MAX,STRING);
                                      return STRING;
                                    }
                                    fprintf(yyout," %s ",yytext);
                                  }

\"[^\"\n]*$                     {printf("Line %3d: Unterminated string %s\n",yylineno,yytext);}
{digit}+({letter}|_)+	          {printf("Line %3d: Illegal identifier name %s\n",yylineno,yytext);}
\n                              {fprintf(yyout,"%s","\n");yylineno++;}

"--"			                      {fprintf(yyout," %s ",yytext);  return DECREMENT;}
"++"			                      {fprintf(yyout," %s ",yytext);  return INCREMENT;}
"+="                            {fprintf(yyout," %s ",yytext);  return ADD_ASSIGN;}
"-="                            {fprintf(yyout," %s ",yytext);  return SUB_ASSIGN;}
"*="                            {fprintf(yyout," %s ",yytext);  return MUL_ASSIGN;}
"/="                            {fprintf(yyout," %s ",yytext);  return DIV_ASSIGN;}
"%="                            {fprintf(yyout," %s ",yytext);  return MOD_ASSIGN;}


"&&" | "and"			              {fprintf(yyout," %s ",yytext);  return LOGICAL_AND;}
"||" | "or"	                    {fprintf(yyout," %s ",yytext);  return LOGICAL_OR;}
"<="			                      {fprintf(yyout," %s ",yytext);  return LS_EQ;}
">="			                      {fprintf(yyout," %s ",yytext);  return GR_EQ;}
"<"                             {fprintf(yyout," %s ",yytext);  return (LESS_THAN);}
">"                             {fprintf(yyout," %s ",yytext);  return (MORE_THAN);}

"=="			                      {fprintf(yyout," %s ",yytext);  return EQ;}
"!="		                        {fprintf(yyout," %s ",yytext);  return NOT_EQ;}

.                               {return yytext[0];}
%%

#ifndef yywrap
   int yywrap (void) {return 1;}
#endif