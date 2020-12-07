%{
	#include <stdlib.h>
	#include <stdio.h>
	#include <stdarg.h>
	int yyerror(char *msg);

	#include "symboltable.h"
	#include "lex.yy.c"

	#define SYMBOL_TABLE symbol_table_list[current_scope].symbol_table

  	extern entry_t** constant_table;

	char *concat(int count, ...);

	int current_dtype;

	table_t symbol_table_list[NUM_TABLES];

	int is_declaration = 0;
	int is_loop = 0;
	int is_func = 0;
	int func_type;

	int param_list[10];
	int p_idx = 0;
	int p=0;
    int rhs = 0;
    int n_label = 0;

	void type_check(int,int,int);
%}

%union
{
	int data_type;
	entry_t* entry;
	types_t types;
}

%token <entry> IDENTIFIER

%token <entry> DEC_CONSTANT HEX_CONSTANT CHAR_CONSTANT FLOAT_CONSTANT

%token LOGICAL_AND LOGICAL_OR LS_EQ GR_EQ EQ NOT_EQ

%token MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN

%token INCREMENT DECREMENT

%token SHORT INT LONG LONG_LONG SIGNED UNSIGNED VOID CHAR FLOAT

%token IF FOR WHILE RETURN PRINT SCAN

%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE

%token PLUS MINUS MULT DIV MOD ASSIGN

%token COMMA NOT LESS_THAN MORE_THAN SEMICOLON

%type <entry> identifier
%type <entry> constant
%type <entry> array_index

%type <types> sub_expr
%type <types> unary_expr
%type <types> arithmetic_expr
%type <types> assignment_expr
%type <types> function_call
%type <types> array_access
%type <types> lhs

%type <types> single_stmt
%type <types> stmt
%type <types> if_block
%type <types> compound_stmt
%type <types> statements

%type <types> parameter_list
%type <types> assign_op
%type <types> expression
%type <types> expression_stmt
%type <types> sub_decl
%type <types> declaration_list
%type <types> declaration
%type <types> func_or_variable_declaration
%type <types> func_or_variable_id
%type <types> data_type
%type <types> type_specifier
%type <types> sign_specifier
%type <types> while_block
%type <types> for_block
%type <types> arg
%type <types> arguments
%type <types> argument_list
%type <types> function
%type <types> builder
%type <types> starter
%type <types> print_call
%type <types> scan_call

%left COMMA
%right ASSIGN
%left LOGICAL_OR
%left LOGICAL_AND
%left EQ NOT_EQ
%left LESS_THAN MORE_THAN LS_EQ GR_EQ
%left PLUS MINUS
%left MULT DIV MOD
%right NOT


%nonassoc UMINUS
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%start printer

%%

printer: starter { fprintf(yyout, "%s", $1.code); }
	;

/* Programa eh composto de varios blocos. */
starter: starter builder 	{ $$.code = concat(2, $1.code, $2.code); }
		|builder			{ $$.code = $1.code; }
		;

/* Cada bloco eh uma funcao ou declaracao */
builder: function		{ $$.code = $1.code; }
		|declaration	{ $$.code = $1.code; }
		;

/* Especificacao de funcao */
function: func_or_variable_declaration
		compound_stmt	{
							is_func = 0;

							if ($2.ret == 0 && func_type != 283) {
								yyerror("return statement not covering all branches");
							}

							$$.code = concat(2, $1.code, $2.code);
						}
        ;

data_type: sign_specifier type_specifier	{ $$.code = concat(2, $1.code, $2.code); }
    		|type_specifier					{ $$.code = $1.code; }
    		;

sign_specifier: SIGNED		{ $$.code = "signed "; }
    			|UNSIGNED	{ $$.code = "unsigned "; }
    			;

type_specifier: INT	{current_dtype = INT; $$.code = "int"; }
    |SHORT INT      {current_dtype = SHORT; $$.code = "short int"; }
    |SHORT          {current_dtype = SHORT; $$.code = "short"; }
    |LONG           {current_dtype = LONG; $$.code = "long"; }
	|LONG INT       {current_dtype = LONG; $$.code = "long int"; }
    |LONG_LONG      {current_dtype = LONG_LONG; $$.code = "long long"; }
    |LONG_LONG INT  {current_dtype = LONG_LONG; $$.code = "long long int"; }
	|CHAR 			{current_dtype = CHAR; $$.code = "char"; }
	|FLOAT 			{current_dtype = FLOAT; $$.code = "float"; }
	|VOID			{current_dtype = VOID; $$.code = "void"; }
    ;

 /* Regras para lista de argumento */
argument_list : arguments	{ $$.code = $1.code; }
    			|			{ $$.code = ""; }
    			;
 /* Argumentos sao separados por virgula*/
arguments : arguments COMMA arg	{ $$.code = concat(3, $1.code, ", ", $3.code); }
    		|arg				{ $$.code = $1.code; }
    		;

 /* Argumentos sao pares de tipo e id */
arg : data_type {is_declaration = 1; } identifier {param_list[p_idx++] = $3->data_type; $$.code = concat(3, $1.code, " ", $3->lexeme); }
    ;

 /* Statement generico*/
stmt: compound_stmt { $$ = $1; }
    | single_stmt 	{ $$ = $1; }
    ;

 /* O corpo da funcao esta entre chaves e tem multiplos statements */
compound_stmt: LBRACE	{
							if(!p)current_scope = create_new_scope();
							else p = 0;
						}
			statements
			RBRACE 		{
							current_scope = exit_scope();
							$$ = $3;
							$$.code = concat(3, "{\n", $3.code, "}\n");
						}
    ;

statements: statements stmt { 
								if ($1.ret == 1 || $2.ret == 1) {
									$$.ret = 1;
								}
								$$.code = concat(2, $1.code, $2.code);
							}
    |	{ 
			$$.ret = 0;
			$$.code = ""; 
		}
    ;

single_stmt: if_block 					{ $$.ret = $1.ret; $$.code = $1.code; }
    		|for_block 					{ $$.ret = 0; $$.code = $$.code; }
    		|while_block 				{ $$.ret = 0; $$.code = $1.code; }
    		|declaration 				{ $$.ret = 0; $$.code = $1.code; }
			|print_call					{ $$.ret = 0; $$.code = $1.code; }
			|scan_call					{ $$.ret = 0; $$.code = $1.code; }
    		|function_call SEMICOLON 	{ $$.ret = 0; $$.code = concat(2, $1.code, ";\n"); }
			|RETURN SEMICOLON			{
											$$.ret = 1;
											if(is_func) {
												if(func_type != VOID) {
													yyerror("return type (VOID) does not match function type");
												}
											} else {
												yyerror("return statement not inside function definition");
											};

											$$.code = "return;\n";
										}
			|RETURN sub_expr SEMICOLON	{
											$$.ret = 1;
											if(is_func) {
												if(func_type != $2.data_type) {
													yyerror("return type does not match function type");
												}
											} else {
												yyerror("return statement not in function definition");
											}

											$$.code = concat(3, "return ", $2.code, ";\n");
										}
    ;

print_call: PRINT LPAREN identifier RPAREN SEMICOLON	{	
															char *type = " ";
															
															switch($3->data_type){
																case 277:
																	type = "i";
																	break;
																case 283:
																	type = "c";
																	break;
																case 284:
																	type = "f";
																	break;
															}

															$$.code=concat(5, "printf(\"%",  type ,"\", ", $3->lexeme, ");\n");
														}
    ;
scan_call: SCAN LPAREN identifier RPAREN SEMICOLON		{	char *type = " ";
															
															switch($3->data_type){
																case 277:
																	type = "i";
																	break;
																case 283:
																	type = "c";
																	break;
																case 284:
																	type = "f";
																	break;
															}

															$$.code=concat(5, "scanf(\"%",  type ,"\", ", $3->lexeme, ");\n"); 	}
    ;

for_block: FOR LPAREN expression_stmt  expression_stmt RPAREN {is_loop = 1;} stmt 			{
																								is_loop = 0;  
																								char* cond = concat(1, $4.code);

																								for(int i=0; i<strlen(cond)-1; i++){
																									if(cond[i] == ';')
																										cond[i] = '\0';
																								}

																								char num[50]; 
																								sprintf(num, "%d", n_label); 
																								n_label++;
																								$$.code = concat(14, $3.code, "\nif(", cond, ") goto l", num, ";\nl", num, ":{\n", $7.code, ";\nif(", cond, ") goto l", num, ";\n}\n");
																								}
    	|FOR LPAREN expression_stmt expression_stmt expression RPAREN {is_loop = 1;} stmt 	{
																								is_loop = 0; 
																								char* cond = concat(1, $4.code);

																								for(int i=0; i<strlen(cond)-1; i++){
																									if(cond[i] == ';')
																										cond[i] = '\0';
																								}

																								char num[50]; 
																								sprintf(num, "%d", n_label); 
																								n_label++;
																								$$.code = concat(15, $3.code, "\nif(", cond, ") goto l", num, ";\nl", num, ":{\n", $8.code, $5.code, ";\nif(", cond, ") goto l", num, ";\n}\n");
																							}
    ;

if_block: IF LPAREN expression RPAREN stmt 			{ 
														$$.ret = 0; 

														char num[50]; 
														sprintf(num, "%d", n_label); 
														n_label++;

														$$.code = concat(9, "if", "(", $3.code, ") goto l", num, ";\nl", num, ":", $5.code); 
													}	%prec LOWER_THAN_ELSE
		|IF LPAREN expression RPAREN stmt ELSE stmt { 
														char if_num[50], el_num[50]; 
														sprintf(if_num, "%d", n_label); 
														n_label++; 
														sprintf(el_num, "%d", n_label); 
														n_label++;

														if ($5.ret == 1 && $7.ret == 1) {
															$$.ret = 1;
														}

														$$.code = concat(15, "if", "(", $3.code, ") goto l", if_num, ";\nl", if_num, ":", $5.code, "goto l", el_num, ";\nl", el_num, ":", $7.code);
													}
    ;

while_block: WHILE LPAREN expression RPAREN {is_loop = 1;} stmt {
																	is_loop = 0; 
																	char num[50]; 
																	sprintf(num, "%d", n_label); 
																	n_label++; 

																	$$.code = concat(14, "if", "(", $3.code, ") goto l", num, ";\nl", num, ":{\n", $6.code, ";\nif(", $3.code, ") goto l", num, ";\n}\n");
																}
	;

func_or_variable_declaration: data_type {is_declaration = 1;} func_or_variable_id	{ $$.code = concat(3, $1.code, " ", $3.code); }
	;

func_or_variable_id: identifier 		{
											func_type = current_dtype;
											is_declaration = 0;
											current_scope = create_new_scope();
										}
		LPAREN argument_list RPAREN 	{
											is_declaration = 0;
											fill_parameter_list($1,param_list,p_idx);
											p_idx = 0;
											is_func = 1;
											p=1;

											$$.code = concat(4, $1->lexeme, "(", $4.code, ")");
										}
		|declaration_list SEMICOLON 	{
											is_declaration = 0;
											$$.code = concat(2, $1.code, ";\n");
										}
	;

declaration: func_or_variable_declaration	{ $$.code = $1.code; }
			|declaration_list SEMICOLON		{ $$.code = concat(2, $1.code, ";\n"); }
			|unary_expr SEMICOLON			{ $$.code = concat(2, $1.code, ";\n"); }
	;

declaration_list: declaration_list COMMA sub_decl	{ $$.code = concat(3, $1.code, ", ", $3.code); }
				|sub_decl							{ $$.code = $1.code; }
	;

sub_decl: assignment_expr	{ $$.code = $1.code; }
    	|identifier			{ $$.data_type = $1->data_type; $$.code = $1->lexeme; }
    	|array_access		{ $$.code = $1.code; }
	;

/* Podemos ter expressoes vazias dentro de loops */
expression_stmt: expression SEMICOLON	{ $$.code = concat(2, $1.code, ";\n"); }
    			|SEMICOLON 				{ $$.code = ";\n"; }
    ;

expression: expression COMMA sub_expr	{ $$.code = concat(3, $1.code, ", ", $3.code); }
    		|sub_expr					{ $$.code = $1.code; }
	;

sub_expr: sub_expr MORE_THAN sub_expr		{ type_check($1.data_type, $3.data_type, 2); $$.data_type = $1.data_type; $$.code = concat(3, $1.code, " > ", $3.code); }
    	|sub_expr LESS_THAN sub_expr		{ type_check($1.data_type, $3.data_type, 2); $$.data_type = $1.data_type; $$.code = concat(3, $1.code, " < ", $3.code); }
    	|sub_expr EQ sub_expr				{ type_check($1.data_type, $3.data_type, 2); $$.data_type = $1.data_type; $$.code = concat(3, $1.code, " == ", $3.code); }
    	|sub_expr NOT_EQ sub_expr			{ type_check($1.data_type, $3.data_type, 2); $$.data_type = $1.data_type; $$.code = concat(3, $1.code, " != ", $3.code); }
    	|sub_expr LS_EQ sub_expr			{ type_check($1.data_type, $3.data_type, 2); $$.data_type = $1.data_type; $$.code = concat(3, $1.code, " <= ", $3.code); }
    	|sub_expr GR_EQ sub_expr			{ type_check($1.data_type, $3.data_type, 2); $$.data_type = $1.data_type; $$.code = concat(3, $1.code, " >= ", $3.code); }
		|sub_expr LOGICAL_AND sub_expr		{ type_check($1.data_type, $3.data_type, 2); $$.data_type = $1.data_type; $$.code = concat(3, $1.code, " && ", $3.code); }
		|sub_expr LOGICAL_OR sub_expr		{ type_check($1.data_type, $3.data_type, 2); $$.data_type = $1.data_type; $$.code = concat(3, $1.code, " || ", $3.code); }
		|NOT sub_expr						{ $$.data_type = $2.data_type; $$.code = concat(2, "!", $2.code); }
		|arithmetic_expr					{ $$.data_type = $1.data_type; $$.code = $1.code; }
    	|assignment_expr					{ $$.data_type = $1.data_type; $$.code = $1.code; }
		|unary_expr							{ $$.data_type = $1.data_type; $$.code = $1.code; }
    ;


assignment_expr: lhs assign_op arithmetic_expr		{type_check($1.data_type, $3.data_type, 1); $$.data_type = $3.data_type; rhs=0; $$.code = concat(3, $1.code, $2.code, $3.code); }
    			|lhs assign_op array_access			{type_check($1.data_type, $3.data_type, 1); $$.data_type = $3.data_type; rhs=0; $$.code = concat(3, $1.code, $2.code, $3.code); }
    			|lhs assign_op function_call		{type_check($1.data_type, $3.data_type, 1); $$.data_type = $3.data_type; rhs=0; $$.code = concat(3, $1.code, $2.code, $3.code); }
				|lhs assign_op unary_expr			{type_check($1.data_type, $3.data_type, 1); $$.data_type = $3.data_type; rhs=0; $$.code = concat(3, $1.code, $2.code, $3.code); }
				|unary_expr assign_op unary_expr	{type_check($1.data_type, $3.data_type, 1); $$.data_type = $3.data_type; rhs=0; $$.code = concat(3, $1.code, $2.code, $3.code); }
    ;

unary_expr:	identifier INCREMENT	{$$.data_type = $1->data_type; $$.code = concat(2, $1->lexeme, "++"); }
			|identifier DECREMENT	{$$.data_type = $1->data_type; $$.code = concat(2, $1->lexeme, "--"); }
			|DECREMENT identifier	{$$.data_type = $2->data_type; $$.code = concat(2, "--", $2->lexeme); }
			|INCREMENT identifier	{$$.data_type = $2->data_type; $$.code = concat(2, "++", $2->lexeme); }
	;

lhs: identifier		{$$.data_type = $1->data_type; $$.code = $1->lexeme;}
   |array_access	{$$.data_type = $1.data_type; $$.code = $1.code;}
	;

identifier: IDENTIFIER  {
                            if(is_declaration && !rhs) 
                            {
                                $1 = insert(SYMBOL_TABLE,yytext,INT_MAX,current_dtype);
                                if($1 == NULL) yyerror("Redeclaration of variable");
                            }
                            else
                            {
                                $1 = search_recursive(yytext);
                                if($1 == NULL) yyerror("Variable not declared");
                            }
                            $$ = $1;
                    	}
    ;

assign_op: ASSIGN		{rhs=1; $$.code = " = "; }
    	|ADD_ASSIGN 	{rhs=1; $$.code = " += "; } 
    	|SUB_ASSIGN 	{rhs=1; $$.code = " -= "; }
    	|MUL_ASSIGN 	{rhs=1; $$.code = " *= "; }
    	|DIV_ASSIGN 	{rhs=1; $$.code = " /= "; }
    	|MOD_ASSIGN 	{rhs=1; $$.code = " %= "; }
    ;

arithmetic_expr: arithmetic_expr PLUS arithmetic_expr	{type_check($1.data_type, $3.data_type, 0); $$.code = concat(3, $1.code, " + ", $3.code); }
    			|arithmetic_expr MINUS arithmetic_expr	{type_check($1.data_type, $3.data_type, 0); $$.code = concat(3, $1.code, " - ", $3.code); }
    			|arithmetic_expr MULT arithmetic_expr	{type_check($1.data_type, $3.data_type, 0); $$.code = concat(3, $1.code, " * ", $3.code); }
    			|arithmetic_expr DIV arithmetic_expr	{type_check($1.data_type, $3.data_type, 0); $$.code = concat(3, $1.code, " / ", $3.code); }
				|arithmetic_expr MOD arithmetic_expr	{type_check($1.data_type, $3.data_type, 0); $$.code = concat(3, $1.code, " % ", $3.code); }
				|LPAREN arithmetic_expr RPAREN			{$$.data_type = $2.data_type; $$.code = concat(3, "(", $2.code, ")"); }
    			|MINUS arithmetic_expr %prec UMINUS		{$$.data_type = $2.data_type; $$.code = concat(2, "-", $2.code); }
    			|identifier								{$$.data_type = $1->data_type; $$.code = $1->lexeme; }
    			|constant								{$$.data_type = $1->data_type; $$.code = $1->lexeme; }
    ;

constant: DEC_CONSTANT 	{$1->is_constant=1; $$ = $1;}
    	|HEX_CONSTANT	{$1->is_constant=1; $$ = $1;}
		|CHAR_CONSTANT	{$1->is_constant=1; $$ = $1;}
		|FLOAT_CONSTANT	{$1->is_constant=1; $$ = $1;}
    ;

array_access: identifier LBRACKET array_index RBRACKET	{
															if(is_declaration) {
																if($3->value <= 0)
																	yyerror("size of array is not positive");
																else
                                                                	if($3->is_constant && !rhs)
																		$1->array_dimension = $3->value;
																	else if(rhs) {
																		if($3->value > $1->array_dimension)
																			yyerror("Array index out of bound");
																		if($3->value < 0)
																			yyerror("Array index cannot be negative");
																	}
															} else if($3->is_constant) {
																if($3->value > $1->array_dimension)
																	yyerror("Array index out of bound");
																if($3->value < 0)
																	yyerror("Array index cannot be negative");
															}
															$$.data_type = $1->data_type;
															$$.code = concat(4, $1->lexeme, "[", $3->lexeme, "]");
														}
	;

array_index: constant	{$$ = $1;}
			|identifier	{$$ = $1;}
	;

function_call: identifier LPAREN parameter_list RPAREN	{
															$$.data_type = $1->data_type;
															check_parameter_list($1,param_list,p_idx);
															p_idx = 0;
															$$.code = concat(4, $1->lexeme, "(", $3.code, ")");
														}
            |identifier LPAREN RPAREN					{
							 								$$.data_type = $1->data_type;
															check_parameter_list($1,param_list,p_idx);
															p_idx = 0;
															$$.code = concat(2, $1->lexeme, "()");
														}
    ;

parameter_list: parameter_list COMMA sub_expr	{ param_list[p_idx++] = $3.data_type; $$.code = concat(3, $1.code, ", ", $3.code); }
            |sub_expr							{ param_list[p_idx++] = $1.data_type; $$.code = $1.code; }
    ;

%%

void type_check(int left, int right, int flag)
{
	if(left != right)
	{
		switch(flag)
		{
			case 0: yyerror("Type mismatch in arithmetic expression"); break;
			case 1: yyerror("Type mismatch in assignment expression"); break;
			case 2: yyerror("Type mismatch in logical expression"); break;
		}
	}
}

char *concat(int count, ...)
{
    va_list ap;
    size_t  len = 0;

    if (count < 1)
        return NULL;

    // First, measure the total length required.
    va_start(ap, count);
    for (int i=0; i < count; i++) {
        const char *s = va_arg(ap, char *);
        len += strlen(s);
    }
    va_end(ap);

    // Allocate return buffer.
    char *ret = malloc(len + 1);
    if (ret == NULL)
        return NULL;

    // Concatenate all the strings into the return buffer.
    char *dst = ret;
    va_start(ap, count);
    for (int i=0; i < count; i++) {
        const char *src = va_arg(ap, char *);

        // This loop is a strcpy.
        while (*dst++ = *src++);
        dst--;
    }
    va_end(ap);
    return ret;
}

int main(int argc, char *argv[])
{
	int i;

	for(i=0; i<NUM_TABLES;i++)
	{
		symbol_table_list[i].symbol_table = NULL;
		symbol_table_list[i].parent = -1;
	}

	constant_table = create_table();
  	symbol_table_list[0].symbol_table = create_table();
	yyin = fopen(argv[1], "r");
	yyout= fopen("output.c","w"); 

	if(!yyparse())
	{
		printf("\nPARSING COMPLETE\n\n\n");
	}
	else
	{
		fclose(yyout);
		remove("./output.c");
		printf("\nPARSING FAILED!\n\n\n");
	}

	/*
	printf("SYMBOL TABLES\n\n");
	display_all();

	printf("CONSTANT TABLE");
	display_constant_table(constant_table);
	*/
	
	fclose(yyin);
	return 0;
}

int yyerror(char *msg)
{
	fclose(yyout);
	remove("./output.c");
	printf("Line no: %d Error message: %s Token: %s\n", yylineno, msg, yytext);
	exit(0);
}
