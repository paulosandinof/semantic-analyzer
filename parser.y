%{
	#include <stdlib.h>
	#include <stdio.h>
	int yyerror(char *msg);

	#include "symboltable.h"
	#include "lex.yy.c"

	#define SYMBOL_TABLE symbol_table_list[current_scope].symbol_table

  	extern entry_t** constant_table;

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

	void type_check(int,int,int);
%}

%union
{
	int data_type;
	entry_t* entry;
	int ret;
}

%token <entry> IDENTIFIER

%token <entry> DEC_CONSTANT HEX_CONSTANT CHAR_CONSTANT FLOAT_CONSTANT

%token STRING

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

%type <data_type> sub_expr
%type <data_type> unary_expr
%type <data_type> arithmetic_expr
%type <data_type> assignment_expr
%type <data_type> function_call
%type <data_type> array_access
%type <data_type> lhs

%type <ret> single_stmt
%type <ret> stmt
%type <ret> if_block
%type <ret> compound_stmt
%type <ret> statements

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


%%

/* Programa eh composto de varios blocos. */
starter: starter builder
		|builder
		;

/* Cada bloco eh uma funcao ou declaracao */
builder: function
		|declaration
		;

/* Especificacao de funcao */
function: func_or_variable_declaration
		compound_stmt	{
							is_func = 0;

							if ($2 == 0 && func_type != 283) {
								yyerror("return statement not covering all branches");
							}
						}
        ;

data_type: sign_specifier type_specifier
    		|type_specifier
    		;

sign_specifier: SIGNED
    			|UNSIGNED
    			;

type_specifier: INT	{current_dtype = INT;}
    |SHORT INT      {current_dtype = SHORT;}
    |SHORT          {current_dtype = SHORT;}
    |LONG           {current_dtype = LONG;}
	|LONG INT       {current_dtype = LONG;}
    |LONG_LONG      {current_dtype = LONG_LONG;}
    |LONG_LONG INT  {current_dtype = LONG_LONG;}
	|CHAR 			{current_dtype = CHAR;}
	|FLOAT 			{current_dtype = FLOAT;}
	|VOID			{current_dtype = VOID;}
    ;

 /* Regras para lista de argumento */
argument_list : arguments
    			|
    			;
 /* Argumentos sao separados por virgula*/
arguments : arguments COMMA arg
    		|arg
    		;

 /* Argumentos sao pares de tipo e id */
arg : data_type {is_declaration = 1; } identifier {param_list[p_idx++] = $3->data_type;}
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
						}
    ;

statements: statements stmt { 
								if ($1 == 1 || $2 == 1) {
									$$ = 1;
								} 
							}
    | { $$ = 0; }
    ;

single_stmt: if_block 					{ $$ = $1; }
    		|for_block 					{ $$ = 0; }
    		|while_block 				{ $$ = 0; }
    		|declaration 				{ $$ = 0; }
			|print_call					{ $$ = 0; }
			|scan_call					{ $$ = 0; }
    		|function_call SEMICOLON 	{ $$ = 0; }
			|RETURN SEMICOLON			{
											$$ = 1;
											if(is_func)
											{
												if(func_type != VOID)
													yyerror("return type (VOID) does not match function type");
											}
											else yyerror("return statement not inside function definition");
										}
			|RETURN sub_expr SEMICOLON	{
											$$ = 1;
											if(is_func)
											{
												if(func_type != $2)
													yyerror("return type does not match function type");
											}
											else yyerror("return statement not in function definition");
										}
    ;

print_call: PRINT LPAREN IDENTIFIER RPAREN SEMICOLON
    ;
scan_call: SCAN LPAREN IDENTIFIER RPAREN SEMICOLON
    ;

for_block: FOR LPAREN expression_stmt  expression_stmt RPAREN {is_loop = 1;} stmt {is_loop = 0;}
    	|FOR LPAREN expression_stmt expression_stmt expression RPAREN {is_loop = 1;} stmt {is_loop = 0;}
    ;

if_block: IF LPAREN expression RPAREN stmt 			{ $$ = 0; }		%prec LOWER_THAN_ELSE
		|IF LPAREN expression RPAREN stmt ELSE stmt { 
														if ($5 == 1 && $7 == 1) {
															$$ = 1;
														} 
													}
    ;

while_block: WHILE LPAREN expression RPAREN {is_loop = 1;} stmt {is_loop = 0;}
	;

func_or_variable_declaration: data_type {is_declaration = 1;} func_or_variable_id
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
										}
		|declaration_list SEMICOLON 	{is_declaration = 0;}
	;

declaration: func_or_variable_declaration
			|declaration_list SEMICOLON
			|unary_expr SEMICOLON
	;

declaration_list: declaration_list COMMA sub_decl
				|sub_decl
	;

sub_decl: assignment_expr
    	|identifier
    	|array_access
	;

/* Podemos ter expressoes vazias dentro de loops */
expression_stmt: expression SEMICOLON
    			|SEMICOLON
    ;

expression: expression COMMA sub_expr
    		|sub_expr
	;

sub_expr: sub_expr MORE_THAN sub_expr		{type_check($1,$3,2); $$ = $1;}
    	|sub_expr LESS_THAN sub_expr		{type_check($1,$3,2); $$ = $1;}
    	|sub_expr EQ sub_expr				{type_check($1,$3,2); $$ = $1;}
    	|sub_expr NOT_EQ sub_expr			{type_check($1,$3,2); $$ = $1;}
    	|sub_expr LS_EQ sub_expr			{type_check($1,$3,2); $$ = $1;}
    	|sub_expr GR_EQ sub_expr			{type_check($1,$3,2); $$ = $1;}
		|sub_expr LOGICAL_AND sub_expr		{type_check($1,$3,2); $$ = $1;}
		|sub_expr LOGICAL_OR sub_expr		{type_check($1,$3,2); $$ = $1;}
		|NOT sub_expr						{$$ = $2;}
		|arithmetic_expr					{$$ = $1;}
    	|assignment_expr					{$$ = $1;}
		|unary_expr							{$$ = $1;}
    ;


assignment_expr: lhs assign_op arithmetic_expr		{type_check($1,$3,1); $$ = $3; rhs=0;}
    			|lhs assign_op array_access			{type_check($1,$3,1); $$ = $3; rhs=0;}
    			|lhs assign_op function_call		{type_check($1,$3,1); $$ = $3; rhs=0;}
				|lhs assign_op unary_expr			{type_check($1,$3,1); $$ = $3; rhs=0;}
				|unary_expr assign_op unary_expr	{type_check($1,$3,1); $$ = $3; rhs=0;}
    ;

unary_expr:	identifier INCREMENT	{$$ = $1->data_type;}
			|identifier DECREMENT	{$$ = $1->data_type;}
			|DECREMENT identifier	{$$ = $2->data_type;}
			|INCREMENT identifier	{$$ = $2->data_type;}
	;

lhs: identifier		{$$ = $1->data_type;}
   |array_access	{$$ = $1;}
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

assign_op: ASSIGN		{rhs=1;}
    	|ADD_ASSIGN 	{rhs=1;} 
    	|SUB_ASSIGN 	{rhs=1;}
    	|MUL_ASSIGN 	{rhs=1;}
    	|DIV_ASSIGN 	{rhs=1;}
    	|MOD_ASSIGN 	{rhs=1;}
    ;

arithmetic_expr: arithmetic_expr PLUS arithmetic_expr	{type_check($1,$3,0);}
    			|arithmetic_expr MINUS arithmetic_expr	{type_check($1,$3,0);}
    			|arithmetic_expr MULT arithmetic_expr	{type_check($1,$3,0);}
    			|arithmetic_expr DIV arithmetic_expr	{type_check($1,$3,0);}
				|arithmetic_expr MOD arithmetic_expr	{type_check($1,$3,0);}
				|LPAREN arithmetic_expr RPAREN			{$$ = $2;}
    			|MINUS arithmetic_expr %prec UMINUS		{$$ = $2;}
    			|identifier								{$$ = $1->data_type;}
    			|constant								{$$ = $1->data_type;}
    ;

constant: DEC_CONSTANT 	{$1->is_constant=1; $$ = $1;}
    	|HEX_CONSTANT	{$1->is_constant=1; $$ = $1;}
		|CHAR_CONSTANT	{$1->is_constant=1; $$ = $1;}
		|FLOAT_CONSTANT	{$1->is_constant=1; $$ = $1;}
    ;

array_access: identifier LBRACKET array_index RBRACKET	{
															if(is_declaration)
															{
																if($3->value <= 0)
																	yyerror("size of array is not positive");
																else
                                                                	if($3->is_constant && !rhs)
																		$1->array_dimension = $3->value;
																	else if(rhs)
																	{
																		if($3->value > $1->array_dimension)
																			yyerror("Array index out of bound");
																		if($3->value < 0)
																			yyerror("Array index cannot be negative");
																	}
															}
															else if($3->is_constant)
															{
																if($3->value > $1->array_dimension)
																	yyerror("Array index out of bound");
																if($3->value < 0)
																	yyerror("Array index cannot be negative");
															}
															$$ = $1->data_type;
														}
	;

array_index: constant	{$$ = $1;}
			|identifier	{$$ = $1;}
	;

function_call: identifier LPAREN parameter_list RPAREN	{
															$$ = $1->data_type;
															check_parameter_list($1,param_list,p_idx);
															p_idx = 0;
														}
            |identifier LPAREN RPAREN					{
							 								$$ = $1->data_type;
															check_parameter_list($1,param_list,p_idx);
															p_idx = 0;
														}
    ;

parameter_list: parameter_list COMMA  parameter
            |parameter
    ;

parameter: sub_expr	{param_list[p_idx++] = $1;}
		|STRING		{param_list[p_idx++] = STRING;}
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
	yyout=fopen("output.c","w"); 
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
