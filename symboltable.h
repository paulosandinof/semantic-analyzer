#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>

#define HASH_TABLE_SIZE 100
#define NUM_TABLES 10

int table_index = 0;
int current_scope = 0;

/* Struct para armazenar cada entrada */
struct entry_s
{
	char *lexeme;
	double value;
	int data_type;
	int *parameter_list; // Para funcoes
	int array_dimension;
	int is_constant;
	int num_params;
	struct entry_s *successor;
};

typedef struct entry_s entry_t;

/* Wrapper para tabela de simbolos com ponteiro para o escopo pai */
struct table_s
{
	entry_t **symbol_table;
	int parent;
};

typedef struct table_s table_t;

/* Struct com os tipos do Yacc, não consegui definir no próprio parser.y */
struct types_s
{
		int data_type;
		char* code;
		int ret;
};

typedef struct types_s types_t;

extern table_t symbol_table_list[NUM_TABLES];

/* Cria nova hash_table. */
entry_t **create_table()
{
	entry_t **hash_table_ptr = NULL;

	if ((hash_table_ptr = malloc(sizeof(entry_t *) * HASH_TABLE_SIZE)) == NULL)
		return NULL;

	int i;

	for (i = 0; i < HASH_TABLE_SIZE; i++)
	{
		hash_table_ptr[i] = NULL;
	}

	return hash_table_ptr;
}

int create_new_scope()
{
	table_index++;

	symbol_table_list[table_index].symbol_table = create_table();
	symbol_table_list[table_index].parent = current_scope;

	return table_index;
}

int exit_scope()
{
	return symbol_table_list[current_scope].parent;
}

/* Gera hash apartir de uma string e retorna um index em [0, HASH_TABLE_SIZE)*/
uint32_t hash(char *lexeme)
{
	size_t i;
	uint32_t hash;

	for (hash = i = 0; i < strlen(lexeme); ++i)
	{
		hash += lexeme[i];
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}
	hash += (hash << 3);
	hash ^= (hash >> 11);
	hash += (hash << 15);

	return hash % HASH_TABLE_SIZE;
}

/* Cria uma entrada para um lexema e par de token */
entry_t *create_entry(char *lexeme, int value, int data_type)
{
	entry_t *new_entry;

	if ((new_entry = malloc(sizeof(entry_t))) == NULL)
	{
		return NULL;
	}

	/* Copia lexeme para new_entry usando strdup. Retorna NULL caso falhe */
	if ((new_entry->lexeme = strdup(lexeme)) == NULL)
	{
		return NULL;
	}

	new_entry->value = value;
	new_entry->successor = NULL;
	new_entry->parameter_list = NULL;
	new_entry->array_dimension = -1;
	new_entry->is_constant = 0;
	new_entry->num_params = 0;
	new_entry->data_type = data_type;

	return new_entry;
}

/* Procura por um lexema. Retorna um ponteiro para a entrada caso o lexema exista e NULL caso contrario */
entry_t *search(entry_t **hash_table_ptr, char *lexeme)
{
	uint32_t idx = 0;
	entry_t *myentry;

	idx = hash(lexeme);

	myentry = hash_table_ptr[idx];

	while (myentry != NULL && strcmp(lexeme, myentry->lexeme) != 0)
	{
		myentry = myentry->successor;
	}

	if (myentry == NULL)
		return NULL;

	else
		return myentry;
}

/* Pocura recursivamente em todos os escopos pais pelo lexema */
entry_t *search_recursive(char *lexeme)
{
	int idx = current_scope;
	entry_t *finder = NULL;

	while (idx != -1)
	{
		finder = search(symbol_table_list[idx].symbol_table, lexeme);

		if (finder != NULL)
			return finder;

		idx = symbol_table_list[idx].parent;
	}

	return finder;
}

/* Insere uma entrada na hash table. */
entry_t *insert(entry_t **hash_table_ptr, char *lexeme, int value, int data_type)
{
	entry_t *finder = search(hash_table_ptr, lexeme);

	// Se o lexema ja existir, nao inserir
	if (finder != NULL)
	{
		if (finder->is_constant)
			return finder;
		return NULL;
	}

	uint32_t idx;
	entry_t *new_entry = NULL;
	entry_t *head = NULL;

	idx = hash(lexeme);
	new_entry = create_entry(lexeme, value, data_type);

	if (new_entry == NULL)
	{
		printf("Insert failed. New entry could not be created.");
		exit(1);
	}

	head = hash_table_ptr[idx];

 	// Primeiro lexema nesse hash
	if (head == NULL)
	{
		hash_table_ptr[idx] = new_entry;
	}
	else
	{
		new_entry->successor = hash_table_ptr[idx];
		hash_table_ptr[idx] = new_entry;
	}

	return hash_table_ptr[idx];
}

int check_parameter_list(entry_t *entry, int *list, int num_args)
{
	int *parameter_list = entry->parameter_list;

	if (num_args != entry->num_params)
	{
		yyerror("Number of parameters and arguments do not match");
	}

	int i;
	for (i = 0; i < num_args; i++)
	{
		if (list[i] != parameter_list[i])
			yyerror("Parameter and argument types do not match");
	}

	return 1;
}

void fill_parameter_list(entry_t *entry, int *list, int num_params)
{
	entry->parameter_list = (int *)malloc(num_params * sizeof(int));

	int i;
	for (i = 0; i < num_params; i++)
	{
		entry->parameter_list[i] = list[i];
	}

	entry->num_params = num_params;
}

void print_dashes(int n)
{
	printf("\n");

	int i;
	for (i = 0; i < n; i++)
		printf("=");
		
	printf("\n");
}

void display_symbol_table(entry_t **hash_table_ptr)
{
	int i;
	entry_t *traverser;

	print_dashes(100);

	printf(" %-20s %-20s %-20s %-20s %-20s\n", "lexeme", "data-type", "array_dimension", "num_params", "param_list");

	print_dashes(100);

	for (i = 0; i < HASH_TABLE_SIZE; i++)
	{
		traverser = hash_table_ptr[i];
		while (traverser != NULL)
		{
			printf(" %-20s %-20d %-20d ", traverser->lexeme, traverser->data_type, traverser->array_dimension);

			printf(" %-20d", traverser->num_params);

			int j;
			for (j = 0; j < traverser->num_params; j++)
				printf(" %d", traverser->parameter_list[j]);
			printf("\n");

			traverser = traverser->successor;
		}
	}

	print_dashes(100);
}

void display_constant_table(entry_t **hash_table_ptr)
{
	int i;
	entry_t *traverser;

	print_dashes(25);

	printf(" %-10s %-10s \n", "lexeme", "data-type");

	print_dashes(25);

	for (i = 0; i < HASH_TABLE_SIZE; i++)
	{
		traverser = hash_table_ptr[i];
		while (traverser != NULL)
		{
			printf(" %-10s %-10d \n", traverser->lexeme, traverser->data_type);
			traverser = traverser->successor;
		}
	}

	print_dashes(25);
}

void display_all()
{
	int i;
	for (i = 0; i <= table_index; i++)
	{
		printf("Scope: %d\n", i);
		display_symbol_table(symbol_table_list[i].symbol_table);
		printf("\n\n");
	}
}