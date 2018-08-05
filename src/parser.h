#ifndef PARSER_H
#define PARSER_H

#include "core.h"
#include "util.h"

#include "lexer.h"

// Pre-declare types 
struct expr_s; 
struct stmt_s;

typedef struct expr_s expr_t;
typedef struct stmt_s stmt_t;

typedef struct
{
	size_t count;
	size_t size;
	stmt_t **data;
} stmtList_t;

/* Abstract Syntax Tree structures */
typedef enum
{
	EXPR_NONE,
	EXPR_GROUP,
	EXPR_UNARY,
	EXPR_BINARY,
	EXPR_LITERAL,
	EXPR_VARIABLE,
	EXPR_ASSIGNMENT,
} exprType_t;
struct expr_s
{
	exprType_t type;
	union
	{
		struct
		{
			expr_t *expression;
		} group;
		struct
		{
			token_t operator;
			expr_t *right;
		} unary;
		struct
		{
			token_t operator;
			expr_t *left;
			expr_t *right;
		} binary;
		struct
		{
			token_t value;
		} literal;
		struct
		{
			token_t name;
		} variable;
		struct 
		{
			token_t name;
			expr_t *value;
		} assignment;
	};
};

typedef enum
{
	STMT_NONE,
	STMT_IF,
	STMT_VAR,
	STMT_EXPR,
	STMT_BLOCK,
	STMT_WHILE,
} stmtType_t;
struct stmt_s
{
	stmtType_t type;
	union
	{
		struct
		{
			expr_t *expr;
		} expression;
		struct
		{
			token_t name;
			token_t type;
			expr_t *initializer;
		} var;
		struct 
		{
			stmtList_t statements;
		} block;
		struct
		{
			expr_t *condition;
			stmt_t *thenBranch;
			stmt_t *elseBranch;
		} conditional;
		struct
		{
			expr_t *condition;
			stmt_t *body;
		} whileLoop;
	};
};

/* Recursive descent parser */
typedef struct
{
	u32 current;
	const char *code;
	const arrayOf(token_t) *tokens;

	linAlloc_t alloc;

	stmtList_t statements;
} parser_t;

typedef enum
{
	PARSER_NO_ERROR,
	PARSER_ERROR,
} parserError_t;

parserError_t parse(parser_t *parser, const char *code, const arrayOf(token_t) *tokens);
void freeParser(parser_t *parser);

#endif