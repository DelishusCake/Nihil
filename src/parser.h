#ifndef PARSER_H
#define PARSER_H

#include "core.h"
#include "util.h"

#include "lexer.h"

// Maximum number of arguments that can be passed to a single function call 
#define MAX_ARGUMENTS		64

// Pre-declare types 
struct expr_s; 
struct stmt_s;

typedef struct expr_s expr_t;
typedef struct stmt_s stmt_t;

/* List of statements
 * NOTE: Both statements and expressions are allocated internally, and should not be accessd 
 * after a call to freeParser
 */
typedef struct
{
	size_t count;
	size_t size;
	stmt_t **data;
} stmtList_t;

/* List of expressions */
typedef struct
{
	size_t count;
	size_t size;
	expr_t **data;
} exprList_t;

/* List of arguments */
typedef struct
{
	token_t name;
	expr_t *type;
} varDecl_t;
typedef struct
{
	size_t count;
	size_t size;
	varDecl_t *data;
} argList_t;

typedef struct
{
	u8 isConst : 1;
} typeFlags_t;

/* Abstract Syntax Tree structures */
typedef enum
{
	EXPR_NONE,
	// Expressions
	EXPR_CALL,
	EXPR_GROUP,
	EXPR_UNARY,
	EXPR_BINARY,
	EXPR_LITERAL,
	EXPR_VARIABLE,
	EXPR_ASSIGNMENT,
	// Type expressions
	EXPR_BUILTIN,
} exprType_t;
struct expr_s
{
	exprType_t type;
	union
	{
		struct
		{
			expr_t *callee;
			exprList_t args;
		} call;
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

		struct
		{
			token_t value;
			typeFlags_t flags;
		} builtin;
	};
};

typedef enum
{
	STMT_NONE,
	// Statements
	STMT_IF,
	STMT_EXPR,
	STMT_BLOCK,
	STMT_WHILE,
	STMT_RETURN,
	// Declarations
	STMT_VAR,
	STMT_FUNCTION,
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
			varDecl_t decl;
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
		struct 
		{
			expr_t *value;
		} ret;
		struct
		{
			varDecl_t decl;
			argList_t arguments;
			stmt_t *body;
		} function;
	};
};

/* Recursive descent parser */
typedef enum
{
	PARSER_NO_ERROR,
	PARSER_ERROR,
} parserError_t;
typedef struct
{
	u32 current;
	const char *code;
	const arrayOf(token_t) *tokens;

	parserError_t error;
	stmtList_t statements;
} parser_t;

parserError_t parse(parser_t *parser, const char *code, const arrayOf(token_t) *tokens);
void freeParser(parser_t *parser);

#endif