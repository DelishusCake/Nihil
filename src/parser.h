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

typedef enum
{
	TYPE_FLAG_NONE		= 0,
	TYPE_FLAG_CONST		= (1 << 0),
	TYPE_FLAG_BASIC		= (1 << 1),
	TYPE_FLAG_RETURN	= (1 << 2),
} typeFlags_t;

/* Abstract Syntax Tree structures */
typedef enum
{
	EXPR_NONE,
	// Expressions
	EXPR_CALL,
	EXPR_GROUP,
	EXPR_PRE_UNARY,
	EXPR_POST_UNARY,
	EXPR_BINARY,
	EXPR_LITERAL,
	EXPR_VARIABLE,
	EXPR_ASSIGNMENT,
	// Type expressions
	EXPR_PTR,
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
		} pre_unary;
		struct
		{
			token_t operator;
			expr_t *left;
		} post_unary;
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
			token_t operator;
			expr_t *target;
			expr_t *value;
		} assignment;

		struct
		{
			typeFlags_t flags;
			token_t value;
		} builtin;
		struct
		{
			typeFlags_t flags;
			expr_t *to;
		} ptr;
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