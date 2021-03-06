#ifndef EXPR_H
#define EXPR_H

#include "core.h"

#include "lexer.h"

struct expr_s; 
typedef struct expr_s expr_t;

typedef struct
{
	size_t count;
	size_t size;
	expr_t **data;
} exprList_t;

typedef enum
{
	TYPE_FLAG_NONE		= 0,
	TYPE_FLAG_CONST		= (1 << 0),
	TYPE_FLAG_BASIC		= (1 << 1),
	TYPE_FLAG_RETURN	= (1 << 2),
} typeFlags_t;
typedef struct
{
	token_t name;
	expr_t *type;
	typeFlags_t flags;
} varDecl_t;

typedef enum
{
	EXPR_NONE,
	// Expressions
	EXPR_CALL,
	EXPR_CAST,
	EXPR_GROUP,
	EXPR_PRE_UNARY,
	EXPR_POST_UNARY,
	EXPR_BINARY,
	EXPR_LITERAL,
	EXPR_VARIABLE,
	// Assignment
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
			expr_t *type;
			expr_t *expression;
		} cast;
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

		// u[8-64], i[8-64], f[32|64], bool char
		struct
		{
			typeFlags_t flags;
			token_t value;
		} builtin;
		// ptr<type>
		struct
		{
			typeFlags_t flags;
			expr_t *to;
		} ptr;
		// fn<args, return>
		struct 
		{
			exprList_t args;
			expr_t *ret;
		} fn;
	};
};

// Expression memory management functions
expr_t* allocExpr();
void freeExpr(expr_t *expr);

expr_t* cloneExpr(const expr_t *old);

#if DEBUG
// Debug prints the expression
// NOTE: Index is the starting indentation for the print
void printExpr(expr_t *expr, u32 index);
#endif

// Expression list functions
void pushExpr(exprList_t *expressions, expr_t *expr);
void freeExprList(exprList_t *expressions);

#endif