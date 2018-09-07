#ifndef STMT_H
#define STMT_H

#include "core.h"

#include "expr.h"

struct stmt_s;
typedef struct stmt_s stmt_t;

typedef struct
{
	size_t count;
	size_t size;
	stmt_t **data;
} stmtList_t;

/* List of arguments */
typedef struct
{
	size_t count;
	size_t size;
	varDecl_t *data;
} argList_t;

typedef enum
{
	STMT_NONE,
	// Statements
	STMT_IF,
	STMT_EXPR,
	STMT_BLOCK,
	STMT_WHILE,
	STMT_RETURN,
	STMT_DEFER,
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
			expr_t *expression;
		} defer;
		struct
		{
			varDecl_t decl;
			argList_t arguments;
			stmt_t *body;
		} function;
	};
};

stmt_t* allocStmt();
void freeStmt(stmt_t *stmt);


void pushStmt(stmtList_t *statements, stmt_t *stmt);
stmt_t* removeStmt(stmtList_t *statements, u32 index);
void insertStmtAt(stmtList_t *statements, stmt_t *stmt, u32 index);
void freeStmtList(stmtList_t *statements);

varDecl_t* pushVarDecl(argList_t *arguments);
void freeArgList(argList_t *arguments);


#endif