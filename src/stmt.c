#include "stmt.h"

stmt_t* allocStatement()
{
	stmt_t *stmt = (stmt_t*) malloc(sizeof(stmt_t));
	assert (stmt);
	zeroMemory(stmt, sizeof(stmt_t));
	return stmt;
};
void freeStmt(stmt_t *stmt)
{
	if (stmt)
	{
		switch (stmt->type)
		{
			case STMT_BLOCK:
			{
				freeStmtList(&stmt->block.statements);
			} break;
			case STMT_EXPR:
			{
				freeExpr(stmt->expression.expr);
			} break;
			case STMT_VAR:
			{
				freeExpr(stmt->var.initializer);
			} break;
			case STMT_IF:
			{
				freeExpr(stmt->conditional.condition);
				freeStmt(stmt->conditional.thenBranch);
				freeStmt(stmt->conditional.elseBranch);
			} break;
			case STMT_WHILE:
			{
				freeExpr(stmt->whileLoop.condition);
				freeStmt(stmt->whileLoop.body);
			} break;
			case STMT_RETURN:
			{
				freeExpr(stmt->ret.value);
			} break;
			case STMT_FUNCTION:
			{
				freeStmt(stmt->function.body);
				freeArgList(&stmt->function.arguments);
			} break;

			case STMT_NONE: break;
		};
		free(stmt);
	} 
};

void pushStmt(stmtList_t *statements, stmt_t *stmt)
{
	if (!statements->size)
	{
		statements->count = 0;
		statements->size = 16;
		statements->data = malloc(statements->size*sizeof(stmt_t*));
		assert (statements->data);
	} else if ((statements->count + 1) >= statements->size) {
		statements->size <<= 1;
		statements->data = realloc(statements->data, statements->size*sizeof(stmt_t*));
		assert (statements->data);
	};
	const u32 index = statements->count ++;
	statements->data[index] = stmt;
};
void freeStmtList(stmtList_t *statements)
{
	if (statements->data)
	{
		// Recursively free all inner statements and expressions
		for (u32 i = 0; i < statements->count; i++)
		{
			stmt_t *stmt = statements->data[i];
			freeStmt(stmt);
			
		};
		// Free the actual array
		free(statements->data);
		// Zero the structure, just in case
		zeroMemory(statements, sizeof(stmtList_t));
	};
};

varDecl_t* pushVarDecl(argList_t *arguments)
{
	if (!arguments->size)
	{
		arguments->count = 0;
		arguments->size = 16;
		arguments->data = malloc(arguments->size*sizeof(varDecl_t));
		assert (arguments->data);
	} else if ((arguments->count + 1) >= arguments->size) {
		arguments->size <<= 1;
		arguments->data = realloc(arguments->data, arguments->size*sizeof(varDecl_t));
		assert (arguments->data);
	};
	const u32 index = arguments->count ++;
	return arguments->data + index;
};
void freeArgList(argList_t *arguments)
{
	if (arguments->data)
	{
		for (u32 i = 0; i < arguments->count; i++)
		{
			varDecl_t *decl = (arguments->data + i);
			// Free the type expression
			freeExpr(decl->type);
		};
		// Free the data array itself
		free(arguments->data);
	};
};