#include "stmt.h"

stmt_t* allocStmt()
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
			case STMT_DEFER:
			{
				freeExpr(stmt->defer.expression);
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
	assert (statements);
	stmt->next = stmt->prev = NULL;
	if (!statements->head)
	{
		statements->head = stmt;	
	} else {
		stmt_t *tail = statements->head->prev;
		if (tail)
		{
			tail->next = stmt;
			stmt->prev = tail;

			statements->head->prev = stmt;
			stmt->next = statements->head;
		} else {
			stmt->prev = statements->head;
			stmt->next = statements->head;

			statements->head->next = stmt;
			statements->head->prev = stmt;
		}
	}
};
void freeStmtList(stmtList_t *statements)
{
	stmt_t *stmt = statements->head;
	while (stmt)
	{
		stmt_t *next = stmt->next;
		freeStmt(stmt);
		if (next == statements->head)
			break;
		stmt = next;
	};
};
void removeStmt(stmtList_t *statements, stmt_t *stmt)
{
	stmt_t *next = stmt->next;
	stmt_t *prev = stmt->prev;

	if (prev) prev->next = next;
	if (next) next->prev = prev;

	if (stmt == statements->head)
		statements->head = NULL;
};
void insertStmtAfter(stmtList_t *statements, stmt_t *after, stmt_t *new_stmt)
{
	stmt_t *next = after->next;
	next->prev = new_stmt;
	new_stmt->next = next;

	after->next = new_stmt;
	new_stmt->prev = after;
};
void insertStmtBefore(stmtList_t *statements, stmt_t *before, stmt_t *new_stmt)
{
	stmt_t *prev = before->prev;
	prev->next = new_stmt;
	new_stmt->prev = prev;

	before->prev = new_stmt;
	new_stmt->next = before;

	if (before == statements->head)
	{
		statements->head = new_stmt;
	}
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