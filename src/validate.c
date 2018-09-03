#include "validate.h"

// Statement iterator, mainly for statement removal while traversing the list
typedef struct
{
	u32 next;
	stmtList_t *stmts;
} itr_t;

static stmt_t* next(itr_t *itr)
{
	stmt_t *stmt = NULL;
	if ((itr->next+1) <= itr->stmts->count)
	{
		stmt = itr->stmts->data[itr->next++];
	}
	return stmt;
};
static stmt_t* current(itr_t *itr)
{
	return itr->stmts->data[itr->next-1];
};
static stmt_t* removeCurrent(itr_t *itr)
{
	if (itr->next != 0)
	{
		// Remove and get the current statement
		stmt_t *stmt = removeStmt(itr->stmts, itr->next-1);
		// Move the iterator back so we dont skip the next statement
		itr->next --;
		return stmt;
	}
	return NULL;
};
static void insertStmtAtCurrent(itr_t *itr, stmt_t *stmt)
{
	// Insert the statement at the current index
	if (itr->next == 0)
	{
		insertStmtAt(itr->stmts, stmt, itr->next-1);	
	} else {
		pushStmt(itr->stmts, stmt);
	}
	itr->next ++;
};

static void unwindDeferStack(deferStack_t *defer, itr_t *itr)
{
	// Pop off all the expressions and push them as statements
	for (u32 i = defer->used; i > 0; i--)
	{
		expr_t *expr = defer->expressions[i-1];

		stmt_t *stmt = allocStmt();
		stmt->type = STMT_EXPR;
		stmt->expression.expr = expr;
		insertStmtAtCurrent(itr, stmt);
	};
};

static void error(const token_t *token, const char *msg)
{
	printf("ERROR [%d:%d] :: %s\n", token->line, token->line_offset, msg);
};
static bool validateStmt(itr_t *itr, stmt_t *stmt, deferStack_t *defer)
{
	switch (stmt->type)
	{
		case STMT_FUNCTION:
		{
			stmt_t *body = stmt->function.body;

			itr_t new_itr = {};
			new_itr.stmts = &body->block.statements;

			while (true)
			{
				stmt_t *stmt = next(&new_itr);
				if (!stmt)
					break;
				if (!validateStmt(&new_itr, stmt, defer))
					return false;
			};
			stmt_t *last = current(&new_itr);
			if (last->type != STMT_RETURN)
			{
				unwindDeferStack(defer, &new_itr);
			}
			resetDeferStack(defer);
		} break;
		case STMT_IF:
		{
			// Validate both branches
			stmt_t *thenBranch = stmt->conditional.thenBranch;
			stmt_t *elseBranch = stmt->conditional.elseBranch;
			validateStmt(itr, thenBranch, defer);
			if (elseBranch)
			{
				validateStmt(itr, elseBranch, defer);
			}
		} break;
		case STMT_WHILE:
		{
			stmt_t *body = stmt->whileLoop.body;
			validateStmt(itr, body, defer);
		} break;
		case STMT_DEFER:
		{
			// Push the expression to the defered stack
			expr_t *expr = stmt->defer.expression;
			pushDeferedExpr(defer, expr);
			// Set the expression to NULL so it doesn't get freed with the statement
			stmt->defer.expression = NULL;
			// Remove and free the statement
			removeCurrent(itr);
		} break;
		case STMT_RETURN:
		{
			stmt_t *ret = removeCurrent(itr);
			// Push the defered statements, but don't reset the stack
			unwindDeferStack(defer, itr);
			insertStmtAtCurrent(itr, ret);
		} break;
		case STMT_VAR:
		{
			varDecl_t *decl = &stmt->var.decl;
			expr_t *initializer = stmt->var.initializer;
			// If there's no type, then we need to inference the variable 
			if (!decl->type)
			{
				// Get the type of the initializer expression
				expr_t *type = evaluateExprType(initializer, decl->flags);
				if (!type)
				{
					error(&decl->name, "Couldn't determine type for type-inferenced variable");
					return false;
				}
				#if 1
				printf("Type for: %.*s\n", decl->name.len, decl->name.start);
				printExpr(type, 0);
				#endif
				// Set the type
				decl->type = type;
			} else {
				
			}
		} break;
		case STMT_BLOCK:
		{
			itr_t new_itr = {};
			new_itr.stmts = &stmt->block.statements;

			while (true)
			{
				stmt_t *stmt = next(&new_itr);
				if (!stmt)
					break;
				if (!validateStmt(&new_itr, stmt, defer))
					return false;
			};
		}
		case STMT_NONE:
		case STMT_EXPR:
			break;
	};
	return true;
}
bool validate(stmtList_t *statements)
{
	itr_t itr = {};
	itr.stmts = statements;

	deferStack_t defer = {};
	initDeferStack(&defer);

	bool error = false;
	while (!error)
	{
		stmt_t *stmt = next(&itr);
		if (!stmt)
			break;
		if (!validateStmt(&itr, stmt, &defer))
			error = true;
	};
	freeDeferStack(&defer);
	return !error;
};