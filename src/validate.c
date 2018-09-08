#include "validate.h"

typedef struct
{
	stmt_t *current;
	stmtList_t *stmts;
} itr_t;

static void unwindDefered(deferStack_t *defer, itr_t *itr)
{
	for (u32 i = defer->used; i > 0; i--)
	{
		expr_t *expr = defer->expressions[i-1];

		stmt_t *stmt = allocStmt();
		stmt->type = STMT_EXPR;
		stmt->expression.expr = expr;
		insertStmtAfter(itr->stmts, itr->current, stmt);
		itr->current = stmt;
	};
}

static void error(const token_t *token, const char *msg)
{
	printf("ERROR [%d:%d] :: %s\n", token->line, token->line_offset, msg);
};

static bool validateStmt(itr_t *itr, stmt_t *stmt, deferStack_t *defer, scopeStack_t *scope);
static bool validateFunctionStmt(itr_t *itr, stmt_t *stmt, deferStack_t *defer, scopeStack_t *scope)
{
	argList_t *args = &stmt->function.arguments;
	stmt_t *body = stmt->function.body;
	assert (body->type == STMT_BLOCK);

	// Push the scope block
	pushScopeBlock(scope);
	{
		// Push the arguments into the scope
		for (u32 i = 0; i < args->count; i++)
		{
			const varDecl_t *decl = args->data + i;
			insertVar(scope, decl);
		}
		// Validate the body statements
		// NOTE: This must be done seperately so that we still have access to the new iterator
		stmtList_t *stmts = &body->block.statements;

		itr_t new_itr = {};
		new_itr.stmts = stmts;
		new_itr.current = stmts->head;

		bool error = false;
		while (!error && new_itr.current)
		{
			assert (new_itr.current);
			if (!validateStmt(&new_itr, new_itr.current, defer, scope))
			{
				error = true;
				break;
			}
			if (new_itr.current->next == new_itr.stmts->head)
				break;
			new_itr.current = new_itr.current->next;
		};
		// If the last line is not a return
		new_itr.current = new_itr.stmts->head->prev;
		if (new_itr.current)
		{
			new_itr.current = new_itr.stmts->head;
		}
		if (new_itr.current && new_itr.current->type != STMT_RETURN)
		{
			// Unwind the defer stack
			unwindDefered(defer, &new_itr);
		}
	}
	// Reset the defer stack
	resetDeferStack(defer);
	// Pop the scope stack
	popScopeBlock(scope);
	return true;
};
static bool validateIfStmt(itr_t *itr, stmt_t *stmt, deferStack_t *defer, scopeStack_t *scope)
{
	// Validate both branches
	stmt_t *thenBranch = stmt->conditional.thenBranch;
	stmt_t *elseBranch = stmt->conditional.elseBranch;
	const bool thenOkay = validateStmt(itr, thenBranch, defer, scope);
	const bool elseOkay = elseBranch ? validateStmt(itr, elseBranch, defer, scope) : true;
	return (thenOkay && elseOkay);
};
static bool validateWhileStmt(itr_t *itr, stmt_t *stmt, deferStack_t *defer, scopeStack_t *scope)
{
	stmt_t *body = stmt->whileLoop.body;
	return validateStmt(itr, body, defer, scope);
};
static bool validateDeferStmt(itr_t *itr, stmt_t *stmt, deferStack_t *defer, scopeStack_t *scope)
{
	#if 0
	// Push the expression to the defered stack
	expr_t *expr = stmt->defer.expression;
	pushDeferedExpr(defer, expr);
	
	stmt_t *next = stmt->next;
	itr->current = next;

	removeStmt(itr->stmts, stmt);
	#endif
	return true;
} 
static bool validateReturnStmt(itr_t *itr, stmt_t *stmt, deferStack_t *defer, scopeStack_t *scope)
{
	if (defer->used != 0)
	{
		unwindDefered(defer, itr);
	}
	return true;
} 
static bool validateVariableStmt(itr_t *itr, stmt_t *stmt, deferStack_t *defer, scopeStack_t *scope)
{
	varDecl_t *decl = &stmt->var.decl;
	expr_t *initializer = stmt->var.initializer;
	// If there's no type, then we need to inference the variable 
	if (!decl->type)
	{
		// Get the type of the initializer expression
		expr_t *type = evaluateExprType(initializer, scope, decl->flags);
		if (!type)
		{
			error(&decl->name, "Couldn't determine type for type-inferenced variable");
			return false;
		}
		#if 0
		printf("Type for: %.*s\n", decl->name.len, decl->name.start);
		printExpr(type, 0);
		#endif
		// Set the type
		// NOTE: We clone it so on free, this is not freed before or after it is necessary to
		decl->type = cloneExpr(type);
	} else {
		// TODO: Check if types match for initializer
	}
	// Check if the variable already exists
	if (getVarType(scope, decl->name))
	{
		printf("ERROR [%d:%d] :: Variable \"%.*s\" already in scope\n", 
			decl->name.line, decl->name.line_offset,
			decl->name.len, decl->name.start);
		return false;
	}
	// Insert the variable into the scope
	insertVar(scope, decl);
	return true;
};
static bool validateBlockStmt(itr_t *itr, stmt_t *stmt, deferStack_t *defer, scopeStack_t *scope)
{
	pushScopeBlock(scope);
	{
		stmtList_t *stmts = &stmt->block.statements;

		itr_t new_itr = {};
		new_itr.stmts = stmts;
		new_itr.current = stmts->head;

		bool error = false;
		while (!error && new_itr.current && new_itr.current)
		{
			assert (new_itr.current);
			if (!validateStmt(&new_itr, new_itr.current, defer, scope))
			{
				error = true;
				break;
			}
			if (new_itr.current->next == new_itr.stmts->head)
				break;
			new_itr.current = new_itr.current->next;
		};
	}
	popScopeBlock(scope);
	return true;
};

static bool validateStmt(itr_t *itr, stmt_t *stmt, deferStack_t *defer, scopeStack_t *scope)
{
	assert (stmt);
	switch (stmt->type)
	{
		case STMT_FUNCTION: return validateFunctionStmt(itr, stmt, defer, scope);
		case STMT_IF:		return validateIfStmt(itr, stmt, defer, scope);
		case STMT_WHILE:	return validateWhileStmt(itr, stmt, defer, scope);
		case STMT_DEFER:	return validateDeferStmt(itr, stmt, defer, scope);
		case STMT_RETURN: 	return validateReturnStmt(itr, stmt, defer, scope);
		case STMT_VAR:		return validateVariableStmt(itr, stmt, defer, scope);
		case STMT_BLOCK:	return validateBlockStmt(itr, stmt, defer, scope);
	
		case STMT_NONE:
		case STMT_EXPR:
			return true;
	};
	return false;
}
bool validate(stmtList_t *statements)
{
	deferStack_t defer = {};
	initDeferStack(&defer);

	scopeStack_t scope = {};
	initScopeStack(&scope);

	itr_t itr = {};
	itr.stmts = statements;
	itr.current = statements->head;

	bool error = false;
	while (!error && itr.current)
	{
		assert (itr.current);
		if (!validateStmt(&itr, itr.current, &defer, &scope))
		{
			error = true;
			break;
		}
		if (itr.current->next == itr.stmts->head)
			break;
		itr.current = itr.current->next;
	};
	freeScopeStack(&scope);
	freeDeferStack(&defer);
	return !error;
};