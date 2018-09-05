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

static void unwindDefered(deferStack_t *defer, itr_t *itr)
{
	for (u32 i = defer->used; i > 0; i--)
	{
		expr_t *expr = defer->expressions[i-1];

		stmt_t *stmt = allocStmt();
		stmt->type = STMT_EXPR;
		stmt->expression.expr = expr;
		insertStmtAt(itr->stmts, stmt, itr->next-1);
		itr->next ++;
	};
}

static void error(const token_t *token, const char *msg)
{
	printf("ERROR [%d:%d] :: %s\n", token->line, token->line_offset, msg);
};
static bool validateStmt(itr_t *itr, stmt_t *stmt, deferStack_t *defer, scopeStack_t *scope)
{
	switch (stmt->type)
	{
		case STMT_FUNCTION:
		{
			argList_t *args = &stmt->function.arguments;
			stmt_t *body = stmt->function.body;

			// Push the scope block
			pushScopeBlock(scope);
			{
				// Push the arguments into the scope
				for (u32 i = 0; i < args->count; i++)
				{
					const varDecl_t *decl = args->data + i;
					insertVar(scope, decl->name, decl->type);
				}
				// Validate the body statements
				// NOTE: This must be done seperately so that we still have access to the new iterator
				itr_t new_itr = {};
				new_itr.stmts = &body->block.statements;
				while (true)
				{
					stmt_t *stmt = next(&new_itr);
					if (!stmt)
						break;
					if (!validateStmt(&new_itr, stmt, defer, scope))
						return false;
				};
				// If the last line is not a return
				stmt_t *last = new_itr.stmts->data[new_itr.next-1];
				if (last->type != STMT_RETURN)
				{
					// Unwind the defer stack
					new_itr.next ++;
					unwindDefered(defer, &new_itr);
				}
			}
			// Reset the defer stack
			resetDeferStack(defer);
			// Pop the scope stack
			popScopeBlock(scope);
		} break;
		case STMT_IF:
		{
			// Validate both branches
			stmt_t *thenBranch = stmt->conditional.thenBranch;
			stmt_t *elseBranch = stmt->conditional.elseBranch;
			validateStmt(itr, thenBranch, defer, scope);
			if (elseBranch)
			{
				validateStmt(itr, elseBranch, defer, scope);
			}
		} break;
		case STMT_WHILE:
		{
			stmt_t *body = stmt->whileLoop.body;
			validateStmt(itr, body, defer, scope);
		} break;
		case STMT_DEFER:
		{
			// Push the expression to the defered stack
			expr_t *expr = stmt->defer.expression;
			pushDeferedExpr(defer, expr);
			
			removeStmt(itr->stmts, itr->next-1);
			itr->next --;
		} break;
		case STMT_RETURN:
		{
			stmt_t *stmt = removeStmt(itr->stmts, itr->next-1);

			unwindDefered(defer, itr);

			insertStmtAt(itr->stmts, stmt, itr->next-1);
		} break;
		case STMT_VAR:
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
			insertVar(scope, decl->name, decl->type);
		} break;
		case STMT_BLOCK:
		{
			pushScopeBlock(scope);

			itr_t new_itr = {};
			new_itr.stmts = &stmt->block.statements;

			while (true)
			{
				stmt_t *stmt = next(&new_itr);
				if (!stmt)
					break;
				if (!validateStmt(&new_itr, stmt, defer, scope))
					return false;
			};
			popScopeBlock(scope);
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

	scopeStack_t scope = {};
	initScopeStack(&scope);

	bool error = false;
	while (!error)
	{
		stmt_t *stmt = next(&itr);
		if (!stmt)
			break;
		if (!validateStmt(&itr, stmt, &defer, &scope))
			error = true;
	};
	freeScopeStack(&scope);
	freeDeferStack(&defer);
	return !error;
};