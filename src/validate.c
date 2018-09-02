#include "validate.h"

static void error(const token_t *token, const char *msg)
{
	printf("ERROR [%d:%d] :: %s\n", token->line, token->line_offset, msg);
};

static bool validateStmt(stmt_t *stmt)
{
	switch (stmt->type)
	{
		case STMT_FUNCTION:
		{
			if(!validateStmt(stmt->function.body))
				return false;
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
			stmtList_t *stmts = &stmt->block.statements;
			for (u32 i = 0; i < stmts->count; i++)
			{
				if(!validateStmt(stmts->data[i]))
					return false;
			};
		}
		default: break;
	};
	return true;
}
bool validate(stmtList_t *statements)
{
	for (u32 i = 0; i < statements->count; i++)
	{
		if(!validateStmt(statements->data[i]))
			return false;
	};
	return true;
};