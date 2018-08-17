#include "expr.h"

expr_t* allocExpression()
{
	expr_t *expr = (expr_t*) malloc(sizeof(expr_t));
	assert (expr);
	zeroMemory(expr, sizeof(expr_t));
	return expr;
};
void freeExpr(expr_t *expr)
{
	if (expr)
	{
		switch (expr->type)
		{
			case EXPR_CAST:
			{
				freeExpr(expr->cast.type);
				freeExpr(expr->cast.expression);
			} break;
			case EXPR_GROUP:
			{
				freeExpr(expr->group.expression);
			} break;
			case EXPR_CALL:
			{
				freeExpr(expr->call.callee);
				freeExprList(&expr->call.args);
			} break;
			case EXPR_PRE_UNARY:
			{
				freeExpr(expr->pre_unary.right);
			} break;
			case EXPR_POST_UNARY:
			{
				freeExpr(expr->post_unary.left);
			} break;
			case EXPR_BINARY:
			{
				freeExpr(expr->binary.left);
				freeExpr(expr->binary.right);
			} break;
			case EXPR_ASSIGNMENT:
			{
				freeExpr(expr->assignment.value);
			} break;
			case EXPR_PTR:
			{
				freeExpr(expr->ptr.to);
			} break;

			case EXPR_LITERAL:
			case EXPR_BUILTIN:
			case EXPR_VARIABLE:
			case EXPR_NONE: break;
		};
		free(expr);
	}
};

static void indent(u32 index)
{
	for (u32 i = 0; i < index; i++)
		printf("\t");
};	
static void printExprList(exprList_t *list, u32 index)
{
	if (list && list->count)
	{
		for (u32 i = 0; i < list->count; i++)
		{
			printExpr(list->data[i], index);
		};
	}
};
static void printBuiltin(const token_t *token)
{
	switch(token->type)
	{
		case TOKEN_F32: printf("f32"); break;
		case TOKEN_I8:  printf("i8"); break;
		case TOKEN_I16: printf("i16"); break;
		case TOKEN_I32: printf("i32"); break;
		case TOKEN_I64: printf("i64"); break;
		case TOKEN_U8:  printf("u8"); break;
		case TOKEN_U16: printf("u16"); break;
		case TOKEN_U32: printf("u32"); break;
		case TOKEN_U64: printf("u64"); break;
		case TOKEN_BOOL: printf("bool"); break;
		default: break;
	};
};
void printExpr(expr_t *expr, u32 index)
{
	indent(index);
	switch(expr->type)
	{
		case EXPR_CALL:
		{
			printf("EXPR_CALL\n");
			printExpr(expr->call.callee, index+1);
			printExprList(&expr->call.args, index+1);
		} break;
		case EXPR_GROUP:
		{
			printf("EXPR_GROUP\n");
			printExpr(expr->group.expression, index+1);
		} break;
		case EXPR_PRE_UNARY:
		{
			printf("EXPR_PRE_UNARY\n");
		} break;
		case EXPR_POST_UNARY:
		{
			printf("EXPR_POST_UNARY\n");

		} break;
		case EXPR_BINARY:
		{
			printf("EXPR_BINARY\n");

		} break;
		case EXPR_LITERAL:
		{
			printf("EXPR_LITERAL\n");

		} break;
		case EXPR_VARIABLE:
		{
			printf("EXPR_VARIABLE\n");

		} break;
		case EXPR_ASSIGNMENT:
		{
			printf("EXPR_ASSIGNMENT\n");

		} break;
		case EXPR_PTR:
		{
			printf("EXPR_PTR\n");

		} break;
		case EXPR_BUILTIN:
		{
			printf("EXPR_BUILTIN: ");
			printBuiltin(&expr->builtin.value);
			printf("\n");
		} break;

		default:
		case EXPR_NONE: printf("Unknown\n"); break;
	}
}

void pushExpr(exprList_t *expressions, expr_t *expr)
{
	if (!expressions->size)
	{
		expressions->count = 0;
		expressions->size = 16;
		expressions->data = malloc(expressions->size*sizeof(expr_t*));
		assert (expressions->data);
	} else if ((expressions->count + 1) >= expressions->size) {
		expressions->size <<= 1;
		expressions->data = realloc(expressions->data, expressions->size*sizeof(expr_t*));
		assert (expressions->data);
	};
	const u32 index = expressions->count ++;
	expressions->data[index] = expr;
};
void freeExprList(exprList_t *expressions)
{
	if (expressions->data)
	{
		// Recursively free all inner expressions
		for (u32 i = 0; i < expressions->count; i++)
		{
			expr_t *expr = expressions->data[i];
			freeExpr(expr);
		};
		// Free the actual array
		free(expressions->data);
		// Zero the structure, just in case
		zeroMemory(expressions, sizeof(exprList_t));
	};
};

// TODO: Make this a "soft" equation? Allow some differing types but throw a warning?
bool typeExpressionsMatch(expr_t *a, expr_t *b)
{
	// TODO: Implement
	return true;
}

static expr_t* evaluateTypeOfLiteral(const expr_t *expr)
{
	// Get the literal token and it's token type
	const token_t literal = expr->literal.value;
	const tokenType_t literalType = literal.type;
	if (literalType == TOKEN_STRING)
	{
		// TODO: Return a ptr<var char> type
		printf("String type not implemented\n");
		return NULL;
	}
	if (literalType == TOKEN_NIL)
	{
		// TODO: Return a ptr<var void> type
		printf("Nil type not implemented\n");
		return NULL;
	}
	
	// Token type -> builtin type
	tokenType_t type;
	switch (expr->literal.value.type)
	{
		case TOKEN_TRUE:
		case TOKEN_FALSE:
		{
			type = TOKEN_BOOL;
		} break;
		case TOKEN_INTEGER:
		{
			type = TOKEN_I32;
		} break;
		case TOKEN_FLOAT:
		{
			type = TOKEN_F32;
		} break;
		default:
		{
			printf("Primary type not implemented\n");
		} return NULL;
	};

	token_t value = {};
	value.type = type;

	expr_t *typeExpr = allocExpression();
	typeExpr->type = EXPR_BUILTIN;
	typeExpr->builtin.flags = (TYPE_FLAG_CONST | TYPE_FLAG_BASIC);
	typeExpr->builtin.value = value;
	return typeExpr;
};
expr_t* evaluateExprType(expr_t *expr)
{
	switch(expr->type)
	{
		case EXPR_GROUP:
			// Return the type of the inner expression
			return evaluateExprType(expr->group.expression);;
		case EXPR_LITERAL:
			return evaluateTypeOfLiteral(expr);

		case EXPR_CAST:
			// Just return the cast type
			return expr->cast.type;

		case EXPR_BINARY:
		{
			// Get the type expressions for the left and right parts of the expression
			expr_t *leftType = evaluateExprType(expr->binary.left);
			expr_t *rightType = evaluateExprType(expr->binary.right);
			// Check that they match
			if (!typeExpressionsMatch(leftType, rightType))
			{
				printf("Sides of binary expression do not match\n");
				return NULL;
			};
			// Return either or, they're the same so it doesn't matter
			return leftType;
		} break;

		case EXPR_PRE_UNARY:
		case EXPR_POST_UNARY:
		{
			printf("Pre- and Post-Unary types not implemented\n");
		} break;

		default:
		{
			printf("Couldn't evaluate type: type %d\n", expr->type);
		};
	};
	return NULL;
};