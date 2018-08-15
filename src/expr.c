#include "expr.h"

static void freeExprList(exprList_t *expressions);

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
static void freeExprList(exprList_t *expressions)
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