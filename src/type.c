#include "type.h"

// TODO: Make this a "soft" equation? Allow some differing types but throw a warning?
bool typeExpressionsMatch(expr_t *a, expr_t *b)
{
	// TODO: Implement
	return true;
}

static tokenType_t tokenToBuiltin(const tokenType_t type)
{
	switch (type)
	{
		// Booleans default to bool
		case TOKEN_TRUE:
		case TOKEN_FALSE:
			return TOKEN_BOOL;
		// Integers to int32_t
		case TOKEN_INTEGER:
			return TOKEN_I32;
		// Floats to float
		case TOKEN_FLOAT:
			return TOKEN_F32;
		// Everything else not implemented (or not valid)
		default: break;
	};
	return TOKEN_EOF;
};
static expr_t* evaluateTypeOfLiteral(const expr_t *expr, const scopeStack_t *scope, typeFlags_t flags)
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
	tokenType_t type = tokenToBuiltin(expr->literal.value.type);
	if (type == TOKEN_EOF)
	{
		printf("Primary type not implemented\n");
		return NULL;
	}

	token_t value = {};
	value.type = type;

	expr_t *typeExpr = allocExpr();
	typeExpr->type = EXPR_BUILTIN;
	typeExpr->builtin.flags = (flags | TYPE_FLAG_BASIC);
	typeExpr->builtin.value = value;
	return typeExpr;
};
static expr_t* evaluateVariableType(expr_t *expr, const scopeStack_t *scope, typeFlags_t flags)
{
	assert (expr);
	// Return the type of the identifier
	// TODO: Checking
	const token_t name = expr->variable.name;
	expr_t *type = getVarType(scope, name);
	if (!type)
	{
		printf("ERROR [%d:%d] :: Identifier \"%.*s\" not found\n", 
			name.line, name.line_offset,
			name.len, name.start);
	}
	return type;
};
static expr_t* evaluateBinaryExprType(expr_t *expr, const scopeStack_t *scope, typeFlags_t flags)
{
	// Get the type expressions for the left and right parts of the expression
	expr_t *leftType = evaluateExprType(expr->binary.left, scope, flags);
	expr_t *rightType = evaluateExprType(expr->binary.right, scope, flags);
	// Check that they match
	if (!typeExpressionsMatch(leftType, rightType))
	{
		printf("Sides of binary expression do not match\n");
		return NULL;
	};
	// Return either or, they're the same so it doesn't matter
	return leftType;
};
static expr_t* evaluatePreUnaryExpr(expr_t *expr, const scopeStack_t *scope, typeFlags_t flags)
{
	const tokenType_t operator = expr->pre_unary.operator.type;
	expr_t *right = expr->pre_unary.right;

	switch (operator)
	{
		// Reference operator
		case TOKEN_AND:
		{
			expr_t *to = evaluateExprType(right, scope, flags);
			if (!to)
			{
				return NULL;
			}

			expr_t *typeExpr = allocExpr();
			typeExpr->type = EXPR_PTR;
			typeExpr->ptr.flags = 0;
			typeExpr->ptr.to = to;
			return typeExpr;
		} break;
		default:
		{
			printf("Pre unary type not implemented\n");
		} break;
	};
	return NULL;
};
expr_t* evaluateExprType(expr_t *expr, const scopeStack_t *scope, typeFlags_t flags)
{
	switch(expr->type)
	{
		case EXPR_GROUP:
			// Return the type of the inner expression
			return evaluateExprType(expr->group.expression, scope, flags);
		case EXPR_VARIABLE:
			return evaluateVariableType(expr, scope, flags);
		case EXPR_LITERAL:
			return evaluateTypeOfLiteral(expr, scope, flags);
		case EXPR_CAST:
			// Just return the cast type
			// TODO: Make sure to handle const flags
			return expr->cast.type;
		case EXPR_BINARY:
			return evaluateBinaryExprType(expr, scope, flags);
		case EXPR_PRE_UNARY:
			return evaluatePreUnaryExpr(expr, scope, flags);
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