#include "output_c.h"

static void output_expression(const expr_t *expr)
{
	switch(expr->type)
	{
		case EXPR_GROUP:
		{
			printf("(");
			output_expression(expr->group.expression);
			printf(")");
		} break;
		case EXPR_UNARY:
		{
			const token_t *operator = &expr->unary.operator; 
			const expr_t *right = expr->unary.right; 

			printf("%.*s", operator->len, operator->start);
			output_expression(right);
		} break;
		case EXPR_BINARY:
		{
			const token_t *operator = &expr->binary.operator; 
			const expr_t *left = expr->binary.left; 
			const expr_t *right = expr->binary.right; 

			output_expression(left);
			printf("%.*s", operator->len, operator->start);
			output_expression(right);
		} break;
		case EXPR_LITERAL:
		{
			const token_t *value = &expr->literal.value; 
			printf("%.*s", value->len, value->start);
		} break;

		default: printf("ERROR NOT IMPLEMENTED\n"); break;
	};
};
static void output_statement(const stmt_t *stmt)
{
	switch(stmt->type)
	{
		case STMT_NONE: break;
		case STMT_VAR:
		{
			printf("%.*s %.*s", 
				stmt->var.decl.type.len, stmt->var.decl.type.start, 
				stmt->var.decl.name.len, stmt->var.decl.name.start);

			if (stmt->var.initializer)
			{
				printf(" = ");
				output_expression(stmt->var.initializer);
			}
		} break;

		default: printf("ERROR NOT IMPLEMENTED\n"); break;
	};
	printf(";\n");
};

void output_c(const parser_t *parser)
{
	const stmtList_t *statements = &parser->statements; 
	for (u32 i = 0; i < statements->count; i++)
	{
		const stmt_t *stmt = statements->data[i];
		output_statement(stmt);
	};
};