#include "output_c.h"

static void indent(u32 count)
{
	for (u32 i = 0; i < count; i++)
		printf("\t");
}
static void output_token(const token_t *token)
{
	char *str = NULL;
	switch (token->type)
	{
		// Single character tokens
		case TOKEN_OPEN_PAREN: 		str = "("; break;
		case TOKEN_CLOSE_PAREN: 	str = ")"; break;
		case TOKEN_OPEN_BRACE: 		str = "{"; break;
		case TOKEN_CLOSE_BRACE: 	str = "}"; break;
		case TOKEN_COMMA: 			str = ","; break;
		case TOKEN_DOT: 			str = "."; break;
		case TOKEN_MINUS: 			str = "-"; break;
		case TOKEN_PLUS: 			str = "+"; break;
		case TOKEN_SEMICOLON: 		str = ";"; break;
		case TOKEN_SLASH: 			str = "/"; break;
		case TOKEN_STAR: 			str = "*"; break;
		// One/two char tokens
		case TOKEN_BANG: 			str = "!"; break;
		case TOKEN_BANG_EQUAL: 		str = "!="; break;
		case TOKEN_EQUAL: 			str = "="; break;
		case TOKEN_EQUAL_EQUAL: 	str = "=="; break;
		case TOKEN_GREATER: 		str = ">"; break;
		case TOKEN_GREATER_EQUAL: 	str = ">="; break;
		case TOKEN_LESS: 			str = "<"; break;
		case TOKEN_LESS_EQUAL: 		str = "<="; break;
		case TOKEN_COLON: 			str = ":"; break;
		case TOKEN_COLON_COLON: 	str = "::"; break;
		case TOKEN_COLON_EQUAL: 	str = ":="; break;
		case TOKEN_PLUS_PLUS: 		str = "++"; break;
		case TOKEN_MINUS_MINUS: 	str = "--"; break;
		case TOKEN_ARROW: 			str = "->"; break;
		case TOKEN_AND: 			str = "&"; break;
		case TOKEN_AND_AND: 		str = "&&"; break;
		case TOKEN_OR: 				str = "|"; break;
		case TOKEN_OR_OR: 			str = "||"; break;
		// Keywords
		case TOKEN_TRUE: 			str = "true"; break;
		case TOKEN_FALSE: 			str = "false"; break;
		case TOKEN_WHILE: 			str = "while"; break;
		case TOKEN_NIL: 			str = "null"; break;
		case TOKEN_RETURN: 			str = "return"; break;
		case TOKEN_STRUCT: 			str = "struct"; break;
		case TOKEN_UNION: 			str = "union"; break;
		case TOKEN_ENUM: 			str = "enum"; break;
		// Built in types
		case TOKEN_U8: 				str = "u8"; break;
		case TOKEN_U16: 			str = "u16"; break;
		case TOKEN_U32: 			str = "u32"; break;
		case TOKEN_U64: 			str = "u64"; break;
		case TOKEN_I8: 				str = "i8"; break;
		case TOKEN_I16: 			str = "i16"; break;
		case TOKEN_I32: 			str = "i32"; break;
		case TOKEN_I64: 			str = "i64"; break;
		case TOKEN_F32: 			str = "f32"; break;
		case TOKEN_F64: 			str = "f64"; break;
		case TOKEN_CHAR: 			str = "char"; break;
		case TOKEN_BOOL: 			str = "bool"; break;
		case TOKEN_VOID: 			str = "void"; break;
		// Unknown (to make the compiler shut up)
		default: str = NULL; break;
	};
	if (str)
	{
		printf(str);
	} else {
		printf("%.*s", token->len, token->start);
	}
};
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

			output_token(operator);
			output_expression(right);
		} break;
		case EXPR_BINARY:
		{
			const token_t *operator = &expr->binary.operator; 
			const expr_t *left = expr->binary.left; 
			const expr_t *right = expr->binary.right; 

			output_expression(left);
			output_token(operator);
			output_expression(right);
		} break;
		case EXPR_LITERAL:
		{
			const token_t *value = &expr->literal.value; 
			output_token(value);
		} break;

		default: printf("ERROR NOT IMPLEMENTED\n"); break;
	};
};
static void output_statement(const stmt_t *stmt, u32 index)
{
	indent(index);
	switch(stmt->type)
	{
		case STMT_NONE: break;
		case STMT_VAR:
		{
			output_token(&stmt->var.decl.type);
			printf(" ");
			output_token(&stmt->var.decl.name);
			if (stmt->var.initializer)
			{
				printf(" = ");
				output_expression(stmt->var.initializer);
			}
			printf(";\n");
		} break;
		case STMT_BLOCK:
		{
			printf("{\n");
			for (u32 i = 0; i < stmt->block.statements.count; ++i)
			{
				output_statement(stmt->block.statements.data[i], index+1);
			}
			indent(index);
			printf("}");
		} break;
		case STMT_IF:
		{
			printf("if (");
			output_expression(stmt->conditional.condition);
			printf(")\n");
			output_statement(stmt->conditional.thenBranch, index);
			printf("\n");
		} break;
		case STMT_RETURN:
		{
			printf("return ");
			output_expression(stmt->ret.value);
			printf(";\n");
		} break;
		case STMT_FUNCTION:
		{
			output_token(&stmt->function.type);
			printf(" ");
			output_token(&stmt->function.name);
			printf("(");

			for (u32 i = 0; i < stmt->function.arguments.count; i++)
			{
				const varDecl_t *var = stmt->function.arguments.data + i;
				output_token(&var->type);
				printf(" ");
				output_token(&var->type);
				if (i != (stmt->function.arguments.count-1))
					printf(", ");
			};
			printf(")\n");
			output_statement(stmt->function.body, index);
			printf("\n");
		} break;

		default: printf("ERROR NOT IMPLEMENTED\n"); break;
	};
};

void output_c(const parser_t *parser)
{
	const stmtList_t *statements = &parser->statements; 
	for (u32 i = 0; i < statements->count; i++)
	{
		const stmt_t *stmt = statements->data[i];
		output_statement(stmt, 0);
	};
};