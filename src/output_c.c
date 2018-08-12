#include "output_c.h"

typedef struct
{
	size_t used;
	size_t size;
	char *data;
} buffer_t;

static void allocBuffer(buffer_t *buffer, size_t initial)
{
	buffer->used = 0;
	buffer->size = initial;
	buffer->data = malloc(initial*sizeof(char));
	assert(buffer->data);
};
static void freeBuffer(buffer_t *buffer)
{
	buffer->used = buffer->size = 0;
	free(buffer->data);
};
static char* pushBytes(buffer_t *buffer, size_t len)
{
	if ((buffer->used + len) > buffer->size)
	{
		buffer->size <<= 1;
		buffer->data = realloc(buffer->data, buffer->size*sizeof(char));
		assert(buffer->data);
	};
	char *ptr = buffer->data + buffer->used;
	buffer->used += len;
	return ptr;
};
#if 0
static void printBuffer(buffer_t *buffer)
{
	puts(buffer->data);
};
#endif
static void saveBuffer(buffer_t *buffer, const char *filename)
{
	FILE *f = fopen(filename, "wb");
	if (f)
	{
		fputs(buffer->data, f);
		fclose(f);
	} else {
		printf("[ERROR] :: Failed to open \"%s\" for writing\n", filename);
	}
};

static void writeStringLen(buffer_t *buffer, const char *str, size_t len)
{
	char *ptr = pushBytes(buffer, len*sizeof(char));
	memcpy(ptr, str, len*sizeof(char));
};
static void writeString(buffer_t *buffer, const char *str)
{
	const size_t len = strlen(str);
	writeStringLen(buffer, str, len);
};
static void writeChar(buffer_t *buffer, char c)
{
	writeStringLen(buffer, &c, 1);
};
static void writeEOF(buffer_t *buffer)
{
	writeChar(buffer, '\0');
};

static void indent(buffer_t *buffer, u32 count)
{
	for (u32 i = 0; i < count; i++)
		writeChar(buffer, '\t');
}
static void output_token(buffer_t *buffer, const token_t *token)
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
		case TOKEN_PLUS_EQUAL: 		str = "+="; break;
		case TOKEN_MINUS_EQUAL: 	str = "-="; break;
		case TOKEN_STAR_EQUAL: 		str = "*="; break;
		case TOKEN_SLASH_EQUAL: 	str = "/="; break;
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
		writeString(buffer, str);
	} else {
		if (token->type == TOKEN_STRING)
			writeChar(buffer, '"');
		writeStringLen(buffer, token->start, token->len);
		if (token->type == TOKEN_STRING)
			writeChar(buffer, '"');
	}
};
static void output_expression(buffer_t *buffer, const expr_t *expr)
{
	switch(expr->type)
	{
		case EXPR_GROUP:
		{
			writeChar(buffer, '(');
			output_expression(buffer, expr->group.expression);
			writeChar(buffer, ')');
		} break;
		case EXPR_PRE_UNARY:
		{
			const token_t *operator = &expr->pre_unary.operator; 
			const expr_t *right = expr->pre_unary.right; 

			output_token(buffer, operator);
			output_expression(buffer, right);
		} break;
		case EXPR_POST_UNARY:
		{
			const token_t *operator = &expr->post_unary.operator; 
			const expr_t *left = expr->post_unary.left; 

			output_expression(buffer, left);
			output_token(buffer, operator);
		} break;
		case EXPR_BINARY:
		{
			const token_t *operator = &expr->binary.operator; 
			const expr_t *left = expr->binary.left; 
			const expr_t *right = expr->binary.right; 

			output_expression(buffer, left);
			output_token(buffer, operator);
			output_expression(buffer, right);
		} break;
		case EXPR_LITERAL:
		{
			const token_t *value = &expr->literal.value; 
			output_token(buffer, value);
		} break;
		case EXPR_VARIABLE:
		{
			const token_t *name = &expr->variable.name;
			output_token(buffer, name);
		} break;
		case EXPR_ASSIGNMENT:
		{
			const token_t *operator = &expr->assignment.operator;
			const token_t *name = &expr->assignment.name;
			output_token(buffer, name);
			writeChar(buffer, ' ');
			output_token(buffer, operator);
			writeChar(buffer, ' ');
			output_expression(buffer, expr->assignment.value);
		} break;
		case EXPR_CALL:
		{
			output_expression(buffer, expr->call.callee);
			writeChar(buffer, '(');
			for (u32 i = 0; i < expr->call.args.count; i++)
			{
				output_expression(buffer, expr->call.args.data[i]);
				if (i != (expr->call.args.count-1))
					writeString(buffer, ", ");
			};
			writeChar(buffer, ')');
		} break;
		case EXPR_BUILTIN:
		{
			const token_t *value = &expr->builtin.value;
			output_token(buffer, value);
			if (expr->builtin.flags & TYPE_FLAG_CONST)
				writeString(buffer, " const");
		} break;
		case EXPR_PTR:
		{
			const expr_t *to = expr->ptr.to;

			output_expression(buffer, to);
			writeString(buffer, " * ");
			if(expr->ptr.flags & TYPE_FLAG_CONST)
				writeString(buffer, "const");
		} break;

		default: writeString(buffer, "ERROR NOT IMPLEMENTED\n"); break;
	};
};
static void output_arg_list(buffer_t *buffer, const argList_t *arguments)
{
	writeString(buffer, "(");
	if (arguments && arguments->count)
	{
		for (u32 i = 0; i < arguments->count; i++)
		{
			const varDecl_t *var = arguments->data + i;
			output_expression(buffer, var->type);
			writeString(buffer, " ");
			output_token(buffer, &var->name);
			if (i != (arguments->count-1))
				writeString(buffer, ", ");
		};
	} else {
		writeString(buffer, "void");
	}
	writeString(buffer, ")");
};

static void output_statement(buffer_t *buffer, const stmt_t *stmt, u32 index)
{
	indent(buffer, index);
	switch(stmt->type)
	{
		case STMT_NONE: break;
		case STMT_VAR:
		{
			output_expression(buffer, stmt->var.decl.type);
			writeChar(buffer, ' ');
			output_token(buffer, &stmt->var.decl.name);
			if (stmt->var.initializer)
			{
				writeString(buffer, " = ");
				output_expression(buffer, stmt->var.initializer);
			}
			writeString(buffer, ";\n");
		} break;
		case STMT_EXPR:
		{
			output_expression(buffer, stmt->expression.expr);
			writeString(buffer, ";\n");
		} break;
		case STMT_BLOCK:
		{
			writeString(buffer, "{\n");
			for (u32 i = 0; i < stmt->block.statements.count; ++i)
			{
				output_statement(buffer, stmt->block.statements.data[i], index+1);
			}
			indent(buffer, index);
			writeString(buffer, "}\n");
		} break;
		case STMT_IF:
		{
			writeString(buffer, "if (");
			output_expression(buffer, stmt->conditional.condition);
			writeString(buffer, ")\n");
			output_statement(buffer, stmt->conditional.thenBranch, index+1);
			writeString(buffer, "\n");
			if (stmt->conditional.elseBranch)
			{
				indent(buffer, index);
				writeString(buffer, "else\n");
				output_statement(buffer, stmt->conditional.elseBranch, index+1);
			};
			indent(buffer, index);
		} break;
		case STMT_RETURN:
		{
			const expr_t *value = stmt->ret.value;

			writeString(buffer, "return ");
			output_expression(buffer, value);
			writeString(buffer, ";\n");
		} break;
		case STMT_FUNCTION:
		{
			const token_t *name = &stmt->function.decl.name;
			const expr_t *type = stmt->function.decl.type;
			const argList_t *arguments = &stmt->function.arguments;
			const stmt_t *body = stmt->function.body;

			output_expression(buffer, type);
			writeString(buffer, " ");
			output_token(buffer, name);
			output_arg_list(buffer, arguments);
			writeString(buffer, "\n");
			output_statement(buffer, body, index);
		} break;
		case STMT_WHILE:
		{
			const expr_t *condition = stmt->whileLoop.condition;
			const stmt_t *body = stmt->whileLoop.body;

			writeString(buffer, "while(");
			output_expression(buffer, condition);
			writeString(buffer, ")\n");
			output_statement(buffer, body, index);
		} break;

		default: writeString(buffer, "ERROR NOT IMPLEMENTED\n"); break;
	};
};

static void output_std_header(buffer_t *buffer)
{
	writeString(buffer, "/* WARNING: This file is auto-generated. Do not modify */\n");
	writeString(buffer, "#include <stdio.h>\n");
	writeString(buffer, "#include <stdlib.h>\n");
	writeString(buffer, "#include <stdint.h>\n");
	writeString(buffer, "#include <stddef.h>\n");
	writeString(buffer, "#include <stdbool.h>\n");
	writeString(buffer, "#include <float.h>\n");

	writeString(buffer, "typedef uint8_t  u8;\n");
	writeString(buffer, "typedef uint16_t u16;\n");
	writeString(buffer, "typedef uint32_t u32;\n");
	writeString(buffer, "typedef uint64_t u64;\n");

	writeString(buffer, "typedef int8_t  i8;\n");
	writeString(buffer, "typedef int16_t i16;\n");
	writeString(buffer, "typedef int32_t i32;\n");
	writeString(buffer, "typedef int64_t i64;\n");

	writeString(buffer, "typedef float  f32;\n");
	writeString(buffer, "typedef double f64;\n");
};
static void output_prototypes(buffer_t *buffer, const parser_t *parser)
{
	writeString(buffer, "/* Function prototypes */\n");
	const stmtList_t *statements = &parser->statements; 
	for (u32 i = 0; i < statements->count; i++)
	{
		const stmt_t *stmt = statements->data[i];
		switch (stmt->type)
		{
			// Output the prototype of all functions
			case STMT_FUNCTION:
			{
				const token_t *name = &stmt->function.decl.name;
				const expr_t *type = stmt->function.decl.type;
				const argList_t *arguments = &stmt->function.arguments;

				output_expression(buffer, type);
				writeString(buffer, " ");
				output_token(buffer, name);
				output_arg_list(buffer, arguments);
				writeString(buffer, ";\n");
			} break;
			// Default case for everything else
			default: break;
		}
	};
};
static void output_all_statements(buffer_t *buffer, const parser_t *parser)
{
	// Just print all the statements
	writeString(buffer, "/* Code */\n");
	const stmtList_t *statements = &parser->statements; 
	for (u32 i = 0; i < statements->count; i++)
	{
		const stmt_t *stmt = statements->data[i];
		output_statement(buffer, stmt, 0);
	};
};

void output_c(const parser_t *parser, const char *filename)
{
	// Allocate an output buffer to write to, small initially
	buffer_t buffer = {};
	allocBuffer(&buffer, 128);
	// Ouput the standard header to the file
	output_std_header(&buffer);
	// Output function prototypes
	output_prototypes(&buffer, parser);
	// Output the rest of the code
	output_all_statements(&buffer, parser);
	// Write the NULL character
	writeEOF(&buffer);
	// Save the buffer to the output
	#if 0
		printBuffer(&buffer);
	#else
		saveBuffer(&buffer, filename);
	#endif
	// Free the buffer
	freeBuffer(&buffer);
};