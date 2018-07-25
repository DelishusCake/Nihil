#include "parser.h"

static void printToken(const char *code, const token_t *token)
{
	char *type;
	switch (token->type)
	{
		case TOKEN_EOF: type = "EOF"; break;
		// Single character tokens
		case TOKEN_OPEN_PAREN: type = "OPEN_PAREN"; break;
		case TOKEN_CLOSE_PAREN: type = "CLOSE_PAREN"; break;
		case TOKEN_OPEN_BRACE: type = "OPEN_BRACE"; break;
		case TOKEN_CLOSE_BRACE: type = "CLOSE_BRACE"; break;
		case TOKEN_COMMA: type = "COMMA"; break;
		case TOKEN_DOT: type = "DOT"; break;
		case TOKEN_MINUS: type = "MINUS"; break;
		case TOKEN_PLUS: type = "PLUS"; break;
		case TOKEN_SEMICOLON: type = "SEMICOLON"; break;
		case TOKEN_SLASH: type = "SLASH"; break;
		case TOKEN_STAR: type = "STAR"; break;
		// One/two char tokens
		case TOKEN_BANG: type = "BANG"; break;
		case TOKEN_BANG_EQUAL: type = "BANG_EQUAL"; break;
		case TOKEN_EQUAL: type = "EQUAL"; break;
		case TOKEN_EQUAL_EQUAL: type = "EQUAL_EQUAL"; break;
		case TOKEN_GREATER: type = "GREATER"; break;
		case TOKEN_GREATER_EQUAL: type = "GREATER_EQUAL"; break;
		case TOKEN_LESS: type = "LESS"; break;
		case TOKEN_LESS_EQUAL: type = "LESS_EQUAL"; break;
		case TOKEN_COLON: type = "COLON"; break;
		case TOKEN_COLON_COLON: type = "COLON_COLON"; break;
		case TOKEN_COLON_EQUAL: type = "COLON_EQUAL"; break;
		case TOKEN_PLUS_PLUS: type = "PLUS_PLUS"; break;
		case TOKEN_MINUS_MINUS: type = "MINUS_MINUS"; break;
		case TOKEN_ARROW: type = "ARROW"; break;
		case TOKEN_AND: type = "AND"; break;
		case TOKEN_AND_AND: type = "AND_AND"; break;
		case TOKEN_OR: type = "OR"; break;
		case TOKEN_OR_OR: type = "OR_OR"; break;
		// Literals
		case TOKEN_IDENTIFIER: type = "IDENTIFIER"; break;
		case TOKEN_STRING: type = "STRING"; break;
		case TOKEN_NUMBER: type = "NUMBER"; break;
		// Keywords
		case TOKEN_LET: type = "LET"; break;
		case TOKEN_VAR: type = "VAR"; break;
		case TOKEN_IF: type = "IF"; break;
		case TOKEN_ELSE: type = "ELSE"; break;
		case TOKEN_TRUE: type = "TRUE"; break;
		case TOKEN_FALSE: type = "FALSE"; break;
		case TOKEN_FOR: type = "FOR"; break;
		case TOKEN_WHILE: type = "WHILE"; break;
		case TOKEN_NIL: type = "NIL"; break;
		case TOKEN_EXTERN: type = "EXTERN"; break;
		case TOKEN_RETURN: type = "RETURN"; break;
		case TOKEN_STRUCT: type = "STRUCT"; break;
		case TOKEN_UNION: type = "UNION"; break;
		// Built in types
		case TOKEN_U8: type = "U8"; break;
		case TOKEN_U16: type = "U16"; break;
		case TOKEN_U32: type = "U32"; break;
		case TOKEN_U64: type = "U64"; break;
		case TOKEN_I8: type = "I8"; break;
		case TOKEN_I16: type = "I16"; break;
		case TOKEN_I32: type = "I32"; break;
		case TOKEN_I64: type = "I64"; break;
		case TOKEN_F32: type = "F32"; break;
		case TOKEN_F64: type = "F64"; break;
		case TOKEN_CHAR: type = "CHAR"; break;
		case TOKEN_BOOL: type = "BOOL"; break;
		// Unknown (to make the compiler shut up)
		default: type = "UNKNOWN"; break;
	};
	// Print out literals
	if ((token->type == TOKEN_IDENTIFIER) ||
		(token->type == TOKEN_STRING) ||
		(token->type == TOKEN_NUMBER))
	{
		printf("[%d:%d]\t:: %s:\"%.*s\"\n", 
			token->line, token->line_offset, type,
			token->len, (code+token->start));
	} else {
		printf("[%d:%d]\t:: %s\n", token->line, token->line_offset, type);
	}
};

/* Abstract Syntax Tree structures */
struct expr_s;
typedef struct expr_s expr_t;

typedef struct
{
	expr_t *expression;
} exprGroup_t;

typedef struct
{
	token_t operator;
	expr_t *right;
} exprUnary_t;

typedef struct
{
	token_t operator;
	expr_t *left;
	expr_t *right;
} exprBinary_t;

typedef struct
{
	token_t value;
} exprLiteral_t;

typedef enum
{
	EXPR_NONE,
	EXPR_GROUP,
	EXPR_UNARY,
	EXPR_BINARY,
	EXPR_LITERAL,
} exprType_t;
struct expr_s
{
	exprType_t type;
	union
	{
		exprGroup_t group;
		exprUnary_t unary;
		exprBinary_t binary;
		exprLiteral_t literal;
	};
};

/* Recursive descent parser */
typedef struct
{
	u32 currentToken;
	const char *code;
	const token_t *tokens;

	u32 expressionSize;
	u32 expressionCount;
	expr_t *expressions;
} parser_t;

static void error(const parser_t *parser, token_t token, const char *msg)
{
	printToken(parser->code, &token);
	printf("ERROR [%d:%d] ::%s\n", token.line, token.line_offset, msg);
};

static expr_t* allocExpr(parser_t *parser)
{
	// TODO: Better allocation scheme
	// If we've used all allocated expressions
	if ((parser->expressionCount + 1) >= parser->expressionSize)
	{
		// Double the array length
		parser->expressionSize <<= 1;
		// Reallocate the array
		parser->expressions = realloc(parser->expressions, parser->expressionSize*sizeof(expr_t));
	};
	// Get the current index and increment it
	const u32 index = parser->expressionCount ++;
	// Return the current expression to use
	return (parser->expressions + index);
};

static token_t peek(const parser_t *parser)
{
	return parser->tokens[parser->currentToken];
};
static token_t peekPrev(const parser_t *parser)
{
	i32 index = parser->currentToken;
	if (index > 0)
		index = index - 1;
	return parser->tokens[index];
};
static inline bool isAtEnd(const parser_t *parser)
{
	token_t token = peek(parser);
	return token.type == EOF;
};
static inline bool check(const parser_t *parser, tokenType_t type)
{
	if (isAtEnd(parser))
		return false;
	return (peek(parser).type == type);
};
static token_t advance(parser_t *parser)
{
	if (!isAtEnd(parser))
		parser->currentToken ++;
	return peekPrev(parser);
};
static bool match(parser_t *parser, const tokenType_t *types, u32 typeCount)
{
	for (u32 i = 0; i < typeCount; i++)
	{
		if (check(parser, types[i]))
		{
			advance(parser);
			return true;
		}
	};
	return false;
};
static bool consume(parser_t *parser, tokenType_t type, const char *msg)
{
	if (check(parser, type))
	{
		advance(parser);
		return true;
	};
	error(parser, peekPrev(parser), msg);
	return false;
};

//Pre-declare basic expression function
// Yaaaaaaaaay recursion and functional programming
static expr_t* expression(parser_t *parser);

static expr_t* primary(parser_t *parser)
{
	// Check for boolean values, strings, NULL, and numbers
	{
		const tokenType_t types[] = { TOKEN_FALSE, TOKEN_TRUE, TOKEN_NIL, TOKEN_NUMBER, TOKEN_STRING };
		if (match(parser, types, static_len(types)))
		{
			token_t value = peekPrev(parser);

			expr_t *lit = allocExpr(parser);
			lit->type = EXPR_LITERAL;
			lit->literal.value = value;
			return lit;
		}
	}
	// Check for groupings
	{
		const tokenType_t types[] = { TOKEN_OPEN_PAREN };
		if (match(parser, types, static_len(types)))
		{
			expr_t *expr = expression(parser);
			if (consume(parser, TOKEN_CLOSE_PAREN, "Expected ')' to close expression"))
			{
				expr_t *group = allocExpr(parser);
				group->type = EXPR_GROUP;
				group->group.expression = expr;
				return group;
			} else {
				return NULL;
			}
		}
	}
	error(parser, peek(parser), "Expected expression");
	return NULL;
};
static expr_t* unary(parser_t *parser)
{
	// Unary checks for '!' and '-'
	const tokenType_t types[] = { TOKEN_BANG, TOKEN_MINUS };
	if (match(parser, types, static_len(types)))
	{
		token_t operator = peekPrev(parser);
		expr_t *right = unary(parser);

		expr_t *un = allocExpr(parser);
		un->type = EXPR_UNARY;
		un->unary.operator = operator;
		un->unary.right = right;
		return un;
	};
	return primary(parser); 
};
static expr_t* multiplication(parser_t *parser)
{
	expr_t *expr = unary(parser);
	if (expr)
	{
		// Multiplication checks for '*' and '/'
		const tokenType_t types[] = { TOKEN_STAR, TOKEN_SLASH };
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = unary(parser);

			expr_t *mult = allocExpr(parser);
			mult->type = EXPR_BINARY;
			mult->binary.left = expr;
			mult->binary.operator = operator;
			mult->binary.right = right;

			expr = mult;
		};
	}
	return expr;
};
static expr_t* addition(parser_t *parser)
{
	expr_t *expr = multiplication(parser);
	if (expr)
	{
		// Addition checks for '+' and '-'
		const tokenType_t types[] = { TOKEN_PLUS, TOKEN_MINUS };
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = multiplication(parser);

			expr_t *add = allocExpr(parser);
			add->type = EXPR_BINARY;
			add->binary.left = expr;
			add->binary.operator = operator;
			add->binary.right = right;

			expr = add;
		};
	}
	return expr;
};
static expr_t* comparison(parser_t *parser)
{
	expr_t *expr = addition(parser);
	if (expr)
	{
		// Comparison checks for >, >=, <, and <=
		const tokenType_t types[] = { TOKEN_GREATER, TOKEN_GREATER_EQUAL, TOKEN_LESS, TOKEN_LESS_EQUAL };
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = addition(parser);

			expr_t *comp = allocExpr(parser);
			comp->type = EXPR_BINARY;
			comp->binary.left = expr;	
			comp->binary.operator = operator;	
			comp->binary.right = right;

			expr = comp;	
		};
	}
	return expr;
};
static expr_t* equality(parser_t *parser)
{
	expr_t *expr = comparison(parser);
	if (expr)
	{
		// Equality checks for != and ==
		const tokenType_t types[] = { TOKEN_BANG_EQUAL, TOKEN_EQUAL_EQUAL };
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = comparison(parser);
			
			expr_t *eq = allocExpr(parser);
			eq->type = EXPR_BINARY;
			eq->binary.left = expr;
			eq->binary.operator = operator;
			eq->binary.right = right;

			expr = eq;
		};
	}
	return expr;
};
static expr_t* expression(parser_t *parser)
{
	return equality(parser);
};

static void printExpression(const char *code, const expr_t *expr, u32 index)
{
	for(u32 i = 0; i < index; i++)
		printf("\t");
	switch (expr->type)
	{
		case EXPR_NONE:
		{
			printf("EXPR_NONE\n");
		} break;
		case EXPR_GROUP:
		{
			printf("EXPR_GROUP\n");
			printExpression(code, expr->group.expression, (index + 1));
		} break;
		case EXPR_UNARY:
		{
			printf("EXPR_UNARY\n");
		} break;
		case EXPR_BINARY:
		{
			printf("EXPR_BINARY\n");
			printExpression(code, expr->binary.left, (index + 1));
			for(u32 i = 0; i < index; i++)
				printf("\t");
			printToken(code, &expr->binary.operator);
			printExpression(code, expr->binary.right, (index + 1));
		} break;
		case EXPR_LITERAL:
		{
			printf("EXPR_LITERAL\n");
			for(u32 i = 0; i < index; i++)
				printf("\t");
			printToken(code, &expr->literal.value);
		} break;
	};
};

i32 parse(const char *code, const token_t *tokens, u32 tokenCount)
{
	parser_t parser = {};
	parser.code = code;
	parser.tokens = tokens;

	parser.expressionCount = 0;
	parser.expressionSize = 16;
	parser.expressions = malloc(parser.expressionSize*sizeof(expr_t));

	#if 0
	for (u32 i = 0; i < tokenCount; i++)
	{
		const token_t *token = tokens + i;
		printToken(code, token);
	}
	#endif

	expr_t *expr = expression(&parser);
	while (expr)
	{
		printExpression(code, expr, 0);
		expr = expression(&parser);
	};

	free(parser.expressions);
	return 0;
}