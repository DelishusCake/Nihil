#include "parser.h"

/* Abstract Syntax Tree structures */
struct expr_s; 
typedef struct expr_s expr_t;

struct stmt_s;
typedef struct stmt_s stmt_t;

defineArrayOf(stmt_t);

typedef enum
{
	EXPR_NONE,
	EXPR_GROUP,
	EXPR_UNARY,
	EXPR_BINARY,
	EXPR_LITERAL,
	EXPR_VARIABLE,
} exprType_t;
struct expr_s
{
	exprType_t type;
	union
	{
		struct
		{
			expr_t *expression;
		} group;
		struct
		{
			token_t operator;
			expr_t *right;
		} unary;
		struct
		{
			token_t operator;
			expr_t *left;
			expr_t *right;
		} binary;
		struct
		{
			token_t value;
		} literal;
		struct
		{
			token_t name;
		} variable;
	};
	// Free/active list pointer
	expr_t *next;
	expr_t *prev;
};

typedef enum
{
	STMT_NONE,
	STMT_EXPR,
	STMT_VAR,
} stmtType_t;
struct stmt_s
{
	stmtType_t type;
	union
	{
		struct
		{
			expr_t *expr;
		} expression;
		struct
		{
			token_t name;
			token_t type;
			expr_t *initializer;
		} var;
		struct 
		{
			arrayOf(stmt_t) statements;
		} group;
	};
};

declareArrayOf(stmt_t);

/* Recursive descent parser */
typedef struct
{
	u32 currentToken;
	const char *code;
	const token_t *tokens;

	// Freelist for expressions
	expr_t *freeExpression;

	// List of global statements
	arrayOf(stmt_t) statements;
} parser_t;

static expr_t* addExpression(parser_t *parser)
{
	expr_t *expr = NULL;
	if (parser->freeExpression)
	{
		expr = parser->freeExpression;
		parser->freeExpression = expr->next;
	}else{
		expr = malloc(sizeof(expr_t));
	}
	if (expr)
	{
		zeroMemory(expr, sizeof(expr_t));
	}
	return expr;
};
static void removeExpression(parser_t *parser, expr_t *expr)
{
	// Recursive removal of child expressions
	switch (expr->type)
	{
		case EXPR_GROUP:
		{
			removeExpression(parser, expr->group.expression);
		} break;
		case EXPR_UNARY:
		{
			removeExpression(parser, expr->unary.right);
		} break;
		case EXPR_BINARY:
		{
			removeExpression(parser, expr->binary.left);
			removeExpression(parser, expr->binary.right);
		} break;
		default: break;
	};

	expr->next = parser->freeExpression;
	parser->freeExpression = expr;
};

static void error(const parser_t *parser, token_t token, const char *msg)
{
	printf("ERROR [%d:%d] ::%s\n", token.line, token.line_offset, msg);
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

// Pre-declare basic expression function
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

			expr_t *lit = addExpression(parser);
			lit->type = EXPR_LITERAL;
			lit->literal.value = value;
			return lit;
		}
	}
	// Check for variables
	{
		const tokenType_t types[] = { TOKEN_IDENTIFIER };
		if (match(parser, types, static_len(types)))
		{
			token_t name = peekPrev(parser);

			expr_t *lit = addExpression(parser);
			lit->type = EXPR_VARIABLE;
			lit->variable.name = name;
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
				expr_t *group = addExpression(parser);
				group->type = EXPR_GROUP;
				group->group.expression = expr;
				return group;
			} else {
				return NULL;
			}
		}
	}
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

		expr_t *un = addExpression(parser);
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

			expr_t *mult = addExpression(parser);
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

			expr_t *add = addExpression(parser);
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
		// Comparison checks for >, >=, <, <=, &&, and ||
		const tokenType_t types[] =
		{ 
			TOKEN_GREATER, TOKEN_GREATER_EQUAL, 
			TOKEN_LESS, TOKEN_LESS_EQUAL,
			TOKEN_AND_AND, TOKEN_OR_OR 
		};
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = addition(parser);

			expr_t *comp = addExpression(parser);
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
			
			expr_t *eq = addExpression(parser);
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

static stmt_t* statement(parser_t *parser);
static stmt_t* expressionStatement(parser_t *parser)
{
	expr_t *expr = expression(parser);
	if (expr)
	{
		if (consume(parser, TOKEN_SEMICOLON, "Expected ';' after expression"))
		{
			stmt_t *stmt = arrayAlloc(stmt_t, &parser->statements);
			stmt->type = STMT_EXPR;
			stmt->expression.expr = expr;
			return stmt;
		};
	};
	return NULL;
};
static stmt_t* statement(parser_t *parser)
{
	return expressionStatement(parser);
};
static stmt_t* variableDeclaration(parser_t *parser)
{
	/* NOTE: There are two types of declaration for variables
		1.	let <variable_name>:<type> = <initializer>;
			Explicit declaration declares the variable of the type <type>, with an optional <initializer> if the variable is non-constant.
			The compiler should present an error if the <initializer> expression does not evaluate to <type>.
		2. let <variable_name> := <initializer>;
			Implicit declaration declares the variable to be the type of the evaluated <initializer> expression. <initializer is not optional.
	*/
	// All variables are constant by default
	bool isConstant = true;
	// Check for the 'var' keyword to make this variable mutable
	{
		const tokenType_t types[] = { TOKEN_VAR };
		if (match(parser, types, static_len(types)))
		{
			isConstant = false;
		};
	}
	// Consume the variable name
	if (consume(parser, TOKEN_IDENTIFIER, "Expected variable name"))
	{
		// Get the name token
		token_t name = peekPrev(parser);
		// TODO: Implement the type system
		token_t type = {};
		// Get the initialization expression
		expr_t *initializer = NULL;
		{
			const tokenType_t type_decl_types[] = { TOKEN_COLON, TOKEN_COLON_EQUAL };
			if (match(parser, type_decl_types, static_len(type_decl_types)))
			{
				switch (peekPrev(parser).type)
				{
					// Explicit declaration
					case TOKEN_COLON:
					{
						// Get the type
						type = advance(parser);
						// If there is an equal sign after the type
						const tokenType_t type_equal_types[] = { TOKEN_EQUAL };
						if (match(parser, type_equal_types, static_len(type_equal_types)))
						{
							// Get the initializer expression
							initializer = expression(parser);
							if (!initializer)
							{
								error(parser, peekPrev(parser), "Expected initializer");
								return NULL;
							};
							// TODO: Check for initializer type
						} else {
							// If the type is supposed to be constant, throw an error if there's no initializer
							if (isConstant) 
							{
								error(parser, peekPrev(parser), "Expected initializer for constant variable declaration");
								return NULL;
							}
						}
					} break;
					// Implicit declaration
					case TOKEN_COLON_EQUAL:
					{
						// Get the initializer expression
						initializer = expression(parser);
						if (!initializer)
						{
							error(parser, peekPrev(parser), "Expected initializer for type-inferenced variable declaration");
							return NULL;
						};
						// TODO: Check for initializer type
					} break;
					// It can only be : or :=, just shut up gcc
					default: break;
				};	
			}
		}

		if (consume(parser, TOKEN_SEMICOLON, "Expected ';' after variable declaration"))
		{
			stmt_t *stmt = arrayAlloc(stmt_t, &parser->statements);
			stmt->type = STMT_VAR;
			stmt->var.name = name;
			stmt->var.type = type;
			stmt->var.initializer = initializer;
			return stmt;
		};
	};
	return NULL;
};
static stmt_t* declaration(parser_t *parser)
{
	{
		const tokenType_t types[] = { TOKEN_LET };
		if (match(parser, types, static_len(types)))
		{
			return variableDeclaration(parser);
		}
	}
	return statement(parser);
}

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
		case TOKEN_REF: type = "REF"; break;
		case TOKEN_STRUCT: type = "STRUCT"; break;
		case TOKEN_UNION: type = "UNION"; break;
		case TOKEN_ENUM: type = "ENUM"; break;
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
		case TOKEN_PTR: type = "PTR"; break;
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
		case EXPR_VARIABLE:
		{
			printf("EXPR_VARIABLE\n");
			for(u32 i = 0; i < index; i++)
				printf("\t");
			printToken(code, &expr->variable.name);
		} break;
	};
};
static void printStatement(const char *code, const stmt_t *stmt)
{
	switch (stmt->type)
	{
		case STMT_NONE: break;
		case STMT_EXPR:
		{
			printExpression(code, stmt->expression.expr, 0);
		} break;
		case STMT_VAR:
		{
			printToken(code, &stmt->var.name);
			printToken(code, &stmt->var.type);
			if (stmt->var.initializer)
			{
				printExpression(code, stmt->var.initializer, 0);
			}
		};
	};
};

i32 parse(const char *code, const token_t *tokens, u32 tokenCount)
{
	parser_t parser = {};
	parser.code = code;
	parser.tokens = tokens;

	#if 0
	for (u32 i = 0; i < tokenCount; i++)
	{
		const token_t *token = tokens + i;
		printToken(code, token);
	}
	#else
	stmt_t *stmt = declaration(&parser);
	while (stmt)
	{
		printStatement(code, stmt);
		stmt = declaration(&parser);
	};
	#endif


	// TODO: Free resources
	freeArray(stmt_t, &parser.statements);
	return 0;
}
