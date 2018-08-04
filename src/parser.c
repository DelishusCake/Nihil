#include "parser.h"

/* Abstract Syntax Tree structures */
struct expr_s; 
typedef struct expr_s expr_t;

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
	expr_t *next, *prev;
};

struct stmt_s;
typedef struct stmt_s stmt_t;

defineArrayOf(stmt_t);

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
			type_t *type;
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

	// Freelists for recyclable types
	expr_t *freeExpression;
	type_t *freeType;

	// List of global statements
	arrayOf(stmt_t) statements;
} parser_t;

static expr_t* createExpression(parser_t *parser)
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
static void recycleExpression(parser_t *parser, expr_t *expr)
{
	// Recursive removal of child expressions
	switch (expr->type)
	{
		case EXPR_GROUP:
		{
			recycleExpression(parser, expr->group.expression);
		} break;
		case EXPR_UNARY:
		{
			recycleExpression(parser, expr->unary.right);
		} break;
		case EXPR_BINARY:
		{
			recycleExpression(parser, expr->binary.left);
			recycleExpression(parser, expr->binary.right);
		} break;
		default: break;
	};

	expr->next = parser->freeExpression;
	parser->freeExpression = expr;
};

static type_t* createType(parser_t *parser)
{
	type_t *type = NULL;
	if (parser->freeType)
	{
		type = parser->freeType;
		parser->freeType = type->next;
	}else{
		type = malloc(sizeof(type_t));
	}
	if (type)
	{
		zeroMemory(type, sizeof(type_t));
	}
	return type;
};
static void recycleType(parser_t *parser, type_t *type)
{
	// Recursive recycling of ptr types
	if (type->ptrTo)
	{
		recycleType(parser, type->ptrTo);
	};

	type->next = parser->freeType;
	parser->freeType = type;
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

static type_t* parseType(parser_t *parser);
static type_t* parseBuiltInType(parser_t *parser)
{
	{
		const tokenType_t types[] =
		{ 
			TOKEN_U8, TOKEN_U16, TOKEN_U32, TOKEN_U64,
			TOKEN_I8, TOKEN_I16, TOKEN_I32, TOKEN_I64,
			TOKEN_F32, TOKEN_F64,
			TOKEN_CHAR, TOKEN_BOOL
		};
		if (match(parser, types, static_len(types)))
		{
			size_t size = 0;
			size_t align = 0;
			typeClass_t class = TYPE_CLASS_NONE;

			tokenType_t tokenType = peekPrev(parser).type;
			switch (tokenType)
			{
				case TOKEN_U8:
				{
					size = 1;
					align = 4;
					class = TYPE_CLASS_U8;
				} break; 
				case TOKEN_U16:
				{
					size = 2;
					align = 4;
					class = TYPE_CLASS_U16;
				} break; 
				case TOKEN_U32:
				{
					size = 4;
					align = 4;
					class = TYPE_CLASS_U32;
				} break; 
				case TOKEN_U64:
				{
					size = 8;
					align = 8;
					class = TYPE_CLASS_U64;
				} break;
				
				case TOKEN_I8:
				{
					size = 1;
					align = 4;
					class = TYPE_CLASS_I8;
				} break; 
				case TOKEN_I16:
				{
					size = 2;
					align = 4;
					class = TYPE_CLASS_I16;
				} break; 
				case TOKEN_I32:
				{
					size = 4;
					align = 4;
					class = TYPE_CLASS_I32;
				} break; 
				case TOKEN_I64:
				{
					size = 8;
					align = 8;
					class = TYPE_CLASS_I64;
				} break;
				
				case TOKEN_F32:
				{
					size = 4;
					align = 4;
					class = TYPE_CLASS_F32;
				} break; 
				case TOKEN_F64:
				{
					size = 8;
					align = 8;
					class = TYPE_CLASS_F64;
				} break;
				
				case TOKEN_CHAR:
				{
					size = 1;
					align = 4;
					class = TYPE_CLASS_CHAR;
				} break; 
				case TOKEN_BOOL:
				{
					size = 4;
					align = 4;
					class = TYPE_CLASS_BOOL;
				} break;

				default: break;
			};

			type_t *type = createType(parser);
			type->class = class;
			type->size = size;
			type->align = align;
			type->ptrTo = NULL;
			return type; 
		};
	}
	return NULL;
};
static type_t* parseType(parser_t *parser)
{
	return parseBuiltInType(parser);
};

static expr_t* parseExpression(parser_t *parser);
static expr_t* parsePrimaryExpression(parser_t *parser)
{
	// Check for boolean values, strings, NULL, and numbers
	{
		const tokenType_t types[] = { TOKEN_FALSE, TOKEN_TRUE, TOKEN_NIL, TOKEN_NUMBER, TOKEN_STRING };
		if (match(parser, types, static_len(types)))
		{
			token_t value = peekPrev(parser);

			expr_t *lit = createExpression(parser);
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

			expr_t *lit = createExpression(parser);
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
			expr_t *expr = parseExpression(parser);
			if (consume(parser, TOKEN_CLOSE_PAREN, "Expected ')' to close expression"))
			{
				expr_t *group = createExpression(parser);
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
static expr_t* parseUnaryExpression(parser_t *parser)
{
	// Unary checks for '!' and '-'
	const tokenType_t types[] = { TOKEN_BANG, TOKEN_MINUS };
	if (match(parser, types, static_len(types)))
	{
		token_t operator = peekPrev(parser);
		expr_t *right = parseUnaryExpression(parser);

		expr_t *un = createExpression(parser);
		un->type = EXPR_UNARY;
		un->unary.operator = operator;
		un->unary.right = right;
		return un;
	};
	return parsePrimaryExpression(parser); 
};
static expr_t* parseMultiplicationExpression(parser_t *parser)
{
	expr_t *expr = parseUnaryExpression(parser);
	if (expr)
	{
		// Multiplication checks for '*' and '/'
		const tokenType_t types[] = { TOKEN_STAR, TOKEN_SLASH };
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = parseUnaryExpression(parser);

			expr_t *mult = createExpression(parser);
			mult->type = EXPR_BINARY;
			mult->binary.left = expr;
			mult->binary.operator = operator;
			mult->binary.right = right;

			expr = mult;
		};
	}
	return expr;
};
static expr_t* parseAdditionExpression(parser_t *parser)
{
	expr_t *expr = parseMultiplicationExpression(parser);
	if (expr)
	{
		// Addition checks for '+' and '-'
		const tokenType_t types[] = { TOKEN_PLUS, TOKEN_MINUS };
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = parseMultiplicationExpression(parser);

			expr_t *add = createExpression(parser);
			add->type = EXPR_BINARY;
			add->binary.left = expr;
			add->binary.operator = operator;
			add->binary.right = right;

			expr = add;
		};
	}
	return expr;
};
static expr_t* parseComparisonExpression(parser_t *parser)
{
	expr_t *expr = parseAdditionExpression(parser);
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
			expr_t *right = parseAdditionExpression(parser);

			expr_t *comp = createExpression(parser);
			comp->type = EXPR_BINARY;
			comp->binary.left = expr;	
			comp->binary.operator = operator;	
			comp->binary.right = right;

			expr = comp;	
		};
	}
	return expr;
};
static expr_t* parseEqualityExpression(parser_t *parser)
{
	expr_t *expr = parseComparisonExpression(parser);
	if (expr)
	{
		// Equality checks for != and ==
		const tokenType_t types[] = { TOKEN_BANG_EQUAL, TOKEN_EQUAL_EQUAL };
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = parseComparisonExpression(parser);
			
			expr_t *eq = createExpression(parser);
			eq->type = EXPR_BINARY;
			eq->binary.left = expr;
			eq->binary.operator = operator;
			eq->binary.right = right;

			expr = eq;
		};
	}
	return expr;
};
static expr_t* parseExpression(parser_t *parser)
{
	return parseEqualityExpression(parser);
};

static stmt_t* parseStatement(parser_t *parser);
static stmt_t* parseExpressionStatement(parser_t *parser)
{
	expr_t *expr = parseExpression(parser);
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
static stmt_t* parseStatement(parser_t *parser)
{
	return parseExpressionStatement(parser);
};
static stmt_t* parseVariableDeclaration(parser_t *parser)
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
		type_t *var_type = NULL;
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
						var_type = parseType(parser);
						if (!var_type)
						{
							error(parser, peekPrev(parser), "Error reading type");
							return NULL;
						};
						// If there is an equal sign after the type
						const tokenType_t type_equal_types[] = { TOKEN_EQUAL };
						if (match(parser, type_equal_types, static_len(type_equal_types)))
						{
							// Get the initializer expression
							initializer = parseExpression(parser);
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
						initializer = parseExpression(parser);
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
			stmt->var.type = var_type;
			stmt->var.initializer = initializer;
			return stmt;
		};
	};
	return NULL;
};
static stmt_t* parseDeclaration(parser_t *parser)
{
	{
		const tokenType_t types[] = { TOKEN_LET };
		if (match(parser, types, static_len(types)))
		{
			return parseVariableDeclaration(parser);
		}
	}
	return parseStatement(parser);
}

static void printToken(const char *code, const token_t *token, bool printLine)
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
		if (printLine)
		{
			printf("[%d:%d]\t:: %s: \"%.*s\"\n",
				token->line, token->line_offset, 
				type, token->len, (code+token->start));
		}else{
			printf("%s: \"%.*s\"\n", 
				type, token->len, (code+token->start));
		}
	} else {
		if (printLine)
		{
			printf("[%d:%d]\t:: %s\n", 
				token->line, token->line_offset,
				type);
		}else{
			printf("%s\n", type);
		}
	}
};
static void printType(const type_t *type)
{
	const char *type_str;
	if (type)
	{
		switch (type->class)
		{
			case TYPE_CLASS_U8:		type_str = "U8"; break; 
			case TYPE_CLASS_U16:	type_str = "U16"; break; 
			case TYPE_CLASS_U32:	type_str = "U32"; break; 
			case TYPE_CLASS_U64:	type_str = "U64"; break;
			
			case TYPE_CLASS_I8:		type_str = "I8"; break; 
			case TYPE_CLASS_I16:	type_str = "I16"; break; 
			case TYPE_CLASS_I32:	type_str = "I32"; break; 
			case TYPE_CLASS_I64:	type_str = "I64"; break;
			
			case TYPE_CLASS_F32:	type_str = "F32"; break; 
			case TYPE_CLASS_F64:	type_str = "F64"; break;
			
			case TYPE_CLASS_CHAR:	type_str = "CHAR"; break; 
			case TYPE_CLASS_BOOL:	type_str = "BOOL"; break;

			case TYPE_CLASS_USER: 	type_str = "USER"; break;

			default:
			case TYPE_CLASS_NONE: 	type_str = "NONE"; break;
		};
	} else {
		// NOTE: For now, implicit types have a NULL type
		type_str = "IMPLICIT";
	}
	printf("TYPE :: %s\n", type_str);
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
			for(u32 i = 0; i < index+1; i++)
				printf("\t");
			printToken(code, &expr->binary.operator, false);
			printExpression(code, expr->binary.right, (index + 1));
		} break;
		case EXPR_LITERAL:
		{
			printf("EXPR_LITERAL\n");
			for(u32 i = 0; i < index+1; i++)
				printf("\t");
			printToken(code, &expr->literal.value, false);
		} break;
		case EXPR_VARIABLE:
		{
			printf("EXPR_VARIABLE\n");
			for(u32 i = 0; i < index+1; i++)
				printf("\t");
			printToken(code, &expr->variable.name, false);
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
			const token_t *nameToken = &stmt->var.name;
			printf("NAME :: %.*s\n", nameToken->len, (code+nameToken->start));
			printType(stmt->var.type);
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
		printToken(code, token, true);
	}
	#else
	stmt_t *stmt = parseDeclaration(&parser);
	while (stmt)
	{
		printStatement(code, stmt);
		stmt = parseDeclaration(&parser);
	};
	#endif


	// TODO: Free resources
	freeArray(stmt_t, &parser.statements);
	return 0;
}
