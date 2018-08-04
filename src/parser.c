#include "parser.h"

declareArrayOf(expr_t);
declareArrayOf(stmt_t);

static expr_t* addExpression(arrayOf(expr_t) *expressions)
{
	expr_t *expr = arrayAlloc(expr_t, expressions);
	assert (expr);
	zeroMemory(expr, sizeof(expr_t));
	return expr;
};
static stmt_t* addStatement(arrayOf(stmt_t) *statements)
{
	stmt_t *stmt = arrayAlloc(stmt_t, statements);
	assert (stmt);
	zeroMemory(stmt, sizeof(stmt_t));
	return stmt;
};

static void error(const parser_t *parser, token_t token, const char *msg)
{
	printf("ERROR [%d:%d] ::%s\n", token.line, token.line_offset, msg);
};

static token_t peek(const parser_t *parser)
{
	return parser->tokens->data[parser->current];
};
static token_t peekPrev(const parser_t *parser)
{
	i32 index = parser->current;
	if (index > 0)
		index = index - 1;
	return parser->tokens->data[index];
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
		parser->current ++;
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

static expr_t* parseExpression(parser_t *parser);
static expr_t* parsePrimaryExpression(parser_t *parser)
{
	// Check for boolean values, strings, NULL, and numbers
	{
		const tokenType_t types[] =
		{ 
			TOKEN_FALSE, TOKEN_TRUE, TOKEN_NIL, 
			TOKEN_INTEGER, TOKEN_FLOAT, TOKEN_STRING
		};
		if (match(parser, types, static_len(types)))
		{
			token_t value = peekPrev(parser);

			expr_t *lit = addExpression(&parser->expressions);
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

			expr_t *lit = addExpression(&parser->expressions);
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
				expr_t *group = addExpression(&parser->expressions);
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

		expr_t *un = addExpression(&parser->expressions);
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

			expr_t *mult = addExpression(&parser->expressions);
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

			expr_t *add = addExpression(&parser->expressions);
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

			expr_t *comp = addExpression(&parser->expressions);
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
			
			expr_t *eq = addExpression(&parser->expressions);
			eq->type = EXPR_BINARY;
			eq->binary.left = expr;
			eq->binary.operator = operator;
			eq->binary.right = right;

			expr = eq;
		};
	}
	return expr;
};
static expr_t* parseAssignmentExpression(parser_t *parser)
{
	expr_t *expr = parseEqualityExpression(parser);
	if (expr)
	{
		const tokenType_t types[] = { TOKEN_EQUAL };
		if (match(parser, types, static_len(types)))
		{
			token_t equals = peekPrev(parser);
			expr_t *value = parseAssignmentExpression(parser);

			if (expr->type == EXPR_VARIABLE)
			{
				token_t name = expr->variable.name;

				expr_t *new_expr = addExpression(&parser->expressions);
				new_expr->type = EXPR_ASSIGNMENT;
				new_expr->assignment.name = name;
				new_expr->assignment.value = value;
				return new_expr;
			} else {
				error(parser, equals, "Invalid assignment target");
				return NULL;
			};
		}
	};
	return expr;
};
static expr_t* parseExpression(parser_t *parser)
{
	return parseAssignmentExpression(parser);
};

static stmt_t* parseStatement(parser_t *parser, arrayOf(stmt_t) *statements);
static stmt_t* parseExpressionStatement(parser_t *parser, arrayOf(stmt_t) *statements)
{
	expr_t *expr = parseExpression(parser);
	if (expr)
	{
		if (consume(parser, TOKEN_SEMICOLON, "Expected ';' after expression"))
		{
			stmt_t *stmt = addStatement(statements);
			stmt->type = STMT_EXPR;
			stmt->expression.expr = expr;
			return stmt;
		};
	};
	return NULL;
};
static stmt_t* parseVariableDeclaration(parser_t *parser, arrayOf(stmt_t) *statements)
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
			stmt_t *stmt = addStatement(statements);
			stmt->type = STMT_VAR;
			stmt->var.name = name;
			stmt->var.type = type;
			stmt->var.initializer = initializer;
			return stmt;
		};
	};
	return NULL;
};
static stmt_t* parseDeclaration(parser_t *parser, arrayOf(stmt_t) *statements)
{
	{
		const tokenType_t types[] = { TOKEN_LET };
		if (match(parser, types, static_len(types)))
		{
			return parseVariableDeclaration(parser, statements);
		}
	}
	return parseStatement(parser, statements);
}
static stmt_t* parseBlockStatement(parser_t *parser, arrayOf(stmt_t) *statements)
{
	stmt_t *stmt = addStatement(statements);
	stmt->type = STMT_BLOCK;

	arrayOf(stmt_t) *inner_statements = &stmt->block.statements;
	zeroMemory(inner_statements, sizeof(stmt_t));
	while (!check(parser, TOKEN_CLOSE_BRACE) && !isAtEnd(parser))
	{
		parseDeclaration(parser, inner_statements);
	};
	return stmt;
};
static stmt_t* parseStatement(parser_t *parser, arrayOf(stmt_t) *statements)
{
	#if 1
	{
		const tokenType_t types[] = { TOKEN_OPEN_BRACE };
		if (match(parser, types, static_len(types)))
		{
			return parseBlockStatement(parser, statements);
		};
	}
	#endif
	return parseExpressionStatement(parser, statements);
};

static void printToken(const token_t *token, bool printLine)
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
		case TOKEN_INTEGER: type = "INTEGER"; break;
		case TOKEN_FLOAT: type = "FLOAT"; break;
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
		(token->type == TOKEN_INTEGER) ||
		(token->type == TOKEN_FLOAT))
	{
		if (printLine)
		{
			printf("[%d:%d]\t:: %s: \"%.*s\"\n",
				token->line, token->line_offset, 
				type, token->len, token->start);
		}else{
			printf("%s: \"%.*s\"\n", 
				type, token->len, token->start);
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
static void printExpression(const expr_t *expr, u32 index)
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
			printExpression(expr->group.expression, (index + 1));
		} break;
		case EXPR_UNARY:
		{
			printf("EXPR_UNARY\n");
		} break;
		case EXPR_BINARY:
		{
			printf("EXPR_BINARY\n");
			printExpression(expr->binary.left, (index + 1));
			for(u32 i = 0; i < index+1; i++)
				printf("\t");
			printToken(&expr->binary.operator, false);
			printExpression(expr->binary.right, (index + 1));
		} break;
		case EXPR_LITERAL:
		{
			printf("EXPR_LITERAL\n");
			for(u32 i = 0; i < index+1; i++)
				printf("\t");
			printToken(&expr->literal.value, false);
		} break;
		case EXPR_VARIABLE:
		{
			printf("EXPR_VARIABLE\n");
			for(u32 i = 0; i < index+1; i++)
				printf("\t");
			printToken(&expr->variable.name, false);
		} break;
		case EXPR_ASSIGNMENT:
		{
			printf("EXPR_ASSIGNMENT\n");
			for(u32 i = 0; i < index+1; i++)
				printf("\t");
			printToken(&expr->assignment.name, false);
			printExpression(expr->assignment.value, (index + 1));
		} break;
	};
};
static void printStatement(const stmt_t *stmt, u32 index)
{
	for (u32 i = 0; i < index; i++)
		printf("\t");
	assert (stmt);
	switch (stmt->type)
	{
		default:
		case STMT_NONE: break;
		case STMT_EXPR:
		{
			printf("STMT_EXPR\n");
			printExpression(stmt->expression.expr, index);
		} break;
		case STMT_VAR:
		{
			printf("STMT_VAR\n");

			const token_t *nameToken = &stmt->var.name;
			printf("NAME :: %.*s\n", nameToken->len, nameToken->start);
			printToken(&stmt->var.type, false);
			if (stmt->var.initializer)
			{
				printExpression(stmt->var.initializer, index);
			}
		} break;
		case STMT_BLOCK:
		{
			printf("STMT_BLOCK\n");
			#if 1
			for (u32 i = 0; i < stmt->block.statements.used; i++)
			{
				const stmt_t *next_stmt = stmt->block.statements.data + i;
				printStatement(next_stmt, index+1);
			};
			#endif
		} break;
	};
};

parserError_t parse(parser_t *parser, const char *code, const arrayOf(token_t) *tokens)
{
	zeroMemory(parser, sizeof(parser_t));
	parser->code = code;
	parser->tokens = tokens;

	#if 0
	for (u32 i = 0; i < tokens->used; i++)
	{
		const token_t *token = tokens->data + i;
		printToken(token, true);
	}
	#else
	stmt_t *stmt = parseDeclaration(parser, &parser->statements);
	while (stmt)
	{
		printStatement(stmt, 0);
		stmt = parseDeclaration(parser, &parser->statements);
	};
	#endif
	return PARSER_NO_ERROR;
}
void freeParser(parser_t *parser)
{
	for (u32 i = 0; i < parser->statements.used; i++)
	{
		stmt_t *stmt = parser->statements.data + i;
		if (stmt->type == STMT_BLOCK)
		{
			freeArray(stmt_t, &stmt->block.statements);
		};
	};
	freeArray(stmt_t, &parser->statements);
	freeArray(expr_t, &parser->expressions);
};