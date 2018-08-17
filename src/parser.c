#include "parser.h"

// Write an error into the std out
static void error(parser_t *parser, token_t token, const char *msg)
{
	printf("ERROR [%d:%d] :: %s\n", token.line, token.line_offset, msg);
	parser->error = PARSER_ERROR;
};

/* Parsing controls */
// Returns the current token without consuming it
static token_t peek(const parser_t *parser)
{
	return parser->tokens->data[parser->current];
};
// Returns the previous token without consuming the current token
static token_t peekPrev(const parser_t *parser)
{
	i32 index = parser->current;
	if (index > 0)
		index = index - 1;
	return parser->tokens->data[index];
};
// Returns the next token without consuming the current token
static token_t peekNext(const parser_t *parser)
{
	i32 index = parser->current;
	if (index < parser->tokens->used)
		index = index + 1;
	return parser->tokens->data[index];
};
// Checks if the token stream is exhausted
static inline bool isAtEnd(const parser_t *parser)
{
	token_t token = peek(parser);
	return token.type == TOKEN_EOF;
};
// Returns true if the current token's type matches the given type
static inline bool check(const parser_t *parser, tokenType_t type)
{
	if (isAtEnd(parser))
		return false;
	return (peek(parser).type == type);
};
// Unconditional advancement of the token list
static token_t advance(parser_t *parser)
{
	if (!isAtEnd(parser))
		parser->current ++;
	return peekPrev(parser);
};
// Check for one of several possable types. Returns true if one or more is found.
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
// Consume an expected token, or throw en error if it's not found
static bool consume_check(parser_t *parser, tokenType_t type, const char *msg)
{
	if (check(parser, type))
	{
		advance(parser);
		return true;
	};
	error(parser, peekPrev(parser), msg);
	return false;
};
// Eat a token type or return NULL from the current function if it doesn't match
#define consume(parser, type, msg)		{ if(!consume_check(parser, type, msg)) return NULL; }

static typeFlags_t checkForVarKeyword(parser_t *parser, typeFlags_t flags)
{
	// Clear the const flag if the var type is set
	{
		const tokenType_t types[] = { TOKEN_VAR };
		if (match(parser, types, static_len(types)))
		{
			if (flags & TYPE_FLAG_CONST)
				flags &= ~TYPE_FLAG_CONST;
		}
	}
	return flags;
};

/* Type expression parsers */
static expr_t* parseType(parser_t *parser, typeFlags_t flags);
static expr_t* parseBuiltinType(parser_t *parser, typeFlags_t flags)
{
	// Clear the const flag if the var type is set
	flags = checkForVarKeyword(parser, flags);
	// Check for basic types
	{
		const tokenType_t types[] = 
		{
			TOKEN_U8, TOKEN_U16, TOKEN_U32, TOKEN_U64,
			TOKEN_I8, TOKEN_I16, TOKEN_I32, TOKEN_I64,
			TOKEN_F32, TOKEN_F64, 
			TOKEN_BOOL, TOKEN_CHAR,
			TOKEN_VOID
		};
		if (match(parser, types, static_len(types)))
		{
			token_t value = peekPrev(parser);

			// Set the basic flag
			flags |= TYPE_FLAG_BASIC;
			// If the type is a return type, then const does nothing. Just flip it off
			if (flags & TYPE_FLAG_RETURN)
			{
				flags &= ~TYPE_FLAG_CONST;
			};

			expr_t *expr = allocExpression();
			expr->type = EXPR_BUILTIN;
			expr->builtin.value = value;
			expr->builtin.flags = flags;
			return expr;
		};
	}
	return NULL;
};
static expr_t* parsePtrType(parser_t *parser, typeFlags_t flags)
{
	consume(parser, TOKEN_LESS, "Expected opening '<' for pointer expression");

	// Default to constant inner types
	expr_t *to = parseType(parser, (TYPE_FLAG_CONST));
	if (!to)
	{
		error(parser, peek(parser), "Expected type expression for ptr statement");
		return NULL;
	}

	consume(parser, TOKEN_GREATER, "Expected closing '>' for pointer expression");

	expr_t *expr = allocExpression();
	expr->type = EXPR_PTR;
	expr->ptr.to = to;
	expr->ptr.flags = flags;
	return expr;
};
static expr_t* parseType(parser_t *parser, typeFlags_t flags)
{
	flags = checkForVarKeyword(parser, flags);
	// Parse pointer types
	{
		const tokenType_t types[] = { TOKEN_PTR };
		if (match(parser, types, static_len(types)))
		{
			return parsePtrType(parser, flags);
		};
	}
	return parseBuiltinType(parser, flags);
};

/* Basic expression parsers */
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

			expr_t *lit = allocExpression();
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

			expr_t *lit = allocExpression();
			lit->type = EXPR_VARIABLE;
			lit->variable.name = name;
			return lit;
		}
	}
	// Check for groupings/cast
	{
		const tokenType_t types[] = { TOKEN_OPEN_PAREN };
		if (match(parser, types, static_len(types)))
		{
			expr_t *typeExpr = parseType(parser, 0);
			if (typeExpr)
			{
				// Cast expression
				consume(parser, TOKEN_CLOSE_PAREN, "Expected ')' to close cast");

				expr_t *expr = parseExpression(parser);
				if (!expr)
				{
					return NULL;
				}

				expr_t *cast = allocExpression();
				cast->type = EXPR_CAST;
				cast->cast.type = typeExpr;
				cast->cast.expression = expr;
				return cast;
			} else {
				// Basic group
				expr_t *expr = parseExpression(parser);
				if (!expr)
				{
					return NULL;
				}

				consume(parser, TOKEN_CLOSE_PAREN, "Expected ')' to close expression");
				
				expr_t *group = allocExpression();
				group->type = EXPR_GROUP;
				group->group.expression = expr;
				return group;
			}
		}
	}
	return NULL;
};
static expr_t* parseCallExpression(parser_t *parser)
{
	expr_t *expr = parsePrimaryExpression(parser);
	if (expr)
	{
		while (true)
		{
			const tokenType_t open_types[] = { TOKEN_OPEN_PAREN };
			if (match(parser, open_types, static_len(open_types)))
			{
				// Allocate a new call expression
				expr_t *call_expr = allocExpression();
				call_expr->type = EXPR_CALL;
				call_expr->call.callee = expr;
				// Push arguments to the args list
				exprList_t *args = &call_expr->call.args;
				if (!check(parser, TOKEN_CLOSE_PAREN))
				{
					const tokenType_t next_types[] = { TOKEN_COMMA };
					do
					{
						if ((args->count + 1) >= MAX_ARGUMENTS)
						{
							error(parser, peek(parser), "Cannot have more than "stringify(MAX_ARGUMENTS)" arguments per function call.");
							return NULL;
						};
						expr_t *expr = parseExpression(parser);
						if (!expr)
						{
							return NULL;
						}
						pushExpr(args, expr);
					} while (match(parser, next_types, static_len(next_types)));
				}
				// Consume the closing parenthesis
				consume(parser, TOKEN_CLOSE_PAREN, "Expected ')' to close function call");
				// Iterate 
				expr = call_expr;
			} else {
				break;
			}
		};
	}
	return expr;
};
static expr_t* parsePostUnaryExpression(parser_t *parser)
{
	expr_t *expr = parseCallExpression(parser);
	if (expr)
	{
		const tokenType_t types[] =
		{
			TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS,
		};
		if (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);

			expr_t *un = allocExpression();
			un->type = EXPR_POST_UNARY;
			un->post_unary.operator = operator;
			un->post_unary.left = expr;

			expr = un;
		};
	};
	return expr;
};
static expr_t* parsePreUnaryExpression(parser_t *parser)
{
	// Unary checks (right to left)
	const tokenType_t types[] =
	{
		TOKEN_BANG, TOKEN_TILDE,
		TOKEN_PLUS, TOKEN_MINUS,
		TOKEN_PLUS_PLUS, TOKEN_MINUS_MINUS,
		TOKEN_STAR, TOKEN_AND,
	};
	if (match(parser, types, static_len(types)))
	{
		token_t operator = peekPrev(parser);
		expr_t *right = parsePreUnaryExpression(parser);
		if (!right)
		{
			error(parser, operator, "Expected right hand expression");
			return NULL;
		}

		expr_t *un = allocExpression();
		un->type = EXPR_PRE_UNARY;
		un->pre_unary.operator = operator;
		un->pre_unary.right = right;
		return un;
	};
	return parsePostUnaryExpression(parser); 
};
static expr_t* parseMultiplicationExpression(parser_t *parser)
{
	expr_t *expr = parsePreUnaryExpression(parser);
	if (expr)
	{
		// Multiplication checks for '*' and '/'
		const tokenType_t types[] = { TOKEN_STAR, TOKEN_SLASH };
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = parsePreUnaryExpression(parser);
			if (!right)
			{
				error(parser, operator, "Expected right hand expression");
				return NULL;
			}

			expr_t *mult = allocExpression();
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
			if (!right)
			{
				error(parser, operator, "Expected right hand expression");
				return NULL;
			}

			expr_t *add = allocExpression();
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
		// Comparison checks for >, >=, <, <=
		const tokenType_t types[] =
		{ 
			TOKEN_GREATER, TOKEN_GREATER_EQUAL, 
			TOKEN_LESS, TOKEN_LESS_EQUAL
		};
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = parseAdditionExpression(parser);
			if (!right)
			{
				error(parser, operator, "Expected right hand expression");
				return NULL;
			}

			expr_t *comp = allocExpression();
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
			if (!right)
			{
				error(parser, operator, "Expected right hand expression");
				return NULL;
			}
			
			expr_t *eq = allocExpression();
			eq->type = EXPR_BINARY;
			eq->binary.left = expr;
			eq->binary.operator = operator;
			eq->binary.right = right;

			expr = eq;
		};
	}
	return expr;
};
static expr_t* parseLogicalAndExpression(parser_t *parser)
{
	expr_t *expr = parseEqualityExpression(parser);
	if (expr)
	{
		const tokenType_t types[] = { TOKEN_AND_AND };
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = parseLogicalAndExpression(parser);
			if (!right)
			{
				error(parser, operator, "Expected right hand expression");
				return NULL;
			}

			expr_t *andExp = allocExpression();
			andExp->type = EXPR_BINARY;
			andExp->binary.operator = operator;
			andExp->binary.left = expr;
			andExp->binary.right = right;
			expr = andExp;
		};
	}
	return expr;
}
static expr_t* parseLogicalOrExpression(parser_t *parser)
{
	expr_t *expr = parseLogicalAndExpression(parser);
	if (expr)
	{
		const tokenType_t types[] = { TOKEN_OR_OR };
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = parseLogicalAndExpression(parser);
			if (!right)
			{
				error(parser, operator, "Expected right hand expression");
				return NULL;
			}

			expr_t *orExp = allocExpression();
			orExp->type = EXPR_BINARY;
			orExp->binary.operator = operator;
			orExp->binary.left = expr;
			orExp->binary.right = right;
			expr = orExp;
		};
	}
	return expr;
};
static expr_t* parseAssignmentExpression(parser_t *parser)
{
	expr_t *expr = parseLogicalOrExpression(parser);
	if (expr)
	{
		const tokenType_t types[] = 
		{ 
			TOKEN_EQUAL, 
			TOKEN_PLUS_EQUAL, TOKEN_MINUS_EQUAL, TOKEN_STAR_EQUAL, TOKEN_SLASH_EQUAL,
		};
		if (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *value = parseAssignmentExpression(parser);
			if (!value)
			{
				error(parser, operator, "Expected right hand expression");
				return NULL;
			}

			// TODO: Check and make sure that the target is valid
			expr_t *new_expr = allocExpression();
			new_expr->type = EXPR_ASSIGNMENT;
			new_expr->assignment.operator = operator;
			new_expr->assignment.target = expr;
			new_expr->assignment.value = value;
			return new_expr;
		}
	};
	return expr;
};
static expr_t* parseExpression(parser_t *parser)
{
	return parseAssignmentExpression(parser);
};

/* Declaration statement parsers */
static stmt_t* parseStatement(parser_t *parser);
static stmt_t* parseBlockStatement(parser_t *parser);

static stmt_t* parseVariableDeclaration(parser_t *parser)
{
	/* NOTE: There are two types of declaration for variables
		1.	let <variable_name>:<type> = <initializer>;
			Explicit declaration declares the variable of the type <type>, with an optional <initializer> if the variable is non-constant.
			The compiler should present an error if the <initializer> expression does not evaluate to <type>.
		2. let <variable_name> := <initializer>;
			Implicit declaration declares the variable to be the type of the evaluated <initializer> expression. <initializer is not optional.
	*/
	// Const by default
	typeFlags_t typeFlags = (TYPE_FLAG_CONST);
	// Check for the 'var' keyword to make this variable mutable
	{
		const tokenType_t types[] = { TOKEN_VAR };
		if (match(parser, types, static_len(types)))
		{
			typeFlags &= ~(TYPE_FLAG_CONST);
		};
	}
	// Consume the variable name
	consume(parser, TOKEN_IDENTIFIER, "Expected variable name");

	// Get the name token
	token_t name = peekPrev(parser);
	expr_t *type = NULL;
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
					type = parseType(parser, typeFlags);

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
						#if 0
						expr_t *typeOfInitializer = evaluateExprType(initializer);
						if (typeOfInitializer != NULL)
						{
							printf("Type for: %.*s\n", name.len, name.start);
							printExpr(typeOfInitializer, 0);
							freeExpr(typeOfInitializer);
						}
						#endif
					} else {
						// If the type is supposed to be constant, throw an error if there's no initializer
						if (typeFlags & TYPE_FLAG_CONST) 
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
					expr_t *typeOfInitializer = evaluateExprType(initializer);
					if (typeOfInitializer != NULL)
					{
						printf("Type for: %.*s\n", name.len, name.start);
						printExpr(typeOfInitializer, 0);
						//freeExpr(typeOfInitializer);
						type = typeOfInitializer;
					} else {
						error(parser, peekPrev(parser), "Couldn't determine type for type-inferenced variable");
						return NULL;
					}
				} break;
				// It can only be : or :=, just shut up gcc
				default: break;
			};	
		}
	}

	consume(parser, TOKEN_SEMICOLON, "Expected ';' after variable declaration");
	
	stmt_t *stmt = allocStatement();
	stmt->type = STMT_VAR;
	stmt->var.decl.name = name;
	stmt->var.decl.type = type;
	stmt->var.initializer = initializer;
	return stmt;
};
static stmt_t* parseFunctionDeclaration(parser_t *parser, token_t name)
{
	// Parse argument list
	argList_t arguments = {};
	if (!check(parser, TOKEN_CLOSE_PAREN))
	{
		const tokenType_t comma_types[] = { TOKEN_COMMA };
		do 
		{
			if (arguments.count >= MAX_ARGUMENTS)
			{
				error(parser, peek(parser), "Too many arguments for function declaration");
				return NULL;
			}

			token_t name = advance(parser);
			if (name.type != TOKEN_IDENTIFIER)
			{
				error(parser, name, "Expected identifier");
				return NULL;
			}
			consume(parser, TOKEN_COLON, "Expected type expression");
			
			// Parse the type, make sure it's marked as const by default
			expr_t *type = parseType(parser, (TYPE_FLAG_CONST));

			varDecl_t *decl = pushVarDecl(&arguments);
			decl->name = name;
			decl->type = type;
		} while(match(parser, comma_types, static_len(comma_types)));
	};
	consume(parser, TOKEN_CLOSE_PAREN, "Expected closing parenthesis");

	// Parse return type
	expr_t *type = NULL;
	const tokenType_t types[] = { TOKEN_ARROW };
	if (match(parser, types, static_len(types)))
	{
		type = parseType(parser, (TYPE_FLAG_RETURN));
	} else {
		// TODO: This is a hack!!
		token_t voidTok = {};
		voidTok.type = TOKEN_VOID;

		type = allocExpression();
		type->type = EXPR_BUILTIN;
		type->builtin.flags = (TYPE_FLAG_RETURN);
		type->builtin.value = voidTok;
	}

	// Parse the body
	consume(parser, TOKEN_OPEN_BRACE, "Expected block statment for function body");
	
	stmt_t *body = parseBlockStatement(parser);
	if (!body) return NULL;

	stmt_t *stmt = allocStatement();
	stmt->type = STMT_FUNCTION;
	stmt->function.decl.name = name;
	stmt->function.decl.type = type;
	stmt->function.arguments = arguments;
	stmt->function.body = body;
	return stmt;
};
static stmt_t* parseDeclaration(parser_t *parser)
{
	// Parse a variable declaration
	{
		const tokenType_t types[] = { TOKEN_LET };
		if (match(parser, types, static_len(types)))
		{
			return parseVariableDeclaration(parser);
		}
	}
	// Parse a function/struct/union/enum declaration
	{
		const tokenType_t func_types[] = { TOKEN_OPEN_PAREN };
		
		if (peekNext(parser).type == TOKEN_COLON_COLON)
		{
			token_t name = advance(parser); // Get the name
			advance(parser); //Eat the colon_colon
			if (match(parser, func_types, static_len(func_types)))
			{
				return parseFunctionDeclaration(parser, name);
			} else {
				error(parser, peek(parser), "Unknown definition type");
				return NULL;
			};
		};
	}
	return parseStatement(parser);
};

/* Basic statement parsers */
static stmt_t* parseExpressionStatement(parser_t *parser)
{
	expr_t *expr = parseExpression(parser);
	if (!expr)
	{
		return NULL;
	}
	
	consume(parser, TOKEN_SEMICOLON, "Expected ';' after expression");

	stmt_t *stmt = allocStatement();
	stmt->type = STMT_EXPR;
	stmt->expression.expr = expr;
	return stmt;
};
static stmt_t* parseBlockStatement(parser_t *parser)
{
	stmt_t *stmt = allocStatement();
	stmt->type = STMT_BLOCK;

	stmtList_t *statements = &stmt->block.statements;
	while (!check(parser, TOKEN_CLOSE_BRACE) && !isAtEnd(parser))
	{
		stmt_t *inner_stmt = parseDeclaration(parser);
		if (inner_stmt)
		{
			pushStmt(statements, inner_stmt);
		} else return NULL;
	};
	consume(parser, TOKEN_CLOSE_BRACE, "Expected closing brace for block statement");
	return stmt;
};
static stmt_t* parseForStatement(parser_t *parser)
{
	// NOTE: For variables are always mutable
	const typeFlags_t typeFlags = (0);

	consume(parser, TOKEN_OPEN_PAREN, "Expected opening parenthesis");
	consume(parser, TOKEN_IDENTIFIER, "Expected identifier");
	
	token_t name = peekPrev(parser);
	consume(parser, TOKEN_COLON, "Expected ':' for type declaration");
	expr_t *type = parseType(parser, typeFlags);
	if (!type)
	{
		error(parser, peek(parser), "Expected type declaration");
		return NULL;
	}

	//Parse opening statement
	consume(parser, TOKEN_IN, "Expected 'in' token");
	consume(parser, TOKEN_OPEN_BRACKET, "Expected opening '['");

	expr_t *open = parsePrimaryExpression(parser);
	if (!open)
	{
		error(parser, peek(parser), "Expected opening expression");
		return NULL;
	}

	consume(parser, TOKEN_COMMA, "Expected ',' to separate expressions");

	expr_t *close = parsePrimaryExpression(parser);
	if (!close)
	{
		error(parser, peek(parser), "Expected closing expression");
		return NULL;
	}

	bool closeIsInclusive = false;
	{
		const tokenType_t types_close[] = { TOKEN_CLOSE_PAREN, TOKEN_CLOSE_BRACKET };
		if (!match(parser, types_close, static_len(types_close)))
		{
			error(parser, peek(parser), "Expected closing ')' or ']'");
			return NULL;
		}
		closeIsInclusive = (peekPrev(parser).type == TOKEN_CLOSE_BRACKET);
	}
	consume(parser, TOKEN_CLOSE_PAREN, "Expected closing parenthesis");

	// Get the body
	stmt_t *body = parseStatement(parser);
	if (!body)
	{
		token_t last = peekPrev(parser);
		error(parser, last, "Expected body for for statement");
		return NULL;
	};

	// De-sugarization
	{
		//Create the increment statement
		expr_t *var = allocExpression();
		var->type = EXPR_VARIABLE;
		var->variable.name = name;

		token_t operator = {};
		operator.type = TOKEN_PLUS_PLUS;
		
		expr_t *increment = allocExpression();
		increment->type = EXPR_POST_UNARY;
		increment->post_unary.operator = operator;
		increment->post_unary.left = var;

		stmt_t *inc_stmt = allocStatement();
		inc_stmt->type = STMT_EXPR;
		inc_stmt->expression.expr = increment;

		//Alloc the overall statement
		stmt_t *stmt = allocStatement();
		stmt->type = STMT_BLOCK;
		stmtList_t *statements = &stmt->block.statements;
		// Add the body statement 
		pushStmt(statements, body);
		// Add the increment statement
		pushStmt(statements, inc_stmt);
		body = stmt;
	};
	// Add the condition
	{
		expr_t *var = allocExpression();
		var->type = EXPR_VARIABLE;
		var->variable.name = name;

		token_t operator = {};
		operator.type = closeIsInclusive ? TOKEN_LESS_EQUAL : TOKEN_LESS;

		expr_t *condition = allocExpression();
		condition->type = EXPR_BINARY;
		condition->binary.operator = operator;
		condition->binary.left = var;
		condition->binary.right = close;

		// Add a while statement with the conditional and the body
		stmt_t *stmt = allocStatement();
		stmt->type = STMT_WHILE;
		stmt->whileLoop.condition = condition;
		stmt->whileLoop.body = body;
		// Swap the old body with the new while loop
		body = stmt;
	}
	// Add the declaration
	{
		// Create the declaration
		stmt_t *declaration = allocStatement();
		declaration->type = STMT_VAR;
		declaration->var.decl.name = name;
		declaration->var.decl.type = type;
		declaration->var.initializer = open;

		stmt_t *stmt = allocStatement();
		stmt->type = STMT_BLOCK;
		stmtList_t *statements = &stmt->block.statements;
		// Add the initializer statement
		pushStmt(statements, declaration);
		// Add the body statement 
		pushStmt(statements, body);
		// Swap the old body with the new one
		body = stmt;
	};
	return body;
}
static stmt_t* parseWhileStatement(parser_t *parser)
{
	consume(parser, TOKEN_OPEN_PAREN, "Expected opening parenthesis");
	expr_t *condition = parseExpression(parser);
	consume(parser, TOKEN_CLOSE_PAREN, "Expected closing parenthesis");

	stmt_t *body = parseStatement(parser);
	if (!body)
	{
		token_t last = peekPrev(parser);
		error(parser, last, "Expected body for while statement");
		return NULL;
	};

	stmt_t *stmt = allocStatement();
	stmt->type = STMT_WHILE;
	stmt->whileLoop.condition = condition;
	stmt->whileLoop.body = body;
	return stmt;
};
static stmt_t* parseIfStatement(parser_t *parser)
{
	// TODO: Do we want c-like if statements?
	consume(parser, TOKEN_OPEN_PAREN, "Expected opening parenthesis");
	expr_t *condition = parseExpression(parser);
	consume(parser, TOKEN_CLOSE_PAREN, "Expected closing parenthesis");

	stmt_t *thenBranch = parseStatement(parser);
	if (!thenBranch)
	{
		token_t last = peekPrev(parser);
		error(parser, last, "Expected then clause in if statement");
		return NULL;
	};
	stmt_t *elseBranch = NULL;
	{
		const tokenType_t types[] = { TOKEN_ELSE };
		if (match(parser, types, static_len(types)))
		{
			elseBranch = parseStatement(parser);
			if (!elseBranch)
			{
				token_t last = peekPrev(parser);
				error(parser, last, "Expected else clause after 'else'");
				return NULL;
			};
		}
	}

	stmt_t *stmt = allocStatement();
	stmt->type = STMT_IF;
	stmt->conditional.condition = condition;
	stmt->conditional.thenBranch = thenBranch;
	stmt->conditional.elseBranch = elseBranch;
	return stmt;
};
static stmt_t* parseReturnStatement(parser_t *parser)
{
	expr_t *value = NULL;
	if (!check(parser, TOKEN_SEMICOLON))
	{
		// Get the return expression
		value = parseExpression(parser);
	};
	consume(parser, TOKEN_SEMICOLON, "Expected ';' after return statement");

	stmt_t *stmt = allocStatement();
	stmt->type = STMT_RETURN;
	stmt->ret.value = value;
	return stmt;
};
static stmt_t* parseStatement(parser_t *parser)
{
	// Block statement
	{
		const tokenType_t types[] = { TOKEN_OPEN_BRACE };
		if (match(parser, types, static_len(types)))
		{
			return parseBlockStatement(parser);
		};
	}
	// If statement
	{
		const tokenType_t types[] = { TOKEN_IF };
		if (match(parser, types, static_len(types)))
		{
			return parseIfStatement(parser);
		};
	}
	// While statement
	{
		const tokenType_t types[] = { TOKEN_WHILE };
		if (match(parser, types, static_len(types)))
		{
			return parseWhileStatement(parser);
		};
	}
	// For statement
	{
		const tokenType_t types[] = { TOKEN_FOR };
		if (match(parser, types, static_len(types)))
		{
			return parseForStatement(parser);
		};
	}
	// Return statement
	{
		const tokenType_t types[] = { TOKEN_RETURN };
		if (match(parser, types, static_len(types)))
		{
			return parseReturnStatement(parser);
		};
	}
	return parseExpressionStatement(parser);
};

parserError_t parse(parser_t *parser, const char *code, const arrayOf(token_t) *tokens)
{
	zeroMemory(parser, sizeof(parser_t));
	parser->code = code;
	parser->tokens = tokens;
	parser->error = PARSER_NO_ERROR;

	while (true)
	{
		stmt_t *stmt = parseDeclaration(parser);
		if (stmt != NULL)
		{
			pushStmt(&parser->statements, stmt);
		}
		if (isAtEnd(parser) || (parser->error != PARSER_NO_ERROR))
		{
			break;
		}
	}
	return parser->error;
}
void freeParser(parser_t *parser)
{
	// Recursively delete statments and expressions
	freeStmtList(&parser->statements);
};