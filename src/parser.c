#include "parser.h"

/* Memory management */
static expr_t* allocExpression(linAlloc_t *alloc)
{
	expr_t *expr = (expr_t*) pushLinAlloc(alloc, sizeof(expr_t));
	assert (expr); // TOOD: Handle reallocating the linear allocator on failed allocation
	zeroMemory(expr, sizeof(expr_t));
	return expr;
};
static stmt_t* allocStatement(linAlloc_t *alloc)
{
	stmt_t *stmt = (stmt_t*) pushLinAlloc(alloc, sizeof(stmt_t));
	assert (stmt); // TOOD: Handle reallocating the linear allocator on failed allocation
	zeroMemory(stmt, sizeof(stmt_t));
	return stmt;
};

/* Statment/Expression list functions */
static void pushStmt(stmtList_t *statements, stmt_t *stmt)
{
	if (!statements->size)
	{
		statements->count = 0;
		statements->size = 16;
		statements->data = malloc(statements->size*sizeof(stmt_t*));
		assert (statements->data);
	} else if ((statements->count + 1) >= statements->size) {
		statements->size <<= 1;
		statements->data = realloc(statements->data, statements->size*sizeof(stmt_t*));
		assert (statements->data);
	};
	const u32 index = statements->count ++;
	statements->data[index] = stmt;
};
static void freeStmtList(stmtList_t *statements)
{
	if (statements->data)
	{
		// Recursively free all inner block statements
		for (u32 i = 0; i < statements->count; i++)
		{
			stmt_t *stmt = statements->data[i];
			if (stmt->type == STMT_BLOCK)
			{
				freeStmtList(&stmt->block.statements);
			};
		};
		// Free the actual array
		free(statements->data);
		// Zero the structure, just in case
		zeroMemory(statements, sizeof(stmtList_t));
	};
};

static void pushExpr(exprList_t *expressions, expr_t *expr)
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
#if 0
static void freeExprList(exprList_t *expressions)
{
	if (expressions->data)
	{
		#if 0
		// Recursively free all inner expression lists
		for (u32 i = 0; i < expressions->count; i++)
		{
			expr_t *stmt = expressions->data[i];
			if (stmt->type == STMT_BLOCK)
			{
				freeStmtList(&stmt->block.expressions);
			};
		};
		#endif
		// Free the actual array
		free(expressions->data);
		// Zero the structure, just in case
		zeroMemory(expressions, sizeof(exprList_t));
	};
};
#endif

static varDecl_t* pushVarDecl(argList_t *arguments)
{
	if (!arguments->size)
	{
		arguments->count = 0;
		arguments->size = 16;
		arguments->data = malloc(arguments->size*sizeof(varDecl_t));
		assert (arguments->data);
	} else if ((arguments->count + 1) >= arguments->size) {
		arguments->size <<= 1;
		arguments->data = realloc(arguments->data, arguments->size*sizeof(varDecl_t));
		assert (arguments->data);
	};
	const u32 index = arguments->count ++;
	return arguments->data + index;
};

/* Debug functions */
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
		case TOKEN_VOID: type = "VOID"; break;
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
		case EXPR_CALL:
		{
			printf("EXPR_CALL\n");
			printExpression(expr->call.callee, index+1);

			const exprList_t *args = &expr->call.args;
			for (u32 i = 0; i < args->count; i++)
			{
				printExpression(args->data[i], index+1);
			}
		} break;
		case EXPR_GROUP:
		{
			printf("EXPR_GROUP\n");
			printExpression(expr->group.expression, (index + 1));
		} break;
		case EXPR_UNARY:
		{
			printf("EXPR_UNARY\n");
			for(u32 i = 0; i < index+1; i++)
				printf("\t");
			printToken(&expr->unary.operator, false);
			printExpression(expr->unary.right, index+1);
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
	switch (stmt->type)
	{
		case STMT_NONE: break;
		case STMT_EXPR:
		{
			printf("STMT_EXPR\n");
			printExpression(stmt->expression.expr, index);
		} break;
		case STMT_VAR:
		{
			printf("STMT_VAR\n");
			for(u32 i = 0; i < index+1; i++)
				printf("\t");
			printToken(&stmt->var.decl.name, false);
			for(u32 i = 0; i < index+1; i++)
				printf("\t");
			printToken(&stmt->var.decl.type, false);
			if (stmt->var.initializer)
			{
				printExpression(stmt->var.initializer, index+1);
			}
		} break;
		case STMT_BLOCK:
		{
			printf("STMT_BLOCK\n");
			#if 1
			for (u32 i = 0; i < stmt->block.statements.count; i++)
			{
				const stmt_t *next_stmt = stmt->block.statements.data[i];
				printStatement(next_stmt, index+1);
			};
			#endif
		} break;
		case STMT_IF:
		{
			printf("STMT_IF\n");
			printExpression(stmt->conditional.condition, index);
			printStatement(stmt->conditional.thenBranch, index+1);
			if (stmt->conditional.elseBranch) 
				printStatement(stmt->conditional.elseBranch, index+1);
		} break;
		case STMT_WHILE:
		{
			printf("STMT_WHILE\n");
			printExpression(stmt->whileLoop.condition, index);
			printStatement(stmt->whileLoop.body, index+1);
		} break;
		case STMT_RETURN:
		{
			printf("STMT_RETURN\n");
			printExpression(stmt->ret.value, index);
		} break;
		case STMT_FUNCTION:
		{
			printf("STMT_FUNCTION\n");
			for(u32 i = 0; i < index+1; i++)
				printf("\t");
			printToken(&stmt->function.name, false);

			const argList_t *args = &stmt->function.arguments;
			for (u32 n = 0; n < args->count; n++)
			{
				for(u32 i = 0; i < index+2; i++)
					printf("\t");
				printToken(&args->data[n].name, false);
				for(u32 i = 0; i < index+2; i++)
					printf("\t");
				printToken(&args->data[n].type, false);
			};

			for(u32 i = 0; i < index+1; i++)
				printf("\t");
			printToken(&stmt->function.type, false);

			printStatement(stmt->function.body, index+1);
		} break;
	};
};
static void error(const parser_t *parser, token_t token, const char *msg)
{
	printf("ERROR [%d:%d] ::%s\n", token.line, token.line_offset, msg);
};

/* Parsing controls */
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
static token_t peekNext(const parser_t *parser)
{
	i32 index = parser->current;
	if (index < parser->tokens->used)
		index = index + 1;
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

			expr_t *lit = allocExpression(&parser->alloc);
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

			expr_t *lit = allocExpression(&parser->alloc);
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
				expr_t *group = allocExpression(&parser->alloc);
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
				expr_t *call_expr = allocExpression(&parser->alloc);
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
						pushExpr(args, parseExpression(parser));
					} while (match(parser, next_types, static_len(next_types)));
				}
				// Consume the closing parenthesis
				if (!consume(parser, TOKEN_CLOSE_PAREN, "Expected ')' to close function call"))
				{
					return NULL;
				}
				// Iterate 
				expr = call_expr;
			} else {
				break;
			}
		};
	}
	return expr;
};
static expr_t* parseUnaryExpression(parser_t *parser)
{
	// Unary checks for '!' and '-'
	const tokenType_t types[] = { TOKEN_BANG, TOKEN_MINUS };
	if (match(parser, types, static_len(types)))
	{
		token_t operator = peekPrev(parser);
		expr_t *right = parseUnaryExpression(parser);

		expr_t *un = allocExpression(&parser->alloc);
		un->type = EXPR_UNARY;
		un->unary.operator = operator;
		un->unary.right = right;
		return un;
	};
	return parseCallExpression(parser); 
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

			expr_t *mult = allocExpression(&parser->alloc);
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

			expr_t *add = allocExpression(&parser->alloc);
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

			expr_t *comp = allocExpression(&parser->alloc);
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
			
			expr_t *eq = allocExpression(&parser->alloc);
			eq->type = EXPR_BINARY;
			eq->binary.left = expr;
			eq->binary.operator = operator;
			eq->binary.right = right;

			expr = eq;
		};
	}
	return expr;
};
static expr_t* parseAndExpression(parser_t *parser)
{
	expr_t *expr = parseEqualityExpression(parser);
	if (expr)
	{
		const tokenType_t types[] = { TOKEN_AND_AND };
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = parseAndExpression(parser);

			expr_t *andExp = allocExpression(&parser->alloc);
			andExp->type = EXPR_BINARY;
			andExp->binary.operator = operator;
			andExp->binary.left = expr;
			andExp->binary.right = right;
			expr = andExp;
		};
	}
	return expr;
}
static expr_t* parseOrExpression(parser_t *parser)
{
	expr_t *expr = parseAndExpression(parser);
	if (expr)
	{
		const tokenType_t types[] = { TOKEN_OR_OR };
		while (match(parser, types, static_len(types)))
		{
			token_t operator = peekPrev(parser);
			expr_t *right = parseAndExpression(parser);

			expr_t *orExp = allocExpression(&parser->alloc);
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
	expr_t *expr = parseOrExpression(parser);
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

				expr_t *new_expr = allocExpression(&parser->alloc);
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

static stmt_t* parseStatement(parser_t *parser);
static stmt_t* parseExpressionStatement(parser_t *parser)
{
	expr_t *expr = parseExpression(parser);
	if (expr)
	{
		if (consume(parser, TOKEN_SEMICOLON, "Expected ';' after expression"))
		{
			stmt_t *stmt = allocStatement(&parser->alloc);
			stmt->type = STMT_EXPR;
			stmt->expression.expr = expr;
			return stmt;
		};
	};
	return NULL;
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
			stmt_t *stmt = allocStatement(&parser->alloc);
			stmt->type = STMT_VAR;
			stmt->var.decl.name = name;
			stmt->var.decl.type = type;
			stmt->var.initializer = initializer;
			return stmt;
		};
	};
	return NULL;
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
			if (!consume(parser, TOKEN_COLON, "Expected comma-separator"))
			{
				return NULL;
			}
			// TODO: Type system
			token_t type = advance(parser);

			varDecl_t *decl = pushVarDecl(&arguments);
			decl->name = name;
			decl->type = type;
		} while(match(parser, comma_types, static_len(comma_types)));
	};
	if (!consume(parser, TOKEN_CLOSE_PAREN, "Expected closing parenthesis"))
		return NULL;
	// Parse return type
	token_t type = {};
	const tokenType_t types[] = { TOKEN_ARROW };
	if (match(parser, types, static_len(types)))
	{
		// TODO: Type system
		type = advance(parser);
	} else {
		type.type = TOKEN_VOID;
	}

	// Parse the body
	stmt_t *body = parseStatement(parser);
	if (!body)
	{
		error(parser, peek(parser), "Expected statement for function body");
		return NULL;
	}

	stmt_t *stmt = allocStatement(&parser->alloc);
	stmt->type = STMT_FUNCTION;
	stmt->function.name = name;
	stmt->function.type = type;
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
}
static stmt_t* parseBlockStatement(parser_t *parser)
{
	stmt_t *stmt = allocStatement(&parser->alloc);
	stmt->type = STMT_BLOCK;

	stmtList_t *statements = &stmt->block.statements;
	while (!check(parser, TOKEN_CLOSE_BRACE) && !isAtEnd(parser))
	{
		pushStmt(statements, parseDeclaration(parser));
	};
	if (!consume(parser, TOKEN_CLOSE_BRACE, "Expected closing brace for block statement"))
		return NULL;
	return stmt;
};
static stmt_t* parseForStatement(parser_t *parser)
{
	if (!consume(parser, TOKEN_OPEN_PAREN, "Expected opening parenthesis")) return NULL;
	// Parse the initializer statement
	stmt_t *initializer;
	{
		const tokenType_t types_empty[] = { TOKEN_SEMICOLON };
		const tokenType_t types_var[] = { TOKEN_LET };
		if (match(parser, types_empty, static_len(types_empty)))
		{
			initializer = NULL;
		} else if (match(parser, types_var, static_len(types_var))) {
			initializer = parseVariableDeclaration(parser);
		} else {
			initializer = parseExpressionStatement(parser);
		}
	}
	// Parse the conditional expression
	expr_t *condition = NULL;
	if (!check(parser, TOKEN_SEMICOLON))
	{
		condition = parseExpression(parser);
	};
	if (!consume(parser, TOKEN_SEMICOLON, "Expected ';' after loop condition")) return NULL; 
	// Parse the incremental expression
	expr_t *increment = NULL;
	if (!check(parser, TOKEN_SEMICOLON))
	{
		increment = parseExpression(parser);
	};
	if (!consume(parser, TOKEN_CLOSE_PAREN, "Expected closing parenthesis")) return NULL;
	// Get the body
	stmt_t *body = parseStatement(parser);
	if (!body)
	{
		token_t last = peekPrev(parser);
		error(parser, last, "Expected body for for statement");
		return NULL;
	};
	// De-sugarization
	// TODO: Do we need to do this?
	// Add the increment (if it exists)
	if (increment != NULL)
	{
		stmt_t *stmt = allocStatement(&parser->alloc);
		stmt->type = STMT_BLOCK;
		stmtList_t *statements = &stmt->block.statements;
		// Add the body statement 
		pushStmt(statements, body);
		// Add the increment statement
		stmt_t *inc_stmt = allocStatement(&parser->alloc);
		inc_stmt->type = STMT_EXPR;
		inc_stmt->expression.expr = increment;
		pushStmt(statements, inc_stmt);
		// Swap the old body with the new one
		body = stmt;
	};
	// Add the condition (or make an infinate loop if there isn't one)
	{
		// Without a given condition, it is considered to be an infinate loop
		if (condition == NULL)
		{
			// Make a 'true' token
			token_t token = {};
			token.type = TOKEN_TRUE;
			// Make a new condition expression as a literal 'true'
			condition = allocExpression(&parser->alloc);
			condition->type = EXPR_LITERAL;
			condition->literal.value = token;
		}
		// Add a while statement with the conditional and the body
		stmt_t *stmt = allocStatement(&parser->alloc);
		stmt->type = STMT_WHILE;
		stmt->whileLoop.condition = condition;
		stmt->whileLoop.body = body;
		// Swap the old body with the new while loop
		body = stmt;
	}
	// Add the initializer (if there is one)
	if (initializer != NULL)
	{
		stmt_t *stmt = allocStatement(&parser->alloc);
		stmt->type = STMT_BLOCK;
		stmtList_t *statements = &stmt->block.statements;
		// Add the initializer statement
		pushStmt(statements, initializer);
		// Add the body statement 
		pushStmt(statements, body);
		// Swap the old body with the new one
		body = stmt;
	};
	return body;
}
static stmt_t* parseWhileStatement(parser_t *parser)
{
	if (!consume(parser, TOKEN_OPEN_PAREN, "Expected opening parenthesis")) return NULL;
	expr_t *condition = parseExpression(parser);
	if (!consume(parser, TOKEN_CLOSE_PAREN, "Expected closing parenthesis")) return NULL;

	stmt_t *body = parseStatement(parser);
	if (!body)
	{
		token_t last = peekPrev(parser);
		error(parser, last, "Expected body for while statement");
		return NULL;
	};

	stmt_t *stmt = allocStatement(&parser->alloc);
	stmt->type = STMT_WHILE;
	stmt->whileLoop.condition = condition;
	stmt->whileLoop.body = body;
	return stmt;
};
static stmt_t* parseIfStatement(parser_t *parser)
{
	// TODO: Do we want c-like if statements?
	if (!consume(parser, TOKEN_OPEN_PAREN, "Expected opening parenthesis")) return NULL;
	expr_t *condition = parseExpression(parser);
	if (!consume(parser, TOKEN_CLOSE_PAREN, "Expected closing parenthesis")) return NULL;

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

	stmt_t *stmt = allocStatement(&parser->alloc);
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
		value = parseExpression(parser);
	};
	if (!consume(parser, TOKEN_SEMICOLON, "Expected ';' after return statement"))
		return NULL;

	stmt_t *stmt = allocStatement(&parser->alloc);
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

	// Two megabytes is enough for everybody, right?
	const size_t size = megabytes(2); 
	u8 *memory = malloc(size);

	initLinAlloc(&parser->alloc, size, memory);

	#if 0
	for (u32 i = 0; i < tokens->used; i++)
	{
		const token_t *token = tokens->data + i;
		printToken(token, true);
	}
	#else
	stmt_t *stmt = parseDeclaration(parser);
	while (stmt)
	{
		printStatement(stmt, 0);
		pushStmt(&parser->statements, stmt);
		stmt = parseDeclaration(parser);
	};
	#endif
	return PARSER_NO_ERROR;
}
void freeParser(parser_t *parser)
{
	freeStmtList(&parser->statements);
	free(parser->alloc.memory);
};