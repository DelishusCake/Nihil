#ifndef PARSER_H
#define PARSER_H

#include "core.h"
#include "util.h"

#include "lexer.h"

#include "expr.h"
#include "stmt.h"

// Maximum number of arguments that can be passed to a single function call 
#define MAX_ARGUMENTS		64

/* Recursive descent parser */
typedef enum
{
	PARSER_NO_ERROR,
	PARSER_ERROR,
} parserError_t;
typedef struct
{
	u32 current;
	const char *code;
	const arrayOf(token_t) *tokens;

	parserError_t error;
	stmtList_t statements;
} parser_t;

parserError_t parse(parser_t *parser, const char *code, const arrayOf(token_t) *tokens);
void freeParser(parser_t *parser);

#endif