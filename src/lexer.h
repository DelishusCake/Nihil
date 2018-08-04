#ifndef LEXER_H
#define LEXER_H

#include "core.h"
#include "util.h"

#include "murmur3.h"

typedef enum
{
	TOKEN_EOF,
	// Single character tokens
	TOKEN_OPEN_PAREN, TOKEN_CLOSE_PAREN,
	TOKEN_OPEN_BRACE, TOKEN_CLOSE_BRACE,
	TOKEN_COMMA, TOKEN_DOT, TOKEN_SEMICOLON,
	TOKEN_SLASH, TOKEN_STAR,
	// One/two char tokens
	TOKEN_BANG,		TOKEN_BANG_EQUAL,
	TOKEN_EQUAL,	TOKEN_EQUAL_EQUAL,
	TOKEN_GREATER,	TOKEN_GREATER_EQUAL,
	TOKEN_LESS,		TOKEN_LESS_EQUAL,
	TOKEN_COLON,	TOKEN_COLON_COLON, TOKEN_COLON_EQUAL,
	TOKEN_PLUS,		TOKEN_PLUS_PLUS,
	TOKEN_MINUS,	TOKEN_MINUS_MINUS, TOKEN_ARROW,
	TOKEN_AND,		TOKEN_AND_AND,
	TOKEN_OR,		TOKEN_OR_OR,
	// Literals
	TOKEN_IDENTIFIER, TOKEN_STRING, 
	TOKEN_INTEGER, TOKEN_FLOAT,
	// Keywords
	TOKEN_LET, TOKEN_VAR,
	TOKEN_TRUE, TOKEN_FALSE, TOKEN_NIL,
	TOKEN_IF, TOKEN_ELSE, 
	TOKEN_FOR, TOKEN_WHILE, 
	TOKEN_STRUCT, TOKEN_UNION, TOKEN_ENUM,
	TOKEN_EXTERN, TOKEN_RETURN, TOKEN_REF,
	// Built in types
	TOKEN_U8, TOKEN_U16, TOKEN_U32, TOKEN_U64,
	TOKEN_I8, TOKEN_I16, TOKEN_I32, TOKEN_I64,
	TOKEN_F32, TOKEN_F64,
	TOKEN_CHAR, TOKEN_BOOL,
	TOKEN_PTR,
} tokenType_t;
typedef struct
{
	tokenType_t type;	// Token type
	const char *start;	// The pointer into the code string of the token
	size_t len;			// Length for the token string
	size_t line;		// The line of code this token exists on
	size_t line_offset;	// The offset into the line this token is on
} token_t;

defineArrayOf(token_t);

// TODO: More descriptive errors
typedef enum
{
	TOKENIZE_NO_ERROR,
	TOKENIZE_ERROR,
} tokenizeError_t;

/* Tokenizes the code string */
tokenizeError_t tokenize(const char *code, size_t code_len, arrayOf(token_t) *tokens);

#endif