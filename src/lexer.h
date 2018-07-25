#ifndef LEXER_H
#define LEXER_H

#include "core.h"
#include "util.h"

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
	TOKEN_MINUS,	TOKEN_MINUS_MINUS,
	TOKEN_ARROW,
	// Literals
	TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,
	// Keywords
	TOKEN_LET, TOKEN_VAR,
	TOKEN_TRUE, TOKEN_FALSE, TOKEN_NIL,
	TOKEN_IF, TOKEN_ELSE, 
	TOKEN_FOR, TOKEN_WHILE, 
	TOKEN_STRUCT, TOKEN_UNION,
	TOKEN_EXTERN, TOKEN_RETURN,
	// Built in types
	TOKEN_U8, TOKEN_U16, TOKEN_U32, TOKEN_U64,
	TOKEN_I8, TOKEN_I16, TOKEN_I32, TOKEN_I64,
	TOKEN_F32, TOKEN_F64,
	TOKEN_CHAR, TOKEN_BOOL,
} tokenType_t;
typedef struct
{
	tokenType_t type;			// Token type
	size_t start, len;			// Start and length for the token string
	size_t line, line_offset;	// Line and line offset for the token
} token_t;

i32 tokenize(const char *code, size_t code_len, 
	token_t *tokens, u32 maxTokens);

#endif