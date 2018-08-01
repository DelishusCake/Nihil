#include "lexer.h"

#define MAX_HASH_INDICES		16
#define MAX_KEYWORDS_PER_LIST	8

typedef struct
{
	size_t len;
	u32 hash[MAX_KEYWORDS_PER_LIST];
	tokenType_t type[MAX_KEYWORDS_PER_LIST];
} keywordList_t;
typedef struct
{
	keywordList_t lists[MAX_HASH_INDICES];
} keywordHash_t;

typedef struct
{
	size_t current;				// Current character
	size_t line, line_offset;	// Line and line offset for the current char
	
	size_t code_len;			// Size of the code string to process
	const char *code;			// String of code to process

	u32 tokenUsed;				// Current number of tokens used
	u32 tokenSize;				// Number of total tokens available to use
	token_t *tokens;			// List of tokens

	keywordHash_t *keywordHash;	// Hash list for resolving keywords
} lexer_t;

typedef struct
{
	char  *str;
	tokenType_t type;
} keyword_t;

// Global hash seed
static const u32 g_hashSeed = 0xDEADBEEF;
// Global list of keywords and accociated token types
static keyword_t g_keywords[] =
{
	// Keywords
	{ "let", TOKEN_LET },
	{ "var", TOKEN_VAR },
	{ "if", TOKEN_IF },
	{ "else", TOKEN_ELSE },
	{ "true", TOKEN_TRUE },
	{ "false", TOKEN_FALSE },
	{ "for", TOKEN_FOR },
	{ "while", TOKEN_WHILE },
	{ "nil", TOKEN_NIL },
	{ "extern", TOKEN_EXTERN },
	{ "return", TOKEN_RETURN },
	{ "ref", TOKEN_REF },
	{ "struct", TOKEN_STRUCT },
	{ "union", TOKEN_UNION },
	{ "enum", TOKEN_ENUM },
	// Built in types
	{ "u8", TOKEN_U8 },
	{ "u16", TOKEN_U16 },
	{ "u32", TOKEN_U32 },
	{ "u64", TOKEN_U64 },
	{ "i8", TOKEN_I8 },
	{ "i16", TOKEN_I16 },
	{ "i32", TOKEN_I32 },
	{ "i64", TOKEN_I64 },
	{ "f32", TOKEN_F32 },
	{ "f64", TOKEN_F64 },
	{ "char", TOKEN_CHAR },
	{ "bool", TOKEN_BOOL },
	{ "ptr", TOKEN_PTR },
};
static bool buildKeywordHash(keywordHash_t *keywordHash)
{
	for (u32 i = 0; i < static_len(g_keywords); i++)
	{
		// Get the keyword, hash it, and convert the hash to an index
		const keyword_t *keyword = g_keywords + i;
		// Hash the value
		u32 hash = 0;
		MurmurHash3_x86_32(keyword->str, strlen(keyword->str), g_hashSeed, &hash);
		// Get the index into the hash table
		const u32 index = hash % MAX_HASH_INDICES;

		#if 0
		printf("[DEBUG] :: Hashed %s -> %d : index %d\n", keyword->str, hash, index);
		#endif

		keywordList_t *list = keywordHash->lists + index;
		if ((list->len + 1) < MAX_KEYWORDS_PER_LIST)
		{
			#if DEBUG
			// NOTE: Only for debug, checking hash collisions
			for (u32 i = 0; i < list->len; i++)
			{
				if (list->hash[i] == hash)
				{
					printf("[ERROR] :: Hash collision! [%d:%d] \n", list, i);
					return false;
				}
			};
			#endif
			// Insert the hash into the list
			const u32 l = list->len++; 
			list->hash[l] = hash;
			list->type[l] = keyword->type;
		} else {
			#if DEBUG
			printf("[ERROR] :: Too many keywords in index %d\n", index);
			return false;
			#endif
		}
	};
	return true;
};
static tokenType_t getIdentifierType(lexer_t *lexer, 
	size_t start, size_t len)
{
	// Get the string parameters
	const size_t str_len = len;
	const char *str = (lexer->code + start);

	// Hash string and convert to index
	u32 hash = 0;
	MurmurHash3_x86_32(str, str_len, g_hashSeed, &hash);
	const u32 index = hash % MAX_HASH_INDICES;
	
	// Linear search the list to find any keyword matches
	const keywordHash_t *keywordHash = lexer->keywordHash;
	const keywordList_t *list = keywordHash->lists + index;
	for (u32 i = 0; i < list->len; i++)
	{
		// Match found
		if (list->hash[i] == hash)
		{
			// Return the keyword type
			return list->type[i];
		}
	}
	// No match found, ergo user-defined identifier
	return TOKEN_IDENTIFIER;
};

// Simple bounds check for the lexer
static inline bool isAtEnd(lexer_t *lexer)
{
	return (lexer->current >= lexer->code_len);
}
// Advance the lexer by one character
static inline char advance(lexer_t *lexer)
{
	char value = '\0'; // Default to NULL char
	if ((lexer->current + 1) <= lexer->code_len)
	{
		value = lexer->code[lexer->current];
		lexer->current ++;
		lexer->line_offset ++;
	}
	return value;
};
// Advance the internal lexer state to a new line
static inline void nextLine(lexer_t *lexer)
{
	lexer->line_offset = 0;
	lexer->line ++;
};
// Get the current character in the stream without advancing the stream
static inline char peek(lexer_t *lexer)
{
	if (isAtEnd(lexer)) return '\0';
	return lexer->code[lexer->current];
};
// Get the next character in the stream without advancing the stream
static inline char peekNext(lexer_t *lexer)
{
	if ((lexer->current + 1) >= lexer->code_len)
		return '\0';
	return lexer->code[lexer->current+1];
};
// Advance the stream iff the current character matches the expected value
static inline bool match(lexer_t *lexer, char expected)
{
	if (isAtEnd(lexer))
		return false;
	if (peek(lexer) != expected)
		return false;
	lexer->current++;
	return true;
};

// Get the next available token in the list, returns NULL if no available tokens
static token_t* getNextToken(lexer_t *lexer)
{
	// Returned token
	token_t *token = NULL;
	// If we are in "count-mode", that is, only counting how many tokens there are, not actually adding them
	if (lexer->tokenSize == 0)
	{
		// Count it
		lexer->tokenUsed ++;
	} else {
		// Check if we have any tokens left
		if ((lexer->tokenUsed + 1) <= lexer->tokenSize)
		{
			token = lexer->tokens + lexer->tokenUsed;
			zeroMemory(token, sizeof(token_t));
			lexer->tokenUsed ++;
		} else {
			printf("[INTERNAL ERROR] :: Not enought tokens\n");
		}
	}
	return token;
};
// Add a token to the list with no associated string
static void addTokenNoValue(lexer_t *lexer, tokenType_t type)
{
	token_t* token = getNextToken(lexer);
	if (token)
	{
		token->type = type;
		token->start = token->len = 0;
		token->line = lexer->line;
		token->line_offset = lexer->line_offset;
	}
};

// Eat all numeric characters
static void eatDigits(lexer_t *lexer)
{
	while (charIsDigit(peek(lexer)))
		advance(lexer);
};
// Eat all whitespace characters
static void eatWhitespace(lexer_t *lexer)
{
	while (charIsWhitespace(peek(lexer)))
		advance(lexer);
};

// Parses a multi-line comment, handles nested comments 
static bool parseMultilineComments(lexer_t *lexer)
{
	const size_t start_line = lexer->line;
	const size_t start_line_offset = lexer->line_offset-1;

	i32 count = 1;
	while (!isAtEnd(lexer))
	{
		if (peek(lexer) == '\n')
			nextLine(lexer);
		if (match(lexer, '/') && match(lexer, '*'))
		{
			// Increment the count of nested comments
			count ++;
			continue;
		}
		if (match(lexer, '*') && match(lexer, '/'))
		{
			// Decrement the count of nested comments
			count --;
			// If we've reached the end of the expected comment nest, break out of the loop
			if (count == 0)
				break;
		}
		advance(lexer);
	}
	if (isAtEnd(lexer) && (count != 0))
	{
		printf("ERROR [%d:%d] :: Unterminated comment\n", start_line, start_line_offset);
		return false;
	}
	return true;
};
// Parses a string literal and adds it to the token list
static bool parseString(lexer_t *lexer)
{
	const size_t start = lexer->current;
	const size_t start_line = lexer->line;
	const size_t start_line_offset = lexer->line_offset-1;

	while ((peek(lexer) != '\"') && !isAtEnd(lexer))
	{
		if (peek(lexer) == '\n')
			nextLine(lexer);
		advance(lexer);
	};

	// Unterminated string
	if (isAtEnd(lexer))
	{
		printf("ERROR [%d:%d] :: Unterminated string\n", start_line, start_line_offset);
		return false;
	};

	const size_t end = lexer->current;
	advance(lexer); // eat the closing \"

	token_t *token = getNextToken(lexer);
	if (token)
	{
		token->type = TOKEN_STRING;
		token->start = start;
		token->len = (end - start);
		token->line = start_line;
		token->line_offset = start_line_offset;
	}
	return true;
};
// Parses integers and floating point literals and adds it to the token list
static bool parseNumber(lexer_t *lexer)
{
	const size_t start = lexer->current-1;
	const size_t start_line = lexer->line;
	const size_t start_line_offset = lexer->line_offset-1;
	// Eat all digits
	eatDigits(lexer);
	// If we've got a dot at the end, it's a float of some kind
	if (match(lexer, '.'))
	{
		// Eat digits after the dot too
		eatDigits(lexer);
	}
	// Calculate the length of the string
	const size_t len = (lexer->current - start);

	// Write the token
	token_t *token = getNextToken(lexer);
	if (token)
	{
		token->type = TOKEN_NUMBER;
		token->start = start;
		token->len = len;
		token->line = start_line;
		token->line_offset = start_line_offset;
	}
	return true;
};
// Parses identifiers and keywords, adds them to the token list
static bool parseIdentifier(lexer_t *lexer)
{
	const size_t start = lexer->current-1;
	const size_t start_line = lexer->line;
	const size_t start_line_offset = lexer->line_offset-1;

	// Eat all alphanumeric characters (and underscores)
	while (charIsAlpha(peek(lexer)) || charIsDigit(peek(lexer)) || (peek(lexer) == '_'))
		advance(lexer);

	const size_t end = lexer->current;
	const size_t len = (end-start);

	// Check if the identifier is a keyword
	const tokenType_t type = getIdentifierType(lexer, start, len);

	token_t *token = getNextToken(lexer);
	if (token)
	{
		token->start = start;
		token->len = len;
		token->type = type;
		token->line = start_line;
		token->line_offset = start_line_offset;
	}
	return true;
};
// Parses a single token from the character stream
static bool parseToken(lexer_t *lexer)
{
	// Eat whitespace before continuing
	eatWhitespace(lexer);

	// Advance by a single character
	char c = advance(lexer);
	switch (c)
	{
		// Handle end of stream
		case '\0':	return true;
		// Handle newlines
		case '\n':	nextLine(lexer); break;
		// Single character tokens
		case '(':	addTokenNoValue(lexer, TOKEN_OPEN_PAREN); break;
		case ')':	addTokenNoValue(lexer, TOKEN_CLOSE_PAREN); break;
		case '{':	addTokenNoValue(lexer, TOKEN_OPEN_BRACE); break;
		case '}':	addTokenNoValue(lexer, TOKEN_CLOSE_BRACE); break;
		case ',':	addTokenNoValue(lexer, TOKEN_COMMA); break;
		case '.':	addTokenNoValue(lexer, TOKEN_DOT); break;
		case ';':	addTokenNoValue(lexer, TOKEN_SEMICOLON); break;
		case '*':	addTokenNoValue(lexer, TOKEN_STAR); break;
		// Single/double character tokens
		case '-':
		{
			addTokenNoValue(lexer,
				match(lexer, '>') ? TOKEN_ARROW : 
				match(lexer, '-') ? TOKEN_MINUS_MINUS : 
				TOKEN_MINUS);
		} break;
		case ':':
		{
			addTokenNoValue(lexer, 
				match(lexer, ':') ? TOKEN_COLON_COLON : 
				match(lexer, '=') ? TOKEN_COLON_EQUAL : 
				TOKEN_COLON);
		} break;
		case '|':	addTokenNoValue(lexer, match(lexer, '|') ? TOKEN_OR_OR : TOKEN_OR); break;
		case '&':	addTokenNoValue(lexer, match(lexer, '&') ? TOKEN_AND_AND : TOKEN_AND); break;
		case '+':	addTokenNoValue(lexer, match(lexer, '+') ? TOKEN_PLUS_PLUS : TOKEN_PLUS); break;
		case '!':	addTokenNoValue(lexer, match(lexer, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG); break;
		case '=':	addTokenNoValue(lexer, match(lexer, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL); break;
		case '<':	addTokenNoValue(lexer, match(lexer, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS); break;
		case '>':	addTokenNoValue(lexer, match(lexer, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER); break;
		// Possible comment
		case '/':
		{
			// Multiline comment
			if (match(lexer, '*'))
			{
				if (!parseMultilineComments(lexer))
					return false;
			// Single line comment
			} else if (match(lexer, '/')) {
				while ((peek(lexer) != '\n') && !isAtEnd(lexer))
					advance(lexer);
			} else {
				addTokenNoValue(lexer, TOKEN_SLASH);
			}
		} break;
		// String literals
		case '\"':
		{
			if (!parseString(lexer))
				return false;
		} break;

		default:
		{	
			// Check for identifiers (and by extention, keywords)
			if (charIsAlpha(c) || (c == '_'))
			{
				if (!parseIdentifier(lexer))
					return false;
			// Check for numeric literals
			} else if (charIsDigit(c)) {
				if (!parseNumber(lexer))
					return false;
			// Unknown characters
			}else{
				printf("ERROR [%d:%d] :: Unknown character \'%c\'\n", lexer->line, lexer->line_offset, c);
				return false;
			}
		} break;
	};
	return true;
};

i32 tokenize(const char *code, size_t code_len, 
	token_t *tokens, u32 maxTokens)
{
	lexer_t lexer = {};
	lexer.current = 0;

	lexer.line = 1;
	lexer.line_offset = 0;
	
	lexer.code = code;
	lexer.code_len = code_len;

	lexer.tokenUsed = 0;
	lexer.tokenSize = maxTokens;
	lexer.tokens = tokens;

	lexer.keywordHash = malloc(sizeof(keywordHash_t));
	if (lexer.keywordHash)
	{
		zeroMemory(lexer.keywordHash, sizeof(keywordHash_t));
		if (buildKeywordHash(lexer.keywordHash))
		{
			while (!isAtEnd(&lexer))
			{
				if (!parseToken(&lexer))
					return -1;
			};
		}
		free(lexer.keywordHash);
	}

	token_t *eof = getNextToken(&lexer);
	if (eof)
	{
		eof->type = TOKEN_EOF;
	}

	return lexer.tokenUsed;
};