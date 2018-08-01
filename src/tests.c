#include "tests.h"

#define MUNIT_ENABLE_ASSERT_ALIASES
#include "munit.h"

#include "core.h"
#include "util.h"

#include "lexer.h"
#include "parser.h"

// Basic lexer test: string -> token_types[]
static MunitResult test_lex(const char *string, const tokenType_t *expected, u32 expected_count)
{
	token_t tokens[128];
	i32 count = tokenize(string, strlen(string), 
		tokens, static_len(tokens));
	// No errors expected
	assert_true(count > 0);
	// Expected the same number of tokens
	assert_int(count, ==, expected_count);
	// Tokens should be equal
	for (u32 i = 0; i < count; i++)
	{
		assert_int((i32) tokens[i].type, ==, (i32) expected[i]);
	};
	// Passed
	return MUNIT_OK;
};
static MunitResult test_lex_whitespace(const MunitParameter params[], void *data)
{
	const char *string = "\t\t\nlet\t\tn\n:=\r\r32.0\t;\n";
	const tokenType_t expected[] = { 
		TOKEN_LET, TOKEN_IDENTIFIER, TOKEN_COLON_EQUAL, TOKEN_NUMBER, TOKEN_SEMICOLON, 
		TOKEN_EOF
	};
	return test_lex(string, expected, static_len(expected));
};
static MunitResult test_lex_keywords(const MunitParameter params[], void *data)
{
	const char *string = "let lets letting letter return returning returns struct structs structure enum enumeration union unionize";
	const tokenType_t expected[] = { 
		TOKEN_LET, TOKEN_IDENTIFIER, TOKEN_IDENTIFIER, TOKEN_IDENTIFIER, 
		TOKEN_RETURN, TOKEN_IDENTIFIER, TOKEN_IDENTIFIER,
		TOKEN_STRUCT, TOKEN_IDENTIFIER, TOKEN_IDENTIFIER,
		TOKEN_ENUM, TOKEN_IDENTIFIER, 
		TOKEN_UNION, TOKEN_IDENTIFIER, 
		TOKEN_EOF
	};
	return test_lex(string, expected, static_len(expected));
};
static MunitResult test_lex_numbers(const MunitParameter params[], void *data)
{
	const char *string = "30.0 1231425 143234.032423 0.04324";
	const tokenType_t expected[] = { 
		// 30.0
		TOKEN_NUMBER,
		// 1231425
		TOKEN_NUMBER,
		// 143234.032423
		TOKEN_NUMBER,
		// 0.04324
		TOKEN_NUMBER,
		TOKEN_EOF
	};
	return test_lex(string, expected, static_len(expected));
};

int run_tests()
{
	MunitTest tests[] = 
	{
		// Name, func, setup, teardown, options, parameters
		// Lexer
		{ "/lex/whitespace", test_lex_whitespace, NULL, NULL, MUNIT_TEST_OPTION_NONE, NULL },
		{ "/lex/keywords", test_lex_keywords, NULL, NULL, MUNIT_TEST_OPTION_NONE, NULL },
		{ "/lex/numbers", test_lex_numbers, NULL, NULL, MUNIT_TEST_OPTION_NONE, NULL },
		{ NULL, NULL, NULL, NULL, MUNIT_TEST_OPTION_NONE, NULL }
	};

	const MunitSuite suite = 
	{
		// Name, tests, suites, iterations, options
		"nihil", tests, NULL, 1, MUNIT_SUITE_OPTION_NONE
	};
	return munit_suite_main(&suite,
		//User data 
		NULL,  
		// argc, argv
		0, NULL);
};