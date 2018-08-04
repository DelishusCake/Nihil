#include <stdio.h>
#include <stdlib.h>

#include "core.h"
#include "util.h"

#include "tests.h"

#include "lexer.h"
#include "parser.h"

static void help()
{
	printf("Help\n");
};

static bool compile(const char *code, size_t code_size)
{
	bool result = false;

	// Tokenize the string
	arrayOf(token_t) tokens = {};
	if (tokenize(code, code_size, &tokens) == TOKENIZE_NO_ERROR)
	{
		// Parse the token stream
		parser_t parser = {};
		if(parse(&parser, code, &tokens) == PARSER_NO_ERROR)
		{
			// Free the parser
			freeParser(&parser);
		};
		// Free the token list
		freeArray(token_t, &tokens);
	}
	return result;
};

int main(int argc, const char *argv[])
{
	int result = 0;
	if (argc > 1)
	{
		const char *filename = argv[1];

		size_t code_size = 0;
		char *code = (char*) loadEntireFile(filename, &code_size);
		if (code)
		{
			if (!compile(code, code_size))
			{
				result = 1;
			}
			free(code);
		} else {
			printf("Failed to load function \"%s\"\n", filename);
		}
	} else {
		run_tests();
	}
	return result;
}