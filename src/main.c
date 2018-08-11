#include <stdio.h>
#include <stdlib.h>

#include "core.h"
#include "util.h"

#include "tests.h"

#include "lexer.h"
#include "parser.h"
#include "output_c.h"

static void help()
{
	printf("Help\n");
};

static bool compile(
	const char *code, size_t code_size,
	const char *output)
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
			output_c(&parser, output);

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
		const char *in_filename = argv[1];
		const char *out_filename = argv[2];

		size_t code_size = 0;
		char *code = (char*) loadEntireFile(in_filename, &code_size);
		if (code)
		{
			if (!compile(code, code_size, out_filename))
			{
				result = 1;
			}
			free(code);
		} else {
			printf("Failed to load function \"%s\"\n", in_filename);
		}
	} else {
		run_tests();
	}
	return result;
}