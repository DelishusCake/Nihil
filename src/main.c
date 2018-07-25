#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "core.h"
#include "util.h"

#include "lexer.h"
#include "parser.h"

static void help()
{
	printf("Help\n");
};

static int compile(const char *filename)
{
	token_t tokens[1024];

	int result = 0;

	size_t code_size = 0;
	char *code = (char*) loadEntireFile(filename, &code_size);
	if (code)
	{
		const clock_t lex_start = clock();
		const i32 error_or_count = tokenize(code, code_size,
			tokens, static_len(tokens));
		const clock_t lex_end = clock();

		#if DEBUG
		printf("[PERF] :: Lexed in %dms\n", (lex_end-lex_start)/(CLOCKS_PER_SEC/1000));
		#endif

		if (error_or_count > 0)
		{
			parse(code, tokens, error_or_count);
		} else {
			result = 1;
		}
		free(code);
	}else{
		printf("Failed to load file \"%s\"\n", filename);
		result = 1;
	}
	return result;
};

int main(int argc, const char *argv[])
{
	if (argc > 1)
	{
		const char *filename = argv[1];
		return compile(filename);
	} else {
		help();
	}
	return 0;
}