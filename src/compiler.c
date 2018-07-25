#include "compiler.h"

bool compile(const char *code, size_t code_size)
{
	bool result = false;

	const i32 tokenCount = tokenize(code, code_size, NULL, 0);
	if (tokenCount > 0)
	{
		token_t *tokens = malloc(tokenCount*sizeof(token_t));
		if (tokens)
		{
			tokenize(code, code_size, tokens, tokenCount);
			parse(code, tokens, tokenCount);
			
			result = true;
		}
	}
	return result;
};