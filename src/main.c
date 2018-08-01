#include <stdio.h>
#include <stdlib.h>

#include "core.h"
#include "util.h"

#include "tests.h"

#include "compiler.h"

static void help()
{
	printf("Help\n");
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
		}
	} else {
		run_tests();
	}
	return result;
}