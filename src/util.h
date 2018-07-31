#ifndef UTIL_H
#define UTIL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "core.h"

void 	copyMemory(void *dst, const void *src, size_t size);
void 	zeroMemory(void *dst, size_t size);

bool	stringTest(
	const char *a, size_t a_length, 
	const char *b, size_t b_length);
void 	reverseString(char *string, size_t length);

size_t	intToStringLength(i32 value, i32 base);
void	intToString(i32 value, i32 base,
		char *string, size_t length);

bool	charIsDigit(char value);
bool	charIsAlpha(char value);
bool 	charIsWhitespace(char value);
i32 	stringToInt(const char *string, size_t size);

// TODO: Move this
u8* loadEntireFile(const char *path, size_t *size);

// Preprocessor macro for declaring an array of something, i.e. arrayOf_int_t
#define arrayOf(__type__)			arrayOf_##__type__##_t
// Preprocessor macro for pre-declaring an array of something
#define defineArrayOf(__type__)\
	typedef struct \
	{ \
		u32 used, size;\
		__type__ *data;\
	} arrayOf(__type__);
// Allocates a member of an array of something
#define arrayAlloc(__type__, array)	arrayAlloc_##__type__(array)
// Frees an array of something
#define freeArray(__type__, array)	freeArray_##__type__(array)
// Declares the interface for an array of something
#define declareArrayOf(__type__) \
	__type__* arrayAlloc_##__type__(arrayOf(__type__) *array) {\
		if (!array->data) {\
			array->used = 0;\
			array->size = 16;\
			array->data = malloc(array->size*sizeof(__type__));\
		};\
		if ((array->used + 1) >= array->size){\
			array->size <<= 1;\
			array->data = realloc(array->data, array->size*sizeof(__type__));\
		};\
		const u32 index = array->used++;\
		return (array->data + index);\
	};\
	void freeArray_##__type__(arrayOf(__type__) *array) {\
		if (array->data) free(array->data);\
	}

#endif