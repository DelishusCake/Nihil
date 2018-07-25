#ifndef UTIL_H
#define UTIL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "core.h"

void 	copyMemory(void *dst, const void *src, size_t size);
void 	zeroMemory(void *dst, size_t size);

u32		hashString(const char *string, size_t length);
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

#endif