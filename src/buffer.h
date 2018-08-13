#ifndef BUFFER_H
#define BUFFER_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "core.h"
#include "util.h"

typedef struct
{
	size_t used;
	size_t size;
	char *data;
} buffer_t;

void allocBuffer(buffer_t *buffer, size_t initial);
void saveBuffer(buffer_t *buffer, const char *filename);
void freeBuffer(buffer_t *buffer);

void writeStringLen(buffer_t *buffer, const char *str, size_t len);
void writeString(buffer_t *buffer, const char *str);
void writeChar(buffer_t *buffer, char c);
void writeEOF(buffer_t *buffer);

#endif