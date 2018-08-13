#include "buffer.h"

void allocBuffer(buffer_t *buffer, size_t initial)
{
	buffer->used = 0;
	buffer->size = initial;
	buffer->data = malloc(initial*sizeof(char));
	assert(buffer->data);
};
void freeBuffer(buffer_t *buffer)
{
	buffer->used = buffer->size = 0;
	free(buffer->data);
};
#if 0
void printBuffer(buffer_t *buffer)
{
	puts(buffer->data);
};
#endif
void saveBuffer(buffer_t *buffer, const char *filename)
{
	FILE *f = fopen(filename, "wb");
	if (f)
	{
		fputs(buffer->data, f);
		fclose(f);
	} else {
		printf("[ERROR] :: Failed to open \"%s\" for writing\n", filename);
	}
};

static char* pushBytes(buffer_t *buffer, size_t len)
{
	if ((buffer->used + len) > buffer->size)
	{
		buffer->size <<= 1;
		buffer->data = realloc(buffer->data, buffer->size*sizeof(char));
		assert(buffer->data);
	};
	char *ptr = buffer->data + buffer->used;
	buffer->used += len;
	return ptr;
};
void writeStringLen(buffer_t *buffer, const char *str, size_t len)
{
	char *ptr = pushBytes(buffer, len*sizeof(char));
	copyMemory(ptr, str, len*sizeof(char));
};
void writeString(buffer_t *buffer, const char *str)
{
	const size_t len = strlen(str);
	writeStringLen(buffer, str, len);
};
void writeChar(buffer_t *buffer, char c)
{
	writeStringLen(buffer, &c, 1);
};
void writeEOF(buffer_t *buffer)
{
	writeChar(buffer, '\0');
};