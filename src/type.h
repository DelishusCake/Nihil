#ifndef TYPE_H
#define TYPE_H

#include "core.h"
#include "util.h"

typedef enum
{
	TYPE_FLAG_NONE 	= 0,
	TYPE_FLAG_PTR	= (1 << 0), // Pointer type?
	TYPE_FLAG_VAR	= (1 << 1), // Variable type or constant?
} typeFlags_t;

typedef enum
{
	TYPE_CLASS_NONE,
	// Built-in types
	TYPE_CLASS_U8, TYPE_CLASS_U16, TYPE_CLASS_U32, TYPE_CLASS_U64,
	TYPE_CLASS_I8, TYPE_CLASS_I16, TYPE_CLASS_I32, TYPE_CLASS_I64,
	TYPE_CLASS_F32, TYPE_CLASS_F64,
	TYPE_CLASS_CHAR, TYPE_CLASS_BOOL,
	// User defined type
	TYPE_CLASS_USER
} typeClass_t;

struct type_s;
typedef struct type_s type_t;

struct type_s
{
	// The "type" of the type
	typeClass_t class;
	// The size and alignment of the type
	size_t size, align;
	// If the type is a pointer, this is the type it points to
	type_t *ptrTo;
	// Freelist pointers
	type_t *next, *prev;
};

#endif