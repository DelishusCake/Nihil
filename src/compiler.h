#ifndef COMPILER_H
#define COMPILER_H

#include "core.h"
#include "util.h"

#include "lexer.h"
#include "parser.h"

bool compile(const char *code, size_t code_size);

#endif