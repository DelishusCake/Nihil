#ifndef PARSER_H
#define PARSER_H

#include "core.h"
#include "util.h"

#include "lexer.h"

i32 parse(const char *code, const token_t *tokens, u32 tokenCount); 

#endif