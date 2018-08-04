#ifndef PARSER_H
#define PARSER_H

#include "core.h"
#include "util.h"

#include "type.h"
#include "lexer.h"

i32 parse(const char *code, const arrayOf(token_t) *tokens); 

#endif