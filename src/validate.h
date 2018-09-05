#ifndef VALIDATE_H
#define VALIDATE_H

#include "core.h"
#include "util.h"

#include "expr.h"
#include "stmt.h"

#include "scope.h"
#include "type.h"

// Validates a global list of statements
bool validate(stmtList_t *statements);

#endif