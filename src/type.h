#ifndef TYPE_H
#define TYPE_H

#include "core.h"
#include "expr.h"

#include "scope.h"

expr_t* evaluateExprType(expr_t *expr, const scopeStack_t *scope, typeFlags_t flags);
bool typeExpressionsMatch(expr_t *a, expr_t *b);

#endif