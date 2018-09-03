#ifndef SCOPE_H
#define SCOPE_H

#include <murmur3.h>

#include "core.h"
#include "expr.h"

struct scopeBlock_s;
typedef struct scopeBlock_s scopeBlock_t;

// The stack of all scopes. The top is the current scope, the bottom is the global scope
typedef struct
{
	u32 used; // Number of scopes in the stack
	u32 size; // Size of the blocks array
	// The actual stack itself. blocks[used-1] is the top of the stack.
	scopeBlock_t** blocks;
} scopeStack_t;

// Scope stack allocation
void initScopeStack(scopeStack_t *stack);
void freeScopeStack(scopeStack_t *stack);
// Scope control
void pushScopeBlock(scopeStack_t *stack);
void popScopeBlock(scopeStack_t *stack);
// Entry insertion. Entries are always inserted into the topmost block of the stack.
void insertVar(scopeStack_t *stack, token_t name, expr_t *type);
/* 
Entry type checking. 
Returns the topmost found entry's type, searching down the stack until an entry is found or NULL if no entry exists
*/
expr_t* getVarType(scopeStack_t *stack, token_t name);

// A stack of defered expressions
typedef struct 
{
	u32 used;
	u32 size;
	expr_t **expressions;
} deferStack_t;
// Memory management
void initDeferStack(deferStack_t *stack);
void freeDeferStack(deferStack_t *stack);
void resetDeferStack(deferStack_t *stack);
// Push a defered expression
void pushDeferedExpr(deferStack_t *stack, expr_t *expr);

#endif