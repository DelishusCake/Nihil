#include "scope.h"

#define SCOPE_BLOCK_SIZE 	128
#define SCOPE_HASH_SEED		0xDEADBEEF

// List that occupies one slot of a scope hash list
typedef struct
{
	u32 used; // Number of entries in the list 
	u32 size; // Size of the entry array
	// Entry array, search is usually on the name, so keep 'em separate
	token_t *names;
	expr_t **types;
} scopeList_t;
// One slot in the scope stack. Hash list
struct scopeBlock_s
{
	// Slots for the hash list
	scopeList_t lists[SCOPE_BLOCK_SIZE];
};

static scopeBlock_t* allocScopeBlock()
{
	scopeBlock_t *block = malloc(sizeof(scopeBlock_t));
	assert(block);
	// Allocate all the lists
	for (u32 i = 0; i < SCOPE_BLOCK_SIZE; i++)
	{
		scopeList_t *list = block->lists + i;
		list->used = 0;
		list->size = 1; // These lists may or may not have anything in them ever, so keep it small
		list->names = malloc(list->size*sizeof(token_t)); assert(list->names);
		list->types = malloc(list->size*sizeof(expr_t*)); assert(list->types);
	};
	return block;
};
static void freeScopeBlock(scopeBlock_t *block)
{
	assert(block);
	// Free all of the lists
	for (u32 i = 0; i < SCOPE_BLOCK_SIZE; i++)
	{
		scopeList_t *list = block->lists + i;
		free(list->names);
		free(list->types);
	};
	// Free the block itself
	free(block);
};
static void insertIntoScopeBlock(scopeBlock_t *block, token_t name, expr_t *type)
{
	// Hash string and convert to index
	u32 hash = 0;
	MurmurHash3_x86_32(name.start, name.len, SCOPE_HASH_SEED, &hash);
	const u32 listIndex = hash % SCOPE_BLOCK_SIZE;
	
	scopeList_t *list = block->lists + listIndex;
	// If there's no space, realloc the list data
	if ((list->used + 1) >= list->size)
	{
		list->size <<= 1; // double it
		list->names = realloc(list->names, list->size*sizeof(token_t)); assert(list->names);
		list->types = realloc(list->types, list->size*sizeof(expr_t*)); assert(list->types);
	};
	// Insert into the list
	const u32 index = list->used ++;
	list->names[index] = name;
	list->types[index] = type;
};
static expr_t* getInScopeBlock(scopeBlock_t *block, token_t name)
{
	/*
	TODO: Can we get away with just checking the token hashes?
	Right now we check every token in the list with a string test, which prevents
	collisions for tokens that evaluate to the same hash, but is O(n*m) (I think).
	*/ 
	// Hash string and convert to index
	u32 hash = 0;
	MurmurHash3_x86_32(name.start, name.len, SCOPE_HASH_SEED, &hash);
	const u32 listIndex = hash % SCOPE_BLOCK_SIZE;
	// Get the list
	scopeList_t *list = block->lists + listIndex;
	for (u32 i = 0; i < list->used; i++)
	{
		// Check the strings
		if (stringTest(
			list->names[i].start, list->names[i].len,
			name.start, name.len))
			return list->types[i];
	};
	return NULL;
};

// Scope stack allocation
void initScopeStack(scopeStack_t *stack)
{
	stack->used = 0;
	stack->size = 16; // Default size of 16
	stack->blocks = malloc(stack->size*sizeof(scopeBlock_t*));
	assert(stack->blocks);
};
void freeScopeStack(scopeStack_t *stack)
{
	// Free the blocks
	for (u32 i = 0; i < stack->used; i++)
	{
		freeScopeBlock(stack->blocks[i]);
	};
	// Free the block array
	free(stack->blocks);
};

void pushScopeBlock(scopeStack_t *stack)
{
	if ((stack->used+1) >= stack->size)
	{
		stack->size <<= 1; // double it
		stack->blocks = realloc(stack->blocks, stack->size*sizeof(scopeBlock_t*));
		assert(stack->blocks);
	};
	
	const u32 index = stack->used++;
	stack->blocks[index] = allocScopeBlock();
};
void popScopeBlock(scopeStack_t *stack)
{
	const u32 index = --stack->used;
	free(stack->blocks[index]);
};

void insertVar(scopeStack_t *stack, token_t name, expr_t *type)
{
	// Get the top scope block
	const u32 top = stack->used-1;
	scopeBlock_t *block = stack->blocks[top];
	assert(block);
	insertIntoScopeBlock(block, name, type);
};
expr_t* getVarType(scopeStack_t *stack, token_t name)
{
	// Search from the top down
	for (u32 i = stack->used; i > 0; i--)
	{
		// Get the block
		scopeBlock_t *block = stack->blocks[i-1];
		assert(block);
		// Check
		expr_t* type = getInScopeBlock(block, name);
		if (type) 
			return type;
	}
	// Not found
	return NULL;
}

void initDeferStack(deferStack_t *stack)
{
	stack->used = 0;
	stack->size = 1;
	stack->expressions = malloc(stack->size*sizeof(expr_t*));
};
void freeDeferStack(deferStack_t *stack)
{
	free(stack->expressions);
};

void pushDeferedExpr(deferStack_t *stack, expr_t *expr)
{
	if ((stack->used+1) >= stack->size)
	{
		stack->size <<= 1;
		stack->expressions = realloc(stack->expressions, stack->size*sizeof(expr_t*));
	};

	const u32 index = stack->used ++;
	stack->expressions[index] = expr;
};