#ifndef INTRIN_H
#define INTRIN_H

#include <xmmintrin.h>

#include "core.h"

/* Compiler detection */
#if defined(__GNUC__)
	#define GCC 1
#else
	#define GCC 0
#endif
#if defined(_MSC_VER)
	#define MSVC 1
#else
	#define MSVC 0
#endif

/* Atomic functions */
static inline u32 atomicIncrementU32(volatile u32 *target)
{
	/*
	Increment a value in a locked way (prevents race conditions)
	const u32 v = *target;
	*target = v + 1;
	return v;
	*/
#if GCC
	return __sync_fetch_and_add(target, 1);
#elif MSVC
	return _InterlockedExchangeAdd(target, 1);
#endif
}
static inline u32 atomicAddU32(volatile u32 *target, u32 value)
{
	/*
	Add a value to the target in a locked way (prevents race conditions)
	const u32 v = *target;
	*target = v + value;
	return v;
	*/
#if GCC
	return __sync_fetch_and_add(target, value);
#elif MSVC
	return _InterlockedExchangeAdd(target, value);
#endif
};

#endif