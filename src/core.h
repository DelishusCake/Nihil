#ifndef CORE_H
#define CORE_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <float.h>
#include <math.h>

#include <assert.h>

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t  i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef float  f32;
typedef double f64;

#define kilobytes(n)	(n<<10)
#define megabytes(n)	(n<<20)
#define gigabytes(n)	(n<<30)

#define U8_MAX		0xFF
#define U16_MAX		0xFFFF
#define U32_MAX		0xFFFFFFFF
#define U64_MAX		0xFFFFFFFFFFFFFFFF

#define PI_32		3.14159f

#define radians(f)	((f) * (PI_32 / 180.f))
#define degrees(f)	((f) * (180.f / PI_32))

#define sign(v)	(((v) < 0) ? -1 : (((v) > 0) ? 1 : 0))

#define static_len(a) (sizeof(a) / sizeof((a)[0]))

#undef max
#define max(a,b) \
	({	__typeof__ (a) _a = (a); \
		__typeof__ (b) _b = (b); \
		_a > _b ? _a : _b; })
#undef min
#define min(a,b) \
	({	__typeof__ (a) _a = (a); \
		__typeof__ (b) _b = (b); \
		_a < _b ? _a : _b; })

#define clamp(v, l, h)	max(l, min(v, h))

#define swap(TYPE, A, B) { TYPE tmp = A; A = B; B = tmp; }

#define membersize(TYPE, MEMBER) sizeof(((TYPE *)0)->MEMBER)

#define heap_parent(i)	((i - 1) >> 1)
#define heap_left(i)	((i << 1) + 1)
#define heap_right(i)	((i << 1) + 2)

#endif