#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <float.h>
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
void do_print(const u32 lim)
{
	{
		u32 i = 0;
		while(i<lim)
		{
			{
				printf("%d ", i);
			}
			i = i+1;
		}
	}
	printf("\n");
}
const i32 main()
{
	do_print(100);
	return 0;
}
