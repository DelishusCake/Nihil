/* WARNING: This file is auto-generated. Do not modify */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <float.h>
/* Function prototypes */
int32_t main(void);
void do_print(uint32_t const lim);
void * return_nil(void);
/* Code */
int32_t main(void)
{
	do_print(100);
	int32_t n = 1;
	int32_t * const r = &n;
	int32_t * const * const p = &r;
	int32_t m = 32;
	uint32_t const k = (uint32_t const)(m+m);
	bool const b = false;
	if (!b||!return_nil())
	{
		*r = 4+4;
		printf("\n%d %d\n", *r, **p);
		printf("Hi!\n");
		return 0;
	}
	printf("\n%d %d\n", *r, **p);
	printf("Hi!\n");
	return 1;
}
void do_print(uint32_t const lim)
{
	{
		uint32_t i = 0;
		while(i<=lim)
		{
			{
				printf("%d ", i);
			}
			i++;
		}
	}
	printf("\n");
}
void * return_nil(void)
{
	return NULL;
}
