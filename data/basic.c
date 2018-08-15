/* WARNING: This file is auto-generated. Do not modify */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <float.h>
/* Function prototypes */
void * return_nil(void);
void do_print(uint32_t const lim);
int32_t main(void);
/* Code */
void * return_nil(void)
{
	return NULL;
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
int32_t main(void)
{
	do_print(100);
	int32_t n = 1;
	int32_t * const r = &n;
	int32_t * const * const p = &r;
	bool const b = false;
	if (!b||!return_nil())
	{
		*r = 4;
	}

	printf("\n%d %d\n", *r, **p);
	return 0;
}
