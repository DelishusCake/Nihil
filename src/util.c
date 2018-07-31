#include "util.h"

void copyMemory(void *dst, const void *src, size_t size)
{
	if ((((uintptr_t) dst % sizeof(u32)) == 0) &&
		(((uintptr_t) src % sizeof(u32)) == 0) &&
		((size % sizeof(u32)) == 0))
	{
		u32 *d = (u32*) dst;
		u32 *s = (u32*) src;
		size_t len = size / sizeof(u32);
		while (len--)
		{
			*d ++ = *s ++;
		}
	}else{
		u8 *d = (u8*) dst;
		u8 *s = (u8*) src;
		while (size--)
		{
			*d ++ = *s ++;
		}
	}
}
void zeroMemory(void *dst, size_t size)
{
	if ((((uintptr_t) dst % sizeof(u32)) == 0) &&
		((size % sizeof(u32)) == 0))
	{
		u32 *d = (u32*) dst;
		size_t len = size / sizeof(u32);
		while (len--)
		{
			*d++ = 0x00000000;
		}
	}else{
		u8 *d = (u8*) dst;
		while (size--)
		{
			*d++ = 0x00;
		}
	}
}

#if 0
u32	hashString(const char *string, size_t length)
{
	// DJB2 hashing function
	// http://www.cse.yorku.ca/~oz/hash.html
	const u8 *str = (const u8*) string;

	u32 hash = 5381;
	for (u32 i = 0; i < length; i++)
	{
		hash = ((hash << 5) + hash) + str[i];
	}
	return hash;
}
#endif

bool stringTest(
	const char *a, size_t a_length, 
	const char *b, size_t b_length)
{
	if (a_length != b_length)
	{
		return false;
	}
	for (u32 i = 0; i < a_length; i++)
	{
		if (a[i] != b[i])
			return false;
	}
	return true;
}
void reverseString(char *string, size_t length)
{
	for(u32 i = 0; i < (length >> 1); i++)
	{
		swap(char, string[i], string[length-i-1]);
	}
};

size_t intToStringLength(i32 value, i32 base)
{
	//length = number of characters in value + 1 for '-' if negative + 1 for null character
	const size_t value_chars = (size_t) ceil(log(abs(value)) / log(base));
	const size_t negative_char = ((base == 10) && (value < 0));
	return (value_chars + negative_char + 1);
};
void intToString(i32 value, i32 base,
	char *string, size_t maxSize)
{
	assert(maxSize >= intToStringLength(value, base));

	if (value == 0)
	{
		string[0] = '0';
		string[1] = '\0';
	}else{
		size_t size = 0;
		bool negative = false;
		if ((value < 0) && (base == 10))
		{
			negative = true;
			value = -value;
		}
		while (value != 0)
		{
			const i32 r = value % base;
			string[size++] = (char) ((r > 9) ? ((r - 10) + 'a') : (r + '0'));
			value /= base;
		}
		if(negative)
			string[size++] = '-';
		string[size] = '\0';
		reverseString(string, size);
	}
};

bool charIsDigit(char value)
{
	return (value >= '0') && (value <= '9');
};
bool charIsAlpha(char value)
{
	return 
		((value >= 'a') && (value <= 'z')) || 
		((value >= 'A') && (value <= 'Z')); 
};
bool charIsWhitespace(char value)
{
	return (value == ' ') || (value == '\t') || (value == '\r');
}

i32 stringToInt(const char *string, size_t length)
{
	i32 value = 0;
	i32 sign = 1;

	u32 index = 0;
	if (string[0] == '-')
	{
		sign = -1;
		index = 1;
	}
	while (index < length)
	{
		if (charIsDigit(string[index]))
			value = value*10 + (string[index] - '0');
		index ++;
	}
	return sign*value;
};

u8* loadEntireFile(const char *path, size_t *size)
{
	u8 *result = NULL;

	FILE *f = fopen(path, "rb");
	if (f)
	{
		fseek(f, 0, SEEK_END);
		const size_t file_size = (size_t) ftell(f);
		fseek(f, 0, SEEK_SET);

		u8 *buffer = malloc((file_size+1)*sizeof(u8));
		if (buffer)
		{
			if (fread(buffer, 1, file_size, f) == file_size)
			{
				buffer[file_size] = 0;
				result = buffer;
				
				if (size)
					*size = file_size;
			} else {
				free(buffer);
			}
		}
		fclose(f);
	}
	return result;
}