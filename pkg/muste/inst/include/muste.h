/*
#if 0
#define _LARGE_FILE       1
#ifndef _FILE_OFFSET_BITS
 #define _FILE_OFFSET_BITS 64
#endif
#define _LARGEFILE_SOURCE 1
#define _LARGEFILE64_SOURCE 1

#ifdef __MINGW32__
#define fseeko fseeko64
#define ftello ftello64
#endif

#include <stdint.h>
typedef int8_t muste_int8;
typedef int16_t muste_int16;
typedef int32_t muste_int32;
typedef int64_t muste_int64;
#endif
*/
typedef long muste_int64;