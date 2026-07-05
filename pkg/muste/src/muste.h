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

#include <stdarg.h>
#include <stdio.h>

static inline int muste_vsnprintf(char *buf, size_t size,
                                  const char *fmt, ...)
{
  va_list ap;
  int rc;
  
  va_start(ap, fmt);
  rc = vsnprintf(buf, size, fmt, ap);
  va_end(ap);
  
  return rc;
}

#define MUSTE_BUFSIZE(buf) (__builtin_object_size((buf), 1) == (size_t)-1 ? LLENGTH : __builtin_object_size((buf), 1))
 
#define muste_sprintf(buf, ...) muste_vsnprintf((buf), MUSTE_BUFSIZE(buf), __VA_ARGS__)
//#define muste_sprintf(buf, ...) snprintf((buf), __builtin_object_size((buf), 1), __VA_ARGS__)

#define muste_snprintf(buf, size, ...) muste_vsnprintf((buf), (MUSTE_BUFSIZE(buf)size), __VA_ARGS__)

#define muste_vsprintf(buf, fmt, ap) vsnprintf((buf), __builtin_object_size((buf), 1), (fmt), (ap))



  