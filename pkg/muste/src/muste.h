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
#include <string.h>
#include <time.h>
#include <errno.h>

/* Drop-in replica of the deprecated legacy sys/timeb.h structure */
struct muste_timeb {
    time_t time;            /* Seconds since Jan 1, 1970 */
    unsigned short millitm; /* Sub-second milliseconds [0-999] */
    short timezone;         /* Legacy timezone offset (unused) */
    short dstflag;          /* Legacy DST indicator (unused) */
};

static inline int muste_vsnprintf(char *buf, size_t size, const char *fmt, ...) {
    va_list ap;
    int rc;

    va_start(ap, fmt);
    rc = vsnprintf(buf, size, fmt, ap);
    va_end(ap);

    return rc;
}

/* Drop-in replacement function for ftime */
extern int muste_ftime(struct muste_timeb *);
extern size_t muste_fread_impl(void *ptr, size_t size, size_t nmemb, FILE *stream, const char *srcfile, int srcline);
extern char *muste_fgets_impl(char *s, int size, FILE *stream, const char *srcfile, int srcline);
extern int muste_fscanf_impl(FILE *stream, const char *format, const char *srcfile, int srcline, ...);

#define MUSTE_BUFSIZE(buf) (__builtin_object_size((buf), 1) == (size_t)-1 ? LLENGTH : __builtin_object_size((buf), 1))

#define muste_sprintf(buf, ...) muste_vsnprintf((buf), MUSTE_BUFSIZE(buf), __VA_ARGS__)
// #define muste_sprintf(buf, ...) snprintf((buf), __builtin_object_size((buf), 1), __VA_ARGS__)

#define muste_snprintf(buf, size, ...) muste_vsnprintf((buf), (size), __VA_ARGS__)

#define muste_vsprintf(buf, fmt, ap) vsnprintf((buf), __builtin_object_size((buf), 1), (fmt), (ap))

static inline char *muste_strncpy(char *dest, const char *src, size_t dest_size) {
    if (dest == NULL || dest_size == 0) {
        return dest;
    }

    muste_snprintf(dest, dest_size, "%s", src ? src : "");

    return dest;
}

static inline void muste_fieldcopy(char *dest, const char *src, size_t width) {
    memmove(dest, src, width);
    dest[width] = '\0';
}

#define muste_fread(ptr, size, nmemb, stream) muste_fread_impl(ptr, size, nmemb, stream, __FILE__, __LINE__)
#define muste_fgets(s, size, stream) muste_fgets_impl(s, size, stream, __FILE__, __LINE__)
#define muste_fscanf(stream, format, ...) muste_fscanf_impl(stream, format, __FILE__, __LINE__, ##__VA_ARGS__)