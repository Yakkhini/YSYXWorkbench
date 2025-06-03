#ifndef KLIB_H__
#define KLIB_H__

#include <am.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// #define __NATIVE_USE_KLIB__

// string.h
void  *memset    (void *s, int c, size_t n);
void  *memcpy    (void *dst, const void *src, size_t n);
void  *memmove   (void *dst, const void *src, size_t n);
int    memcmp    (const void *s1, const void *s2, size_t n);
size_t strlen    (const char *s);
char  *strcat    (char *dst, const char *src);
char  *strcpy    (char *dst, const char *src);
char  *strncpy   (char *dst, const char *src, size_t n);
int    strcmp    (const char *s1, const char *s2);
int    strncmp   (const char *s1, const char *s2, size_t n);

// stdlib.h
void   srand     (unsigned int seed);
int    rand      (void);
void  *malloc    (size_t size);
void   free      (void *ptr);
int    abs       (int x);
int    atoi      (const char *nptr);

// stdio.h
int    printf    (const char *format, ...);
int    sprintf   (char *str, const char *format, ...);
int    snprintf  (char *str, size_t size, const char *format, ...);
int    vsprintf  (char *str, const char *format, va_list ap);
int    vsnprintf (char *str, size_t size, const char *format, va_list ap);

// vsimd.h
void   simd_setzero(uintptr_t ptr3, uintptr_t ptr2, uintptr_t ptr1);
void   simd_load(uintptr_t dest, uintptr_t src);
void   simd_loaddup(uintptr_t dest, uintptr_t src);
void   simd_mul_add(uintptr_t dest, uintptr_t src1, uintptr_t src2);
void   simd_memset(uintptr_t dest, uintptr_t value, size_t size);
void   simd_memmove(uintptr_t dest, uintptr_t src, size_t size);
void   simd_memcpy(uintptr_t dest, uintptr_t src, size_t size);

// assert.h
#ifdef NDEBUG
  #define assert(ignore) ((void)0)
#else
  #define assert(cond) \
    do { \
      if (!(cond)) { \
        printf("Assertion fail at %s:%d\n", __FILE__, __LINE__); \
        halt(1); \
      } \
    } while (0)
#endif

#ifdef __cplusplus
}
#endif

#endif
