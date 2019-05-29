/* $Id: common.h,v 1.7 2003/10/16 02:55:24 fredette Exp $ */

/* tme/common.h - header file for common things: */

/*
 * Copyright (c) 2002, 2003 Matt Fredette
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by Matt Fredette.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _TME_COMMON_H
#define _TME_COMMON_H

/* includes: */
#include <assert.h>
#include <unistd.h>
#include <tmeconfig.h>
#ifdef _TME_IMPL
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */
#endif /* _TME_IMPL */
#include <sys/types.h>

/* netinet/in.h is needed to get the hton and ntoh functions: */
#include <netinet/in.h>

/* macros: */
#undef FALSE
#undef TRUE
#define FALSE (0)
#define TRUE (!FALSE)

/* RCS IDs: */
#ifdef notyet
#define _TME_RCSID(x) static const char _tme_rcsid[] = x
_TME_RCSID("$Id: common.h,v 1.7 2003/10/16 02:55:24 fredette Exp $");
#else  /* !_TME_IMPL */
#define _TME_RCSID(x)
#endif /* !_TME_IMPL */

/* concatenation: */
#if ((defined(__STDC__) || defined(__cplusplus) || defined(c_plusplus)) && !defined(UNIXCPP)) || defined(ANSICPP)
#define __TME_CONCAT(a,b) a ## b
#define _TME_CONCAT(a,b) __TME_CONCAT(a,b)
#else
#define _TME_CONCAT(a,b) a/**/b
#endif
#define _TME_CONCAT5(a,b,c,d,e) _TME_CONCAT(a,_TME_CONCAT(b,_TME_CONCAT(c,_TME_CONCAT(d,e))))
#define _TME_CONCAT4(a,b,c,d) _TME_CONCAT(a,_TME_CONCAT(b,_TME_CONCAT(c,d)))
#define _TME_CONCAT3(a,b,c) _TME_CONCAT(a,_TME_CONCAT(b,c))

/* prototypes: */
#if defined(__STDC__) || defined(__cplusplus)
#define _TME_P(x) x
#else  /* !__STDC__ && !__cplusplus */
#define _TME_P(x) ()
#endif /* !__STDC__ && !__cplusplus */

/* const and inline: */
#if defined(_TME_IMPL) || defined(__STDC__) || defined(__cplusplus)
#define _tme_const const
#define _tme_inline inline
#else  /* !_TME_IMPL && !__STDC__ && !__cplusplus */
#define _tme_const 
#define _tme_inline
#endif /* !_TME_IMPL && !__STDC__ && !__cplusplus */

/* bits: */
#define _TME_BIT(t, x)		(((t) 1) << (x))
#define TME_BIT(x)		_TME_BIT(unsigned int, x)

/* alignment: */
#define TME_ALIGN(x, y)		(((x) + ((y) - 1)) & -(y))
#define TME_ALIGN_MAX		(8)

/* endianness: */
#define TME_ENDIAN_LITTLE	(0)
#define TME_ENDIAN_BIG		(1)
#ifdef _TME_WORDS_BIGENDIAN
#define TME_ENDIAN_NATIVE TME_ENDIAN_BIG
#else  /* !_TME_WORDS_BIGENDIAN */
#define TME_ENDIAN_NATIVE TME_ENDIAN_LITTLE
#endif /* !_TME_WORDS_BIGENDIAN */

/* cast auditing: */
#ifndef TME_NO_AUDIT_CASTS
#define _tme_audit_type(e, t)	(1 ? (e) : ((t) 0))
#else  /* TME_NO_AUDIT_CASTS */
#define _tme_audit_type(e, t)	(e)
#endif /* TME_AUDIT_CASTS */

/* branch prediction: */
#define __tme_predict_true(e)	(e)
#define __tme_predict_false(e)	(e)

/* memory allocation: */
#define tme_new(t, x)		((t *) tme_malloc(sizeof(t) * (x)))
#define tme_new0(t, x)		((t *) tme_malloc0(sizeof(t) * (x)))
#define tme_renew(t, m, x)	((t *) tme_realloc(m, sizeof(t) * (x)))
#define tme_dup(t, m, x)	((t *) tme_memdup(m, sizeof(t) * (x)))

/* minimum/maximum: */
#define TME_MIN(a, b)		(((a) < (b)) ? (a) : (b))
#define TME_MAX(a, b)		(((a) > (b)) ? (a) : (b))

/* sign extension: */
#define TME_EXT_S8_S16(x)	((tme_int16_t) _tme_audit_type(x, tme_int8_t))
#define TME_EXT_S8_S32(x)	((tme_int32_t) _tme_audit_type(x, tme_int8_t))
#define TME_EXT_S16_S32(x)	((tme_int32_t) _tme_audit_type(x, tme_int16_t))
#define TME_EXT_S8_U16(x)	((tme_uint16_t) TME_EXT_S8_S16(x))
#define TME_EXT_S8_U32(x)	((tme_uint32_t) TME_EXT_S8_S32(x))
#define TME_EXT_S16_U32(x)	((tme_uint32_t) TME_EXT_S16_S32(x))

/* bitfields: */
#define _TME_FIELD_EXTRACTU(t, v, s, l) ((((t) (v)) >> (s)) & (_TME_BIT(t, l) - 1))
#define _TME_FIELD_EXTRACTS(t, v, s, l) (_TME_FIELD_EXTRACTU(t, v, s, l) - (((v) & _TME_BIT(t, (s) + (l) - 1)) ? _TME_BIT(t, l) : 0))
#define TME_FIELD_EXTRACTU(v, s, l) _TME_FIELD_EXTRACTU(unsigned int, v, s, l)
#define TME_FIELD_EXTRACTS(v, s, l) _TME_FIELD_EXTRACTS(signed int, v, s, l)
#define TME_FIELD_EXTRACTS32(v, s, l) _TME_FIELD_EXTRACTS(tme_int32_t, v, s, l)
#define _TME_FIELD_DEPOSIT(t, v, s, l, x) (v) = (((v) & ~((_TME_BIT(t, l) - 1) << (s))) | ((x) << (s)))
#define TME_FIELD_DEPOSIT8(v, s, l, x) _TME_FIELD_DEPOSIT(tme_uint8_t, v, s, l, x)
#define TME_FIELD_DEPOSIT16(v, s, l, x) _TME_FIELD_DEPOSIT(tme_uint16_t, v, s, l, x)
#define TME_FIELD_DEPOSIT32(v, s, l, x) _TME_FIELD_DEPOSIT(tme_uint32_t, v, s, l, x)

/* byteswapping: */
#ifndef _TME_WORDS_BIGENDIAN
#define tme_bswap_u16(x) ((tme_uint16_t) htons((tme_uint16_t) (x)))
#define tme_bswap_u32(x) ((tme_uint32_t) htonl((tme_uint32_t) (x)))
#define tme_htobe_u16(x) tme_bswap_u16(x)
#define tme_htobe_u32(x) tme_bswap_u32(x)
#define tme_htole_u16(x) (x)
#define tme_htole_u32(x) (x)
#else  /* !_TME_WORDS_BIGENDIAN */
#define tme_htobe_u16(x) (x)
#define tme_htobe_u32(x) (x)
#endif /* !_TME_WORDS_BIGENDIAN */
#define tme_betoh_u16(x) tme_htobe_u16(x)
#define tme_betoh_u32(x) tme_htobe_u32(x)
#define tme_letoh_u16(x) tme_htole_u16(x)
#define tme_letoh_u32(x) tme_htole_u32(x)

/* i18n: */
#define _(x) x

/* 64-bit values: */
#ifndef TME_HAVE_INT64_T

/* gcc has a `long long' type that is defined to be twice as long as
   an int: */
/* XXX when exactly did this feature appear? */
#if defined(__GNUC__) && (__GNUC__ >= 2) && (_TME_SIZEOF_INT == 4)
#define TME_HAVE_INT64_T
typedef signed long long int tme_int64_t;
typedef unsigned long long int tme_uint64_t;
#endif /* __GNUC__ && __GNUC__ >= 2 */

#endif /* TME_HAVE_INT64_T */
union tme_value64 {
#ifdef TME_HAVE_INT64_T
  tme_int64_t tme_value64_int;
  tme_uint64_t tme_value64_uint;
#endif /* TME_HAVE_INT64_T */
  tme_int32_t tme_value64_int32s[2];
  tme_int32_t tme_value64_uint32s[2];
#ifndef _TME_WORDS_BIGENDIAN
#define tme_value64_int32_lo tme_value64_int32s[0]
#define tme_value64_int32_hi tme_value64_int32s[1]
#define tme_value64_uint32_lo tme_value64_int32s[0]
#define tme_value64_uint32_hi tme_value64_int32s[1]
#else  /* _TME_WORDS_BIGENDIAN */
#define tme_value64_int32_lo tme_value64_int32s[1]
#define tme_value64_int32_hi tme_value64_int32s[0]
#define tme_value64_uint32_lo tme_value64_int32s[1]
#define tme_value64_uint32_hi tme_value64_int32s[0]
#endif /* _TME_WORDS_BIGENDIAN */
};

/* 64-bit math: */
union tme_value64 *tme_value64_add _TME_P((union tme_value64 *, _tme_const union tme_value64 *));
union tme_value64 *tme_value64_sub _TME_P((union tme_value64 *, _tme_const union tme_value64 *));
union tme_value64 *tme_value64_mul _TME_P((union tme_value64 *, _tme_const union tme_value64 *));
union tme_value64 *tme_value64_div _TME_P((union tme_value64 *, _tme_const union tme_value64 *));
union tme_value64 *_tme_value64_set _TME_P((union tme_value64 *, _tme_const tme_uint8_t *, int));
#ifdef TME_HAVE_INT64_T
#define tme_value64_add(a, b) (((a)->tme_value64_uint += (b)->tme_value64_uint), (a))
#define tme_value64_sub(a, b) (((a)->tme_value64_uint -= (b)->tme_value64_uint), (a))
#define tme_value64_mul(a, b) (((a)->tme_value64_uint *= (b)->tme_value64_uint), (a))
#define tme_value64_div(a, b) (((a)->tme_value64_uint /= (b)->tme_value64_uint), (a))
#define tme_value64_set(a, b) (((b) >= 0) ? ((a)->tme_value64_uint = (b), (a)) : ((a)->tme_value64_int = (b), (a)))
#else  /* !TME_HAVE_INT64_T */
#define tme_value64_set(a, b) _tme_value64_set(a, (_tme_const tme_uint8_t *) &(b), ((b) >= 0 ? sizeof(b) : -sizeof(b)))
#endif /* !TME_HAVE_INT64_T */

/* miscellaneous: */
#define TME_ARRAY_ELS(x)	(sizeof(x) / sizeof(x[0]))
#define TME_EMULATOR_OFF_UNDEF	((void *) (-1))
#define TME_RANGES_OVERLAP(low0, high0, low1, high1)	\
  (((low0) <= (low1) && (low1) <= (high0))		\
   || ((low0) <= (high1) && (high1) <= (high0)))
#define TME_ARG_IS(s, x)	((s) != NULL && !strcmp(s, x))
#define TME_OK			(0)

/* prototypes: */
void *tme_malloc _TME_P((unsigned int));
void *tme_malloc0 _TME_P((unsigned int));
void *tme_realloc _TME_P((void *, unsigned int));
void *tme_memdup _TME_P((_tme_const void *, unsigned int));
void tme_free _TME_P((void *));
void tme_free_string_array _TME_P((char **, int));
char *tme_strdup _TME_P((_tme_const char *));

#ifdef _TME_IMPL
/* string and memory prototypes: */
#ifdef STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#else  /* !STDC_HEADERS */
void *memcpy _TME_P((void *, _tme_const void *, size_t));
void *memset _TME_P((void *, int, size_t));
void *memmove _TME_P((void *, _tme_const void *, size_t));
#endif /* !STDC_HEADERS */
#endif /* _TME_IMPL */

#endif /* !_TME_COMMON_H */
