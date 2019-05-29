/* $Id: common.h,v 1.3 2003/05/16 21:48:14 fredette Exp $ */

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

/* macros: */
#undef FALSE
#undef TRUE
#define FALSE (0)
#define TRUE (!FALSE)

/* RCS IDs: */
#ifdef notyet
#define _TME_RCSID(x) static const char _tme_rcsid[] = x
_TME_RCSID("$Id: common.h,v 1.3 2003/05/16 21:48:14 fredette Exp $");
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

/* i18n: */
#define _(x) x

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
char *tme_strdup _TME_P((_tme_const char *));

#endif /* !_TME_COMMON_H */
