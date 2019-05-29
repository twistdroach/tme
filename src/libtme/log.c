/* $Id: log.c,v 1.2 2003/09/01 14:58:57 fredette Exp $ */

/* libtme/log.c - logging functions: */

/*
 * Copyright (c) 2003 Matt Fredette
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

#include <tme/common.h>
_TME_RCSID("$Id: log.c,v 1.2 2003/09/01 14:58:57 fredette Exp $");

/* includes: */
#include <tme/log.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else  /* HAVE_STDARG_H */
#include <varargs.h>
#endif /* HAVE_STDARG_H */

/* the locals needed for log-prf.c: */
#define LOG_PRF_LOCALS				\
  va_list prf_args;				\
  int prf_state;				\
  const char *prf_agg;				\
  char prf_char;				\
  int prf_width;				\
  int prf_flag_l;				\
  int prf_flag_0;				\
  int prf_digit;				\
  int prf_value_d;				\
  long int prf_value_ld;			\
  const char *prf_value_s;			\
  char prf_value_c;				\
  char *prf_value_buffer;			\
  /* the largest prf_format_buffer needed is:	\
     % 0 INT l CONVERSION NUL: */		\
  char prf_format_buffer[1 + 1 + (sizeof(int) * 3) + 1 + 1 + 1]

/* this appends raw output: */
void
tme_output_append_raw(char **_output, const char *output_new, unsigned int output_new_length)
{
  char *output;
  unsigned int output_length;
  int saved_errno;

  saved_errno = errno;
  output = *_output;
  if (output == NULL) {
    output_length = 0;
    output = tme_new(char, output_new_length + 1);
  }
  else {
    output_length = strlen(output);
    output = tme_renew(char, output, output_length + output_new_length + 1);
  }
  memcpy(output + output_length, output_new, output_new_length);
  output[output_length + output_new_length] = '\0';
  *_output = output;
  errno = saved_errno;
}

/* this prepends raw output: */
void
tme_output_prepend_raw(char **_output, const char *output_new, unsigned int output_new_length)
{
  char *output;
  unsigned int output_length;
  int saved_errno;

  saved_errno = errno;
  output = *_output;
  if (output == NULL) {
    output_length = 0;
    output = tme_new(char, output_new_length + 1);
  }
  else {
    output_length = strlen(output);
    output = tme_renew(char, output, output_length + output_new_length + 1);
  }
  memmove(output + output_new_length, output, output_length);
  memcpy(output, output_new, output_new_length);
  output[output_length + output_new_length] = '\0';
  *_output = output;
  errno = saved_errno;
}

/* this appends or prepends a character: */
static void
tme_output_xpend_char(char **_output, char output_char, int prepend)
{
  char *output;
  unsigned int output_length;
  int saved_errno;

  saved_errno = errno;
  output = *_output;
  if (output == NULL) {
    output_length = 0;
    output = tme_new(char, 2);
  }
  else {
    output_length = strlen(output);
    output = tme_renew(char, output, output_length + 2);
  }
  if (prepend) {
    memmove(output + 1, output, output_length);
    output[0] = output_char;
  }
  else {
    output[output_length] = output_char;
  }
  output[output_length + 1] = '\0';
  *_output = output;
  errno = saved_errno;
}

/* this appends printf-style output: */
#ifdef HAVE_STDARG_H
void tme_output_append(char **_output, const char *prf_format, ...)
#else  /* HAVE_STDARG_H */
void tme_output_append(_output, prf_format, va_alist)
     char **_output;
     const char *prf_format;
     va_dcl
#endif /* HAVE_STDARG_H */
{
  LOG_PRF_LOCALS;

#define PRF_OUT_MEM(s, len) tme_output_append_raw(_output, s, len)
#define PRF_OUT_CHAR(c) tme_output_xpend_char(_output, c, FALSE)

  do
#include "log-prf.c"
  while (/* CONSTCOND */ 0);

#undef PRF_OUT_MEM
#undef PRF_OUT_CHAR
}

/* this appends printf-style output for an error: */
#ifdef HAVE_STDARG_H
void tme_output_append_error(char **_output, const char *prf_format, ...)
#else  /* HAVE_STDARG_H */
void tme_output_append_error(_output, prf_format, va_alist)
     char **_output;
     const char *prf_format;
     va_dcl
#endif /* HAVE_STDARG_H */
{
  LOG_PRF_LOCALS;

#define PRF_OUT_MEM(s, len) tme_output_append_raw(_output, s, len)
#define PRF_OUT_CHAR(c) tme_output_xpend_char(_output, c, FALSE)

  do
#include "log-prf.c"
  while (/* CONSTCOND */ 0);

#undef PRF_OUT_MEM
#undef PRF_OUT_CHAR
}

/* this prepends printf-style output: */
#ifdef HAVE_STDARG_H
void tme_output_prepend(char **_output, const char *prf_format, ...)
#else  /* HAVE_STDARG_H */
void tme_output_prepend(_output, prf_format, va_alist)
     char **_output;
     const char *prf_format;
     va_dcl
#endif /* HAVE_STDARG_H */
{
  LOG_PRF_LOCALS;

#define PRF_OUT_MEM(s, len) tme_output_prepend_raw(_output, s, len)
#define PRF_OUT_CHAR(c) tme_output_xpend_char(_output, c, TRUE)

  do
#include "log-prf.c"
  while (/* CONSTCOND */ 0);

#undef PRF_OUT_MEM
#undef PRF_OUT_CHAR
}

/* this prepends printf-style output for an error: */
#ifdef HAVE_STDARG_H
void tme_output_prepend_error(char **_output, const char *prf_format, ...)
#else  /* HAVE_STDARG_H */
void tme_output_prepend_error(_output, prf_format, va_alist)
     char **_output;
     const char *prf_format;
     va_dcl
#endif /* HAVE_STDARG_H */
{
  LOG_PRF_LOCALS;

#define PRF_OUT_MEM(s, len) tme_output_prepend_raw(_output, s, len)
#define PRF_OUT_CHAR(c) tme_output_xpend_char(_output, c, TRUE)

  do
#include "log-prf.c"
  while (/* CONSTCOND */ 0);

#undef PRF_OUT_MEM
#undef PRF_OUT_CHAR
}

/* this logs printf-style output: */
#ifdef HAVE_STDARG_H
void tme_log_part(struct tme_log_handle *handle, const char *prf_format, ...)
#else  /* HAVE_STDARG_H */
void tme_log_part(handle, prf_format, va_alist)
     struct tme_log_handle *handle;
     const char *prf_format;
     va_dcl
#endif /* HAVE_STDARG_H */
{
  LOG_PRF_LOCALS;
  int saved_errno;

  /* do nothing if we have no format: */
  if (prf_format == NULL) {
    return;
  }

#define PRF_OUT_MEM(s, len) tme_output_append_raw(&handle->tme_log_handle_message, s, len)
#define PRF_OUT_CHAR(c) tme_output_xpend_char(&handle->tme_log_handle_message, c, FALSE)

  saved_errno = errno;
  do
#include "log-prf.c"
  while (/* CONSTCOND */ 0);
  errno = saved_errno;

#undef PRF_OUT_MEM
#undef PRF_OUT_CHAR
}
