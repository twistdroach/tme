/* $Id: misc.c,v 1.4 2004/05/11 12:03:36 fredette Exp $ */

/* libtme/misc.c - miscellaneous: */

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
_TME_RCSID("$Id: misc.c,v 1.4 2004/05/11 12:03:36 fredette Exp $");

/* includes: */
#include <tme/threads.h>
#include <tme/module.h>
#include <tme/misc.h>
#include <ctype.h>

/* this initializes libtme: */
int
tme_init(void)
{
  int rc;
  
  /* initialize the threading system: */
  tme_threads_init();

  /* initialize the module system: */
  tme_module_init();

  rc = TME_OK;
  return (rc);
}

/* this tokenizes a string by whitespace: */
char **
tme_misc_tokenize(const char *string,
		  char comment,
		  int *_tokens_count)
{
  int tokens_count;
  int tokens_size;
  char **tokens;
  const char *p1;
  const char *p2;
  char c;

  /* we initially have no tokens: */
  tokens_count = 0;
  tokens_size = 1;
  tokens = tme_new(char *, tokens_size);

  /* tokenize this line by whitespace and watch for comments: */
  p1 = NULL;
  for (p2 = string;; p2++) {
    c = *p2;

    /* if this is a token delimiter: */
    if (c == '\0'
	|| isspace(c)
	|| c == comment) {

      /* if we had been collecting a token, it's finished: */
      if (p1 != NULL) {
	  
	/* save this token: */
	tokens[tokens_count] = tme_dup(char, p1, (p2 - p1) + 1);
	tokens[tokens_count][p2 - p1] = '\0';
	p1 = NULL;

	/* resize the tokens array if needed: */
	if (++tokens_count == tokens_size) {
	  tokens_size += (tokens_size >> 1) + 1;
	  tokens = tme_renew(char *, tokens, tokens_size);
	}
      }

      /* stop if this is the end of the line or the beginning of a
	 comment: */
      if (c == '\0'
	  || c == comment) {
	break;
      }
    }

    /* otherwise this is part of a token: */
    else {
      if (p1 == NULL) {
	p1 = p2;
      }
    }
  }

  /* done: */
  *_tokens_count = tokens_count;
  tokens[tokens_count] = NULL;
  return (tokens);
}

/* this frees an array of strings: */
void
tme_free_string_array(char **array, int length)
{
  char *string;
  int i;

  if (length < 0) {
    for (i = 0;
	 (string = array[i]) != NULL;
	 i++) {
      tme_free(string);
    }
  }
  else {
    for (i = 0;
	 i < length;
	 i++) {
      tme_free(array[i]);
    }
  }
  tme_free(array);
}
    
#ifdef TME_HAVE_INT64_T
#define _tme_unumber_t tme_uint64_t
#define _tme_number_t tme_int64_t
#else  /* !TME_HAVE_INT64_T */
#define _tme_unumber_t tme_uint32_t
#define _tme_number_t tme_int32_t
#endif /* !TME_HAVE_INT64_T */

/* this internal function parses a number: */
static _tme_unumber_t
_tme_misc_number_parse(const char *string,
		       _tme_unumber_t max_positive,
		       _tme_unumber_t max_negative,
		       _tme_unumber_t underflow,
		       int *_failed)
{
  char c;
  int negative;
  unsigned int base;
  _tme_unumber_t value, max, max_pre_shift;
  tme_uint32_t units;
  unsigned long digit;
  int failed;
  char cbuf[2], *p1;

  /* assume simple conversion failure: */
  *_failed = TRUE;
  errno = 0;

  /* return simple conversion failure for a NULL string: */
  if (string == NULL) {
    return (0);
  }

  /* XXX parts of this might be ASCII-centric: */

  /* skip leading whitespace: */
  for (; (c = *string) != '\0' && isspace(c); string++);

  /* check for a leading '-' or '+' character: */
  if ((negative = (c == '-'))
      || c == '+') {
    c = *(++string);
  }

  /* check for a leading 0x or 0X, indicating hex, or a leading 0,
     indicating octal.  in the octal case, we don't skip the leading
     zero, because it may be the only digit to convert: */
  base = 10;
  if (c == '0') {
    base = 8;
    c = *(string + 1);
    if (c == 'x'
	|| c == 'X') {
      base = 16;
      string += 2;
    }
  }

  /* determine the maximum magnitude of the converted value, and the
     maximum magnitude past which we cannot shift it to add another
     digit in this base without overflowing: */
  max = (negative ? max_negative : max_positive);
  max_pre_shift = max / base;

  /* prepare the strtoul character buffer: */
  cbuf[1] = '\0';

  /* convert characters: */
  value = 0;  
  for (failed = TRUE;
       (c = *string) != '\0';
       failed = FALSE, string++) {
    
    /* stop if we can't convert this character into a digit: */
    cbuf[0] = c;
    digit = strtoul(cbuf, &p1, base);
    if (*p1 != '\0') {
      break;
    }

    /* return ERANGE if this digit causes an overflow: */
    if (value > max_pre_shift
	|| digit > (max - (value *= base))) {
      errno = ERANGE;
      return (negative ? underflow : max_positive);
    }
    value += digit;
  }

  /* get any units: */
  units = 1;
  if (!strcmp(string, "GB")
      || !strcasecmp(string, "G")) {
    units = 1024 * 1024 * 1024;
  }
  else if (!strcmp(string, "MB")
	   || !strcasecmp(string, "M")) {
    units = 1024 * 1024;
  }
  else if (!strcmp(string, "KB")
	   || !strcasecmp(string, "K")) {
    units = 1024;
  }
  else if (*string != '\0') {
    failed = TRUE;
  }

  /* return ERANGE if the units cause an overflow: */
  if (!failed
      && value > (max / units)) {
    errno = ERANGE;
    return (negative ? underflow : max_positive);
  }

  /* return success: */
  *_failed = FALSE;
  value *= units;
  return (negative ? 0 - value : value);
}

/* this parses an unsigned number: */
_tme_unumber_t
tme_misc_unumber_parse_any(const char *string, 
			   int *_failed)
{
  _tme_unumber_t max;
  max = 0;
  max -= 1;
  return (_tme_misc_number_parse(string,
				 max,
				 max,
				 max,
				 _failed));
}

/* this parses a signed number: */
_tme_number_t 
tme_misc_number_parse_any(const char *string,
			  int *_failed)
{
  _tme_unumber_t max_positive;
  _tme_unumber_t max_negative;
  max_positive = 1;
  max_positive = (max_positive << ((sizeof(max_positive) * 8) - 1)) - 1;
  max_negative = max_positive + 1;
  return (_tme_misc_number_parse(string,
				 max_positive,
				 max_negative,
				 max_negative,
				 _failed));
}

/* this parses an unsigned number that has a restricted range: */
_tme_unumber_t
tme_misc_unumber_parse(const char *string, 
		       _tme_unumber_t failure_value)
{
  int failed;
  _tme_unumber_t value;
  value = tme_misc_unumber_parse_any(string, &failed);
  return (failed ? failure_value : value);
}

/* this parses a signed number that has a restricted range: */
_tme_number_t
tme_misc_number_parse(const char *string, 
		      _tme_number_t failure_value)
{
  int failed;
  _tme_number_t value;
  value = tme_misc_number_parse_any(string, &failed);
  return (failed ? failure_value : value);
}

    
