/* $Id: misc.c,v 1.2 2003/06/27 21:00:21 fredette Exp $ */

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
_TME_RCSID("$Id: misc.c,v 1.2 2003/06/27 21:00:21 fredette Exp $");

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
    
    
