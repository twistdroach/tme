/* $Id: fb.c,v 1.2 2003/07/31 01:35:12 fredette Exp $ */

/* generic/fb.c - generic framebuffer implementation support: */

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
_TME_RCSID("$Id: fb.c,v 1.2 2003/07/31 01:35:12 fredette Exp $");

/* includes: */
#include <tme/generic/fb.h>

/* include the automatically-generated translation functions: */
#include "fb-xlat-auto.c"

/* this returns the best translation function: */
const struct tme_fb_xlat *
tme_fb_xlat_best(const struct tme_fb_xlat *xlat_user)
{
  unsigned int xlat_i;
  const struct tme_fb_xlat *xlat;
  const struct tme_fb_xlat *xlat_best;
  unsigned int xlat_best_score, xlat_score;

  /* loop over the xlats: */
  xlat_best = NULL;
  xlat_best_score = 0;
  for (xlat_i = 0;
       xlat_i < TME_ARRAY_ELS(tme_fb_xlats);
       xlat_i++) {

    /* get this xlat: */
    xlat = &tme_fb_xlats[xlat_i];
    xlat_score = 0;

    /* if this xlat only works for a particular value of the given
       member, and the user's value is different, we cannot use this
       xlat.  otherwise, increase this xlat's score: */
#define TME_FB_XLAT_SCORE(score, member, specific)	\
if ((xlat->member specific)				\
    && (xlat->member != xlat_user->member)) {		\
  continue;						\
}							\
if (xlat->member specific)				\
  xlat_score += score

    TME_FB_XLAT_SCORE(100, tme_fb_xlat_width, != 0);
    TME_FB_XLAT_SCORE(100, tme_fb_xlat_height, != 0);
    TME_FB_XLAT_SCORE(  0, tme_fb_xlat_scale, || TRUE);
    TME_FB_XLAT_SCORE(100, tme_fb_xlat_src_depth, != 0);
    TME_FB_XLAT_SCORE(100, tme_fb_xlat_src_bits_per_pixel, != 0);
    TME_FB_XLAT_SCORE(100, tme_fb_xlat_src_skipx, >= 0);
    TME_FB_XLAT_SCORE(100, tme_fb_xlat_src_scanline_pad, != 0);
    TME_FB_XLAT_SCORE(  0, tme_fb_xlat_src_order, || TRUE);
    TME_FB_XLAT_SCORE(100, tme_fb_xlat_dst_depth, != 0);
    TME_FB_XLAT_SCORE(100, tme_fb_xlat_dst_bits_per_pixel, != 0);
    TME_FB_XLAT_SCORE(100, tme_fb_xlat_dst_skipx, >= 0);
    TME_FB_XLAT_SCORE(100, tme_fb_xlat_dst_scanline_pad, != 0);
    TME_FB_XLAT_SCORE(  0, tme_fb_xlat_dst_order, || TRUE);

#undef TME_FB_XLAT_SCORE

    /* update the best xlat: */
    if (xlat_best == NULL
	|| xlat_best_score < xlat_score) {
      xlat_best = xlat;
      xlat_best_score = xlat_score;
    }
  }

  /* return the best xlat: */
  assert (xlat_best != NULL);
  return (xlat_best);
}

/* this returns nonzero iff the translation function is optimal: */
int
tme_fb_xlat_is_optimal(const struct tme_fb_xlat *xlat)
{
  return (xlat->tme_fb_xlat_width != 0
	  && xlat->tme_fb_xlat_height != 0
	  && xlat->tme_fb_xlat_src_depth != 0
	  && xlat->tme_fb_xlat_src_bits_per_pixel != 0
	  && xlat->tme_fb_xlat_src_skipx >= 0
	  && xlat->tme_fb_xlat_src_scanline_pad != 0
	  && xlat->tme_fb_xlat_dst_depth != 0
	  && xlat->tme_fb_xlat_dst_bits_per_pixel != 0
	  && xlat->tme_fb_xlat_dst_skipx >= 0
	  && xlat->tme_fb_xlat_dst_scanline_pad != 0);
}

/* this returns the number of bytes required for a source framebuffer
   scanline: */
static unsigned long
_tme_fb_xlat_src_bypl(const struct tme_fb_connection *src)
{
  /* NB that this definition must match the one in the
     automatically-generated xlat functions: */
  const unsigned long src_bypl
    = (((((src->tme_fb_connection_skipx
	   + src->tme_fb_connection_width)
	  * src->tme_fb_connection_bits_per_pixel)
	 + (src->tme_fb_connection_scanline_pad - 1))
	& -src->tme_fb_connection_scanline_pad)
       / 8);
  return (src_bypl);
}

/* this returns the number of bytes required for a source framebuffer: */
static unsigned long
_tme_fb_xlat_src_bypb_real(const struct tme_fb_connection *src)
{
  /* NB that these definitions must match those in the
     automatically-generated xlat functions: */
  const unsigned long src_bypl
    = _tme_fb_xlat_src_bypl(src);
  const unsigned long src_bypb_real
    = (((src->tme_fb_connection_height * src_bypl) + 3) & -4);
  return (src_bypb_real);
}

/* this returns the number of bytes allocated for a source framebuffer.
   this includes the guard regions that are needed to guarantee that
   the translation function main loop terminates: */
static unsigned long
_tme_fb_xlat_src_bypb(const struct tme_fb_connection *src)
{
  /* NB that this definition must match the one in the
     automatically-generated xlat functions: */
  const unsigned long src_bypl
    = _tme_fb_xlat_src_bypl(src);
  const unsigned long src_bypb_real
    = _tme_fb_xlat_src_bypb_real(src);
  const unsigned long src_bypb
    = ((src_bypb_real + (src_bypl * 2)) & -4);
  return (src_bypb);
}

/* this forces the next translation to retranslate the entire buffer: */
void
tme_fb_xlat_redraw(struct tme_fb_connection *src)
{
  const tme_uint32_t *src_user;
  tme_uint32_t *src_back;
  unsigned int count32;

  src_user
    = ((const tme_uint32_t *) 
       src->tme_fb_connection_buffer);
  src_back
    = ((tme_uint32_t *) 
       (src->tme_fb_connection_buffer
	+ _tme_fb_xlat_src_bypb(src)));
  for (count32 = _tme_fb_xlat_src_bypb_real(src) / sizeof(tme_uint32_t);
       count32-- > 0; ) {
    *(src_back++) = ~(*(src_user++));
  }
}

/* this allocates memory for a source framebuffer: */
int
tme_fb_xlat_alloc_src(struct tme_fb_connection *src)
{

  /* allocate the buffer.  remember, this is really two buffers - the
     first half is the real, current framebuffer, and the second half
     holds the last frame that was translated: */
  src->tme_fb_connection_buffer
    = tme_new0(tme_uint8_t,
	       _tme_fb_xlat_src_bypb(src) * 2);

  /* force the next translation to do a complete redraw: */
  tme_fb_xlat_redraw(src);
  
  return (TME_OK);
}

/* this scores a framebuffer connection: */
int
tme_fb_connection_score(struct tme_connection *conn, unsigned int *_score)
{
  struct tme_fb_connection *conn_fb;
  struct tme_fb_connection *conn_fb_other;

  /* both sides must be Ethernet connections: */
  assert(conn->tme_connection_type == TME_CONNECTION_FRAMEBUFFER);
  assert(conn->tme_connection_other->tme_connection_type == TME_CONNECTION_FRAMEBUFFER);

  /* one side must be a real display and the other side must be a
     framebuffer emulator: */
  conn_fb = (struct tme_fb_connection *) conn;
  conn_fb_other = (struct tme_fb_connection *) conn->tme_connection_other;
  *_score = ((conn_fb->tme_fb_connection_mode_change != NULL)
	     != (conn_fb_other->tme_fb_connection_mode_change != NULL));
  return (TME_OK);
}

