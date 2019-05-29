/* $Id: fb.h,v 1.1 2003/06/27 21:05:35 fredette Exp $ */

/* tme/generic/fb.h - header file for generic framebuffer support: */

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

#ifndef _TME_GENERIC_FB_H
#define _TME_GENERIC_FB_H

#include <tme/common.h>
_TME_RCSID("$Id: fb.h,v 1.1 2003/06/27 21:05:35 fredette Exp $");

/* includes: */
#include <tme/element.h>

/* macros: */

/* translation scaling: */
#define TME_FB_XLAT_SCALE_HALF		(2 / 2)
#define TME_FB_XLAT_SCALE_NONE		(2)
#define TME_FB_XLAT_SCALE_DOUBLE	(2 * 2)

/* types: */

/* a framebuffer connection: */
struct tme_fb_connection {

  /* the generic connection side: */
  struct tme_connection tme_fb_connection;

  /* this is called when the framebuffer mode changes: */
  int (*tme_fb_connection_mode_change) _TME_P((struct tme_fb_connection *));

  /* this is called before the framebuffer's display is updated: */
  int (*tme_fb_connection_update) _TME_P((struct tme_fb_connection *));

  /* scanlines have this many displayed pixels: */
  unsigned int tme_fb_connection_width;

  /* frames have this many displayed scanlines: */
  unsigned int tme_fb_connection_height;

  /* pixels have this depth, in bits: */
  unsigned int tme_fb_connection_depth;

  /* pixels have this many bits.  this must be at least the same as
     the depth, but it can also be greater, and it must be a power of
     two: */
  unsigned int tme_fb_connection_bits_per_pixel;

  /* scanlines begin with bits for this many *undisplayed* pixels.
     these pixels are not included in tme_fb_connection_width: */
  unsigned int tme_fb_connection_skipx;

  /* scanlines are padded to this many bits.  this must be a power of
     two greater than or equal to 8: */
  unsigned int tme_fb_connection_scanline_pad;

  /* scanline data has this endianness.  this is either TME_ENDIAN_BIG
     or TME_ENDIAN_LITTLE: */
  unsigned int tme_fb_connection_order;

  /* the real framebuffer memory: */
  tme_uint8_t *tme_fb_connection_buffer;

  /* if this is displaying a 1-bit deep source framebuffer, this maps
     from 1-bit pixel values (if we're halving, actually four pixel
     values added together) to a pixel value for the destination: */
  tme_uint32_t *tme_fb_connection_depth1_map;
};

/* one frame buffer translation function: */
struct tme_fb_xlat {

  /* the translation function itself: */
  int (*tme_fb_xlat_func) _TME_P((struct tme_fb_connection *, struct tme_fb_connection *));

  /* iff nonzero, this function applies only when scanlines have this
     many displayed pixels: */
  unsigned int tme_fb_xlat_width;

  /* iff nonzero, this function applies only when there are this many
     scanlines: */
  unsigned int tme_fb_xlat_height;

  /* the scaling that this function does: */
  unsigned int tme_fb_xlat_scale;

  /* iff nonzero, this function applies only when source pixels have
     this depth: */
  unsigned int tme_fb_xlat_src_depth;

  /* iff nonzero, this function applies only when source pixels have
     this many bits: */
  unsigned int tme_fb_xlat_src_bits_per_pixel;

  /* iff >= 0, this function applies only when source scanlines begin
     with this many *undisplayed* pixels.  these pixels are not
     counted in tme_fb_xlat_width: */
  int tme_fb_xlat_src_skipx;

  /* iff nonzero, this function applies only when source scanlines are
     padded to this many bits: */
  unsigned int tme_fb_xlat_src_scanline_pad;

  /* this function applies only when source scanline data has this
     order (endianness): */
  int tme_fb_xlat_src_order; 

  /* iff nonzero, this function applies only when destination pixels
     have this depth: */
  unsigned int tme_fb_xlat_dst_depth;

  /* iff nonzero, this function applies only when destination pixels
     have this many bits: */
  unsigned int tme_fb_xlat_dst_bits_per_pixel;

  /* iff >= 0, this function applies only when destination scanlines
     begin with this many *undisplayed* pixels.  these pixels are not
     counted in tme_fb_xlat_width: */
  int tme_fb_xlat_dst_skipx;

  /* iff nonzero, this function applies only when destination
     scanlines are padded to this many bits: */
  unsigned int tme_fb_xlat_dst_scanline_pad;

  /* this function applies only when destination scanline data has
     this order (endianness): */
  int tme_fb_xlat_dst_order; 
};

/* prototypes: */
_tme_const struct tme_fb_xlat *tme_fb_xlat_best _TME_P((_tme_const struct tme_fb_xlat *));
int tme_fb_xlat_is_optimal _TME_P((_tme_const struct tme_fb_xlat *));
int tme_fb_xlat_alloc_src _TME_P((struct tme_fb_connection *));
void tme_fb_xlat_redraw _TME_P((struct tme_fb_connection *));
int tme_fb_connection_score _TME_P((struct tme_connection *, unsigned int *));

#endif /* !_TME_GENERIC_FB_H */
