#! /bin/sh

# $Id: fb-xlat-auto.sh,v 1.5 2003/09/29 11:42:56 fredette Exp $

# generic/fb-xlat-auto.sh - automatically generates C code 
# for many framebuffer translation functions:

#
# Copyright (c) 2003 Matt Fredette
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. All advertising materials mentioning features or use of this software
#    must display the following acknowledgement:
#      This product includes software developed by Matt Fredette.
# 4. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#

# this script generates one or more C functions.  each function
# translates an image from a source image format into a destination
# image format, optionally halving or doubling the image size in the
# process.
#
# the source and destination image formats are represented by "keys",
# strings that describe the different attributes of a format.
#
# a source image format key has the form: WxHdDbBsSpPoO[_X]
#
# a destination image format key has the form: dDbBsSpPoO
#
# all of the lowercase letters and underscores are literals, and all
# of the uppercase letters represent attribute values.  the different
# attributes are:
#
# W is the width of the source image.  if zero, the generated function
# will be able to translate source images of any width, but it will be
# suboptimal.  if nonzero, the generated function will only be able to
# translate source images of that width, but it will have some
# optimization.  this attribute is only used in source keys, since the
# destination width is always the scaled source width.
#
# H is the height of the source image.  if zero, the generated
# function will be able to translate source images of any height, but
# it will be suboptimal.  if nonzero, the generated function will only
# be able to translate source images of that height, but it will have
# some optimization.  this attribute is only used in source keys,
# since the destination height is always the scaled source height.
#
# D is the color (d)epth, in bits, of the source/destination image.
# if zero, the generated function will be able to handle
# source/destination images of any depth, but it will be suboptimal.
# if nonzero, the generated function will only be able to handle
# source/destination images of that depth, but it will have some
# optimization.
#
# B is the (b)its-per-pixel of the source/destination image.  this
# must be at least as large as the depth, and it may be larger.  if
# zero, the generated function will be able to handle
# source/destination images of any bits-per-pixel, but it will be
# suboptimal.  if nonzero, the generated function will only be able to
# handle source/destination images of that bits-per-pixel, but it will
# have some optimization.
#
# S is the (s)kip-x-bits of the source/destination image.  this is the
# number of bits at the beginning of each image scanline that are not
# displayed.  if an underscore, the generated function will be able to
# handle source/destination images with any skip-x-bits, but it will
# be suboptimal.  if not an underscore, the generated function will
# only be able to handle source/destination images with that many
# skip-x-bits, but it will have some optimization.
#
# P is the (p)adding of the source/destination image.  this is how
# many bits each image scanline is padded to.  if zero, the generated
# function will be able to handle source/destination images with any
# padding, but it will be suboptimal.  if nonzero, the generated
# function will only be able to handle source/destination images with
# that padding, but it will have some optimization.
#
# O is the byte and bit (o)rder of the source/destination image.  this
# is always m or l for most- or least-significant, respectively.
# there is no "any" wildcard value.  this script cannot handle image
# formats where a word-unit of pixels has one order for its bytes in
# memory, but the opposite order for its pixels within the word-unit.
#
# X is the optional scaling.  it is `h' to halve the source image,
# or `d' to double it.  this attribute is only used in source keys.

# this script generates a set of functions for the product of all
# source image formats and all destination image formats.
#
# "all source image formats" includes all source image formats given
# on the command line, plus:
#
#   the "any" source image format, unscaled
#   the "any" source image format, halved
#   the "any" source image format, doubled
#
# "all destination image formats" includes all destination
# image formats given on the command line, plus:
#
#   the "any" destination image format
#
src_all=
dst_all=
for order in m l; do
  src_key="0x0d0b0s_p0o${order}"
  src_all="${src_all} ${src_key} ${src_key}_h ${src_key}_d"
  dst_all="${dst_all} d0b0s_p0o${order}"
done

which=
for arg
do
    case $arg in
    src|dst) which=$arg ;;
    *)
	if test "x${which}" != x; then
	    eval "${which}_all=\"$arg \$${which}_all\""
	fi
	;;
    esac
done

PROG=`basename $0`
cat <<EOF
/* automatically generated by $PROG, do not edit! */

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

_TME_RCSID("\$Id: fb-xlat-auto.sh,v 1.5 2003/09/29 11:42:56 fredette Exp $");

/* the central feature of these translation functions is the "bit
   FIFO", a first-in, first-out stream of bits.  source bit FIFOs are
   used to read pixel bits out of source images, and destination bit
   FIFOs are used to write pixel bits into destination images.

   a bit FIFO has a visible part and an invisible part.

   the visible part of a bit FIFO is 32 bits wide, meaning the
   translation code can read (for a source bit FIFO) or write (for a
   destination bit FIFO) up to 32 bits of pixel data before a bit
   FIFO shift.

   the invisible part of a source bit FIFO contains pixel bits that
   have already been read from the source image memory, but that have
   yet to be shifted in to the visible part.

   the invisible part of a destination bit FIFO contains the pixel
   bits that have been shifted out of the visible part, but that have
   yet to be written to the destination image memory.

   depending on various attributes of an image format, it may be
   possible for the translation code to always read or write the
   entire 32 visible bits of a bit FIFO at a time, and as a result
   always shift the bit FIFO 32 bits at a time.  this is desirable
   because it minimizes bit FIFO shifts.

   when this does happen, it may also be the case that the visible
   part of the bit FIFO always perfectly corresponds to an aligned
   32-bit word in the image buffer.

   this is the optimal situation, as it makes it unnecessary to track
   the invisible part in C local variables at all - each 32-bit FIFO
   shift of a source bit FIFO just reads the next 32-bit word directly
   into the visible part, and each 32-bit FIFO shift of a destination
   bit FIFO just writes the next 32-bit word directly out of the
   visible part.

   within the visible part of a bit FIFO, which bits belong to which
   pixels depends on the "order" of the image format.  most-significant
   (big-endian) images have earlier (more to the left on the screen)
   pixels in more-significant bit positions.  least-significant
   (little-endian) images have earlier pixels in less-significant bit
   bit positions.

   bit significance *within* a pixel never depends on image order.
   once the bits belonging to a pixel have been identified according
   to image order, the least significant bit in that pixel's value is
   always the bit with the least absolute value.

   some pictures may help explain this better.  each picture shows the
   32 bit visible part of a bit FIFO, with the bits marked from 0
   (least significant) to 31 (most significant).  all of these
   examples are for a two-bit-deep/two-bits-per-pixel image, and in
   the visible part sixteen pixels are numbered.  lesser numbered
   pixels are earlier (more to the left on the screen).  some of the
   invisible part is also shown, along with the direction that the bit
   FIFO shifts in:

   a source bit FIFO for a little-endian source image with two bits
   per pixel looks like:

                        |  
                        | 31 30 29 28     7  6  5  4  3  2  1  0
      --+--+--+--+--+--+|+--+--+--+--+-  --+--+--+--+--+--+--+--+
    .. p18 | p17 | p16 ||| p15 | p14 | .. p3  | p2  | p1  | p0  |  
      --+--+--+--+--+--+|+--+--+--+--+-  --+--+--+--+--+--+--+--+
                        |
   invisible part       |             visible part
   
          shift -> shift -> shift -> shift -> shift ->

   a source bit FIFO for a big-endian source image with two bits per
   pixel looks like:

                                            |
     31 30 29 28 27 26 25 26  4  3  2  1  0 |                
    +--+--+--+--+--+--+--+--  -+--+--+--+--+|+--+--+--+--+--+--
    | p0  | p1  | p2  | p3  .. | p14 | p15 ||| p16 | p17 | p18 ..
    +--+--+--+--+--+--+--+--  -+--+--+--+--+|+--+--+--+--+--+--
                                            |
               visible part                 |    invisible part

        <- shift <- shift <- shift <- shift <- shift

   a destination bit FIFO for a little-endian destination image with
   two bits per pixel looks like:

                                            |
     31 30 29 28 27 26 25 26  4  3  2  1  0 |                
    +--+--+--+--+--+--+--+--  -+--+--+--+--+|+--+--+--+--+--+--
    | p23 | p22 | p21 | p20 .. | p9  | p8  ||| p7  | p6  | p5  ..
    +--+--+--+--+--+--+--+--  -+--+--+--+--+|+--+--+--+--+--+--
                                            |
               visible part                 |    invisible part

          shift -> shift -> shift -> shift -> shift ->

   a destination bit FIFO for a big-endian destination image with
   two bits per pixel looks like:

                        |  
                        | 31 30 29 28     7  6  5  4  3  2  1  0
      --+--+--+--+--+--+|+--+--+--+--+-  --+--+--+--+--+--+--+--+
    .. p5  | p6  | p7  ||| p8  | p9  | .. p20 | p21 | p22 | p23 |  
      --+--+--+--+--+--+|+--+--+--+--+-  --+--+--+--+--+--+--+--+
                        |
   invisible part       |             visible part

        <- shift <- shift <- shift <- shift <- shift
   
  each translation function has at least one source bit FIFO and at
  least one destination bit FIFO.  the general idea is to read source
  pixel value(s) from the source image, map those source pixel
  value(s) somehow into destination pixel value(s), and write those
  destination pixel value(s) to the destination image.

  translation functions that do not scale the image have exactly one
  source bit FIFO and exactly one destination bit FIFO.  one pixel
  read from the source bit FIFO becomes one pixel written to the
  destination bit FIFO.

  translation functions that halve the image size have two source bit
  FIFOs and one destination bit FIFO.  one source bit FIFO sources
  bits from an even-numbered scanline, and the other sources bits from
  an odd-numbered scanline.  four pixels read from the source bit
  FIFOs - two from each source bit FIFO, making a 2x2 square - become
  one pixel written to the destination bit FIFO.

  translation functions that double the image size have one source
  bit FIFO and two destination bit FIFOs.  one destination bit FIFO
  sinks bits for an even-numbered scanline, and the other sinks bits
  for an odd-numbered scanline.  one pixel read from the source bit
  FIFO becomes four pixels written to the destination bit FIFOs -
  two to each destination bit FIFO, making a 2x2 square.   

  translation functions don't necessarily always translate the entire
  source image into the entire destination image.  instead, the buffer
  holding the current source image is expected to be twice as large as
  necessary (plus some overhead), with the second half of the buffer
  holding a copy of the last translated ("old") source image.

  before setting up and translating pixels using bit FIFOs, a
  translation function treats the the current and old source images as
  an array of aligned 32-bit words and compares them.  if it finds no
  32-bit word that has changed, no translation is done and the
  function returns.

  otherwise, initial source pixel x and y coordinates are derived from
  the offset of the 32-bit word that failed comparison, and the source
  primary bit FIFO is primed.  if this translation function halves the
  image size, the source secondary bit FIFO is primed from the same x
  coordinate on the "other" (think y ^ 1) scanline.

  then, initial destination pixel x and y coordinates are derived from
  the initial source x and y coordinates, and the destination primary
  bit FIFO is primed.  if this translation function doubles the image
  size, the destination secondary bit FIFO is primed from the same x
  coordinate on the "other" (think y ^ 1) scanline.

  as mentioned previously, the buffer holding the current source image
  is expected to be twice as large as necessary (plus some overhead),
  with the second half of the buffer holding a copy of the last
  translated ("old") source image.

  this "overhead" is approximately two extra scanlines worth of data,
  that is initialized to all-bits-zero and must always remain zero.
  this extra data is present at the end of both the current ("new")
  and last translated ("old") source images.

  these extra, always-blank scanlines guarantee that the pixel
  translation loop terminates.  the pixel translation loop *never*
  checks for the end of the image buffer.  instead, it terminates only
  after it has read in TME_FB_XLAT_RUN consecutive aligned 32-bit
  words that have *not* changed between the new and old source images.
  this small amount of extra memory overhead simplifies the pixel
  translation loop, because it doesn't have to worry about going past
  the end of the actual image.
*/


/* macros: */

/* given a 32-bit aligned pointer into the current source image, this
   returns the corresponding 32-bit aligned pointer in the last
   translated ("old") source image.  since the old source image
   follows the current source image, this is simple pointer arithmetic
   using src_bypb: */
#define TME_FB_XLAT_SRC_OLD(raw)				\\
  ((tme_uint32_t *) (((tme_uint8_t *) (raw)) + src_bypb))

/* when the fast, aligned 32-bit word comparison loop finds a word
   that has changed in the source image, pixels are translated until
   TME_FB_XLAT_RUN consecutive aligned 32-bit words are processed that
   have *not* changed in the source image, at which point the fast
   comparison loop resumes.

   the idea is that after you've started translating pixels, once the
   FIFO shift operation has read TME_FB_XLAT_RUN consecutive raw,
   unchanged 32-bit words, all of the pixels from previous, changed
   32-bit words have been translated and shifted out of the source bit
   FIFO(s), and all bits remaining in the source bit FIFO(s) are for
   pixels in those unchanged 32-bit words.  since the pixels are
   unchanged, pixel translation can stop, and the entire state of the
   source bit FIFO(s) can be discarded.

   so, each time a raw, changed 32-bit word is read, xlat_run is
   reloaded with TME_FB_XLAT_RUN, and each time 32 bits worth of
   source image pixels are processed, it is decremented.  when it
   reaches zero, the source bit FIFO(s) are discarded, the destination
   bit FIFO(s) are flushed, the pixel translation loop breaks and the
   fast comparison loop continues: */
#define TME_FB_XLAT_RUN (2)

/* this shifts a source FIFO: */
#define TME_FB_XLAT_SHIFT_SRC(unaligned, fifo, next, bits, shift, raw, order)\\
do {								\\
								\\
  /* if the source FIFO may not be 32-bit aligned: */           \\
  if (unaligned) {                                              \\
                                                                \\
    /* we must be shifting between 1 and 32 bits: */		\\
    assert ((shift) >= 1 && (shift) <= 32);			\\
								\\
    /* the FIFO must have more than 32 bits in it already: */	\\
    assert (bits > 32);						\\
								\\
    /* shift the FIFO: */					\\
    if ((shift) == 32) {					\\
      fifo = next;						\\
    }								\\
    else if (order == TME_ENDIAN_BIG) {				\\
      fifo = (fifo << ((shift) & 31)) | (next >> (32 - (shift)));\\
      next <<= ((shift) & 31);					\\
    }								\\
    else {							\\
      fifo = (fifo >> ((shift) & 31)) | (next << (32 - (shift)));\\
      next >>= ((shift) & 31);					\\
    }								\\
    bits -= (shift);						\\
								\\
    /* if we have a new 32-bit word to read: */			\\
    if (bits <= 32) {						\\
      next = *raw;						\\
      if (*TME_FB_XLAT_SRC_OLD(raw) != next) {			\\
        *TME_FB_XLAT_SRC_OLD(raw) = next;			\\
        xlat_run = TME_FB_XLAT_RUN;				\\
      }								\\
      raw++;							\\
      next = (order == TME_ENDIAN_BIG				\\
              ? tme_betoh_u32(next)				\\
              : tme_letoh_u32(next));				\\
								\\
      /* before the load, if there were fewer than 32 bits	\\
         remaining in the FIFO, shift bits from the word	\\
         we just loaded into their proper positions: */		\\
      if (bits < 32) {						\\
        if (order == TME_ENDIAN_BIG) {				\\
          fifo |= (next >> bits);				\\
          next <<= (32 - bits);					\\
        }							\\
        else {							\\
          fifo |= (next << bits);				\\
          next >>= (32 - bits);					\\
        }							\\
      }								\\
								\\
      /* there are now 32 more bits in the FIFO: */		\\
      bits += 32;						\\
    }								\\
  }								\\
								\\
  /* otherwise, if the source FIFO is always 32-bit aligned: */ \\
  else {                                                        \\
                                                                \\
    /* we must be shifting exactly 32 bits: */                  \\
    assert((shift) == 32);                                      \\
                                                                \\
    /* load the next 32-bit word: */                            \\
    fifo = *raw;                                                \\
    if (*TME_FB_XLAT_SRC_OLD(raw) != fifo) {                    \\
      *TME_FB_XLAT_SRC_OLD(raw) = fifo;                         \\
      xlat_run = TME_FB_XLAT_RUN;                               \\
    }                                                           \\
    raw++;                                                      \\
    fifo = (order == TME_ENDIAN_BIG                             \\
            ? tme_betoh_u32(fifo)                               \\
            : tme_letoh_u32(fifo));                             \\
  }                                                             \\
} while (/* CONSTCOND */ 0)

/* this shifts a destination FIFO: */
#define TME_FB_XLAT_SHIFT_DST(unaligned, fifo, next, bits, shift, raw, order)\\
do {								\\
								\\
  /* if the destination FIFO may not be 32-bit aligned: */      \\
  if (unaligned) {						\\
								\\
    /* we must be shifting between 1 and 32 bits: */		\\
    assert ((shift) >= 1 && (shift) <= 32);			\\
								\\
    /* the FIFO must have fewer than 32 bits in it: */	        \\
    assert (bits < 32);						\\
								\\
    /* shift the FIFO: */					\\
    if (order == TME_ENDIAN_BIG) {				\\
      next |= (fifo >> bits);					\\
      fifo <<= (32 - bits);					\\
    }								\\
    else {							\\
      next |= (fifo << bits);					\\
      fifo >>= (32 - bits);					\\
    }								\\
    bits += (shift);						\\
								\\
    /* if we have a completed 32-bit word to write: */		\\
    if (bits >= 32) {						\\
      *(raw++) = (order == TME_ENDIAN_BIG			\\
                  ? tme_htobe_u32(next)				\\
                  : tme_htole_u32(next));			\\
      bits -= 32;						\\
      assert(bits != 0 || fifo == 0);				\\
      next = fifo;						\\
    }								\\
  }								\\
								\\
  /* the destination FIFO is always 32-bit aligned: */		\\
  else {							\\
								\\
    /* we must be shifting exactly 32 bits: */                  \\
    assert((shift) == 32);                                      \\
                                                                \\
    /* store the next 32-bit word: */                           \\
    *(raw++) = (order == TME_ENDIAN_BIG                         \\
                ? tme_htobe_u32(fifo)                           \\
                : tme_htole_u32(fifo));                         \\
                                                                \\
  }								\\
                                                                \\
  /* clear the writable part of the FIFO: */			\\
  fifo = 0;							\\
} while (/* CONSTCOND */ 0)
EOF

# the next function number:
xlat_next=0
xlat_array=

# permute over the source possibilities:
for src_key in ${src_all}; do

    # take apart the source possibility:
    src_parts=${src_key}

    # get the width:
        width=`echo ${src_parts} | sed -e 's/^\([0-9][0-9]*\)\(.*\)$/\1/'`
    src_parts=`echo ${src_parts} | sed -e 's/^\([0-9][0-9]*\)\(.*\)$/\2/'`
    
    # get the height:
       height=`echo ${src_parts} | sed -e 's/^x\([0-9][0-9]*\)\(.*\)$/\1/'`
    src_parts=`echo ${src_parts} | sed -e 's/^x\([0-9][0-9]*\)\(.*\)$/\2/'`

    # get the source depth:
    src_depth=`echo ${src_parts} | sed -e 's/^d\([0-9][0-9]*\)\(.*\)$/\1/'`
    src_parts=`echo ${src_parts} | sed -e 's/^d\([0-9][0-9]*\)\(.*\)$/\2/'`

    # get the source bits per pixel:
     src_bipp=`echo ${src_parts} | sed -e 's/^b\([0-9][0-9]*\)\(.*\)$/\1/'`
    src_parts=`echo ${src_parts} | sed -e 's/^b\([0-9][0-9]*\)\(.*\)$/\2/'`

    # get the source skip pixel count:
    src_skipx=`echo ${src_parts} | sed -e 's/^s\([_0-9][0-9]*\)\(.*\)$/\1/'`
    src_parts=`echo ${src_parts} | sed -e 's/^s\([_0-9][0-9]*\)\(.*\)$/\2/'`
    
    # get the source scanline pad:
      src_pad=`echo ${src_parts} | sed -e 's/^p\([0-9][0-9]*\)\(.*\)$/\1/'`
    src_parts=`echo ${src_parts} | sed -e 's/^p\([0-9][0-9]*\)\(.*\)$/\2/'`

    # get the source "order" - i.e., byte order and leftmost (first)
    # pixel significance:
    src_order=`echo ${src_parts} | sed -e 's/^o\([ml]\)\(.*\)$/\1/'`
    src_parts=`echo ${src_parts} | sed -e 's/^o\([ml]\)\(.*\)$/\2/'`

    # get the source scaling:
    scale="${src_parts}_"

    # permute over the destination possibilities:
    for dst_key in ${dst_all}; do

	# take apart the destination possibility:
	dst_parts=${dst_key}

	# get the destination depth:
	dst_depth=`echo ${dst_parts} | sed -e 's/^d\([0-9][0-9]*\)\(.*\)$/\1/'`
	dst_parts=`echo ${dst_parts} | sed -e 's/^d\([0-9][0-9]*\)\(.*\)$/\2/'`

	# get the destination bits per pixel:
	 dst_bipp=`echo ${dst_parts} | sed -e 's/^b\([0-9][0-9]*\)\(.*\)$/\1/'`
	dst_parts=`echo ${dst_parts} | sed -e 's/^b\([0-9][0-9]*\)\(.*\)$/\2/'`
    
	# get the destination skip pixel count:
	dst_skipx=`echo ${dst_parts} | sed -e 's/^s\([_0-9][0-9]*\)\(.*\)$/\1/'`
	dst_parts=`echo ${dst_parts} | sed -e 's/^s\([_0-9][0-9]*\)\(.*\)$/\2/'`
    
	# get the destination scanline pad:
	  dst_pad=`echo ${dst_parts} | sed -e 's/^p\([0-9][0-9]*\)\(.*\)$/\1/'`
	dst_parts=`echo ${dst_parts} | sed -e 's/^p\([0-9][0-9]*\)\(.*\)$/\2/'`

	# get the destination "order" - i.e., byte order and leftmost
	# (first) pixel significance:
	dst_order=`echo ${dst_parts} | sed -e 's/^o\([ml]\)\(.*\)$/\1/'`
	dst_parts=`echo ${dst_parts} | sed -e 's/^o\([ml]\)\(.*\)$/\2/'`

	# when both source and destination depth are known, ignore
	# this permutation if the source depth is greater:
	if test $src_depth != 0 \
	   && test $dst_depth != 0 \
	   && test `expr ${src_depth} \> ${dst_depth}` = 1; then
	   continue
	fi

	# allocate the xlat number:
	xlat=${xlat_next}
	xlat_next=`expr ${xlat_next} + 1`
	xlat_info="tme_fb_xlat${xlat}"

	# start the function:
	echo ""
	echo "/* this translates frame buffer contents from this source format:"
	case "${width}x${height}" in
	0x0) echo -n "     any dimensions" ;;
	*x0) echo -n "     width $width, any height" ;;
	0x*) echo -n "     any width, height ${height}" ;;
	*)   echo -n "     ${width}x${height}" ;;
	esac
	xlat_info="${xlat_info}, ${width}, ${height}"
	case ${scale} in
	_h_) echo " (image will be halved)" ; value="HALF" ;;
	_d_) echo " (image will be doubled)" ; value="DOUBLE" ;;
	*) echo "" ; value="NONE" ;;
	esac
	xlat_info="${xlat_info}, TME_FB_XLAT_SCALE_${value}"
	echo -n "     "
	case ${src_depth} in
	0)  echo -n "any depth" ;;
	1)  echo -n "1 bit deep" ;;
	*)  echo -n "${src_depth} bits deep" ;;
	esac
	xlat_info="${xlat_info}, ${src_depth}"
	case ${src_bipp} in
	0)  echo -n ", any bits per pixel" ;;
	1)  echo -n ", 1 bit per pixel" ;;
	*)  echo -n ", ${src_bipp} bits per pixel" ;;
	esac
	xlat_info="${xlat_info}, ${src_bipp}"
	case ${src_skipx} in
	_) echo -n ", any number of pixels skipped" ; value="-1";;
	*) echo -n ", ${src_skipx} pixels skipped" ; value=${src_skipx} ;;
	esac
	xlat_info="${xlat_info}, ${value}"
	case ${src_pad} in
	0)  echo -n ", any scanline padding" ;;
	*)  echo -n ", ${src_pad}-bit scanline padding" ;;
	esac
	xlat_info="${xlat_info}, ${src_pad}"
	case ${src_order} in
	m)  echo -n ", MSB-first" ; value="BIG" ;;
	l)  echo -n ", LSB-first" ; value="LITTLE" ;;
	esac
	xlat_info="${xlat_info}, TME_ENDIAN_${value}"
	echo ""
	echo "   to this destination format:"
	echo -n "     "
	case ${dst_depth} in
	0)  echo -n "any depth" ;;
	1)  echo -n "1 bit deep" ;;
	*)  echo -n "${dst_depth} bits deep" ;;
	esac
	xlat_info="${xlat_info}, ${dst_depth}"
	case ${dst_bipp} in
	0)  echo -n ", any bits per pixel" ;;
	1)  echo -n ", 1 bit per pixel" ;;
	*)  echo -n ", ${dst_bipp} bits per pixel" ;;
	esac
	xlat_info="${xlat_info}, ${dst_bipp}"
	case ${dst_skipx} in
	_) echo -n ", any number of pixels skipped" ; value="-1";;
	*) echo -n ", ${dst_skipx} pixels skipped" ; value=${dst_skipx} ;;
	esac
	xlat_info="${xlat_info}, ${value}"
	case ${dst_pad} in
	0)  echo -n ", any scanline padding" ;;
	*)  echo -n ", ${dst_pad}-bit scanline padding" ;;
	esac
	xlat_info="${xlat_info}, ${dst_pad}"
	case ${dst_order} in
	m)  echo -n ", MSB-first" ; value="BIG" ;;
	l)  echo -n ", LSB-first" ; value="LITTLE" ;;
	esac
	xlat_info="${xlat_info}, TME_ENDIAN_${value}"
	save_IFS=$IFS
	IFS=
	xlat_array="${xlat_array}
  { $xlat_info }, "
	IFS=$save_IFS
	echo ""
	echo "*/"
	echo "static int"
	echo "tme_fb_xlat${xlat}(struct tme_fb_connection *src,"
	echo "             struct tme_fb_connection *dst)"
	echo "{"
	undef_macros=

	echo ""
	echo "  /* whenever possible we define macros instead of declaring"
	echo "     variables, for optimization: */"

	echo ""
	echo "  /* declare src_x and src_y.  these are the current translation"
	echo "     coordinates in the source image: */"
	echo "  unsigned int src_x, src_y;"

	echo ""
	echo "  /* declare dst_x and dst_y.  these are the current translation"
	echo "     coordinates in the destination image.  since this function"
	if test $scale = _; then
	    echo "     does not scale the image, these coordinates are always"
	    echo "     the same as the coordinates in the source image: */"
	    echo "#define dst_x (src_x)"
	    echo "#define dst_y (src_y)"
	    undef_macros="${undef_macros} dst_x dst_y"
	    scale_op=
	    scale_math=
	else
	    if test $scale = _h_; then
		scale_op='/'
		scale_name='halves'
	    else
		scale_op='*'
		scale_name='doubles'
	    fi
	    scale_math=" ${scale_op} 2"
	    echo "     ${scale_name} the image, the coordinates in the destination image"
	    echo "     are always the coordinates in the source image${scale_math}: */"
	    echo "  unsigned int dst_x, dst_y;"
	fi

	echo ""
	echo "  /* declare pixel.  this holds a single pixel value being translated"
	echo "     for the destination image: */"
	echo "  tme_uint32_t pixel;"

	if test $scale = _h_ && test $src_depth != 1; then
	    echo ""
	    echo "  /* since this function halves the source image, and this"
	    echo "     image isn't grayscale, we need to read all pixels in the"
	    echo "     2x2 square in the source image into separate variables"
	    echo "     and then make a decision about how to map those four"
	    echo "     pixels into one color value for the destination pixel."
	    echo "     the above \"pixel\" will hold one of the four pixels,"
	    echo "     and these three variables will hold the others: */"
	    echo "  tme_uint32_t src_pixel1, src_pixel2, src_pixel3;"
	fi

	echo ""
	echo "  /* declare src_width and dst_width.  these are in terms of pixels: */"
	value="(src_width${scale_math})"	
	if test ${width} = 0; then
	    echo "  const unsigned int src_width = src->tme_fb_connection_width;"
	    if test $scale != _; then
		echo "  const unsigned int dst_width = ${value};"
	    else
		echo "#define dst_width ${value}"
		undef_macros="${undef_macros} dst_width"
	    fi
	else
	    echo "#define src_width (${width})"
	    echo "#define dst_width ${value}"
	    undef_macros="${undef_macros} src_width dst_width"
	fi

	echo ""
	echo "  /* declare src_depth, the source pixel depth, which is in"
	echo "     terms of bits.  declare src_mask, which is the corresponding"
	echo "     mask of one bits: */"
	value="(0xffffffff >> (32 - src_depth))"
	if test ${src_depth} = 0; then
	    echo "  const unsigned int src_depth = src->tme_fb_connection_depth;"
	    echo "  const tme_uint32_t src_mask = ${value};"
	else
	    echo "#define src_depth (${src_depth})"
	    echo "#define src_mask ${value}"
	    undef_macros="${undef_macros} src_depth src_mask"
	fi

	echo ""
	echo "  /* declare src_bipp and dst_bipp.  these are the bits-per-pixel"
	echo "     values for the source and destination images: */"
	if test ${src_bipp} = 0; then
	    echo "  const unsigned int src_bipp = src->tme_fb_connection_bits_per_pixel;"
	else
	    echo "#define src_bipp (${src_bipp})"
	    undef_macros="${undef_macros} src_bipp"
	fi
	if test ${dst_bipp} = 0; then
	    echo "  const unsigned int dst_bipp = dst->tme_fb_connection_bits_per_pixel;"
	else
	    echo "#define dst_bipp (${dst_bipp})"
	    undef_macros="${undef_macros} dst_bipp"
	fi

	echo ""
	echo "  /* declare src_skipx and dst_skipx.  these are the counts of"
	echo "     undisplayed pixels at the beginning of each scanline in the"
	echo "     source and destination images: */"
	if test ${src_skipx} = _; then
	    echo "  const unsigned int src_skipx = src->tme_fb_connection_skipx;"
	else
	    echo "#define src_skipx (${src_skipx})"
	    undef_macros="${undef_macros} src_skipx"
	fi
	if test ${dst_skipx} = _; then
	    echo "  const unsigned int dst_skipx = dst->tme_fb_connection_skipx;"
	else
	    echo "#define dst_skipx (${dst_skipx})"
	    undef_macros="${undef_macros} dst_skipx"
	fi

	echo ""
	echo "  /* declare src_pad and dst_pad.  these are the paddings, in bits,"
	echo "     of each scanline in the source and destination images: */"
	if test ${src_pad} = 0; then
	    echo "  const unsigned int src_pad = src->tme_fb_connection_scanline_pad;"
	else
	    echo "#define src_pad (${src_pad})"
	    undef_macros="${undef_macros} src_pad"
	fi
	if test ${dst_pad} = 0; then
	    echo "  const unsigned int dst_pad = dst->tme_fb_connection_scanline_pad;"
	else
	    echo "#define dst_pad (${dst_pad})"
	    undef_macros="${undef_macros} dst_pad"
	fi

	echo ""
	echo "  /* declare src_order and dst_order.  these are the bit and byte"
	echo "     orders (either TME_ENDIAN_BIG or TME_ENDIAN_LITTLE) of the"
	echo "     source and destination images.  since these values profoundly"
	echo "     affect optimization, they are always constant: */"
	if test ${src_order} = m; then value=BIG; else value=LITTLE; fi
	echo "#define src_order (TME_ENDIAN_${value})"
	undef_macros="${undef_macros} src_order"
	if test ${dst_order} = m; then value=BIG; else value=LITTLE; fi
	echo "#define dst_order (TME_ENDIAN_${value})"
	undef_macros="${undef_macros} dst_order"

	echo ""
	echo "  /* declare src_bypl and dst_bypl.  these are the bytes per scanline"
	echo "     in the source and destination images.  these values are calculated"
	echo "     from the count of undisplayed and displayed pixels per scanline,"
	echo "     the number of bits per pixel, and the scanline padding: */"
	value="(((((src_skipx + src_width) * src_bipp) + (src_pad - 1)) & -src_pad) / 8)"
	if test ${src_skipx} = _ || test ${width} = 0 || test ${src_bipp} = 0 || test ${src_pad} = 0; then
	    echo "  const unsigned int src_bypl = ${value};"
	    src_bypl_var=true
	else
	    echo "#define src_bypl ${value}"
	    undef_macros="${undef_macros} src_bypl"
	    src_bypl_var=false
	fi
	value="(((((dst_skipx + dst_width) * dst_bipp) + (dst_pad - 1)) & -dst_pad) / 8)"
	if test ${dst_skipx} = _ || test ${width} = 0 || test ${dst_bipp} = 0 || test ${dst_pad} = 0; then
	    echo "  const unsigned int dst_bypl = ${value};"
	    dst_bypl_var=true
	else
	    echo "#define dst_bypl ${value}"
	    undef_macros="${undef_macros} dst_bypl"
	    dst_bypl_var=false
	fi

	echo ""
	echo "  /* declare src_packed and dst_packed.  these are nonzero iff"
	echo "     every last bit in a scanline belongs to a displayed pixel."
	echo "     put another way, this is zero iff a scanline has undisplayed"
	echo "     pixels at its beginning or padding bits at its end.  when"
	echo "     a source image or destination image is packed, translation"
	echo "     doesn't have to worry about skipping FIFO bits to get to"
	echo "     bits belonging to displayed pixels: */"
	value="((src_width * src_bipp) == (src_bypl * 8))"
	if $src_bypl_var; then
	    echo "  const unsigned int src_packed = ${value};"
	else
	    echo "#define src_packed ${value}"
	    undef_macros="${undef_macros} src_packed"
	fi
	value="((dst_width * dst_bipp) == (dst_bypl * 8))"
	if $dst_bypl_var; then
	    echo "  const unsigned int dst_packed = ${value};"
	else
	    echo "#define dst_packed ${value}"
	    undef_macros="${undef_macros} dst_packed"
	fi

	echo ""
	echo "  /* declare src_bypb and src_bypb_real.  src_bypb is the bytes"
	echo "     per source image buffer with the \"translation termination"
	echo "     overhead\" of approximately two extra scanlines.  src_bypb_real"
	echo "     is the real bytes per source image buffer with no overhead."
	echo "     both values are padded to a multiple of 4 bytes (32 bits): */"
	if test ${height} = 0; then
	    value="src->tme_fb_connection_height"
	else
	    value="${height}"
	fi
	value_real="(((${value} * src_bypl) + 3) & -4)"
	value="((src_bypb_real + (src_bypl * 2)) & -4)"
	if test ${height} = 0 || $src_bypl_var; then
	    echo "  const unsigned int src_bypb_real = ${value_real};"
	    echo "  const unsigned int src_bypb = ${value};"
	else
	    echo "#define src_bypb_real ${value_real}"
	    echo "#define src_bypb ${value}"
	    undef_macros="${undef_macros} src_bypb_real src_bypb"
	fi
	
	echo ""
	echo "  /* declare the source primary bit FIFO:"
	echo ""
	echo "     src_raw0 points to the next aligned 32-bit word to be"
	echo "     read from the image buffer."
	echo ""
	echo "     src_fifo0 is the visible part of the bit FIFO."
	echo ""
	echo "     src_fifo0_next and src_fifo0_bits are only used when the"
	echo "     visible part of the bit FIFO is not guaranteed to always"
	echo "     correspond to an aligned 32-bit word in the image buffer."
	echo "     src_fifo0_next is the invisible part of the bit FIFO,"
	echo "     and src_fifo0_bits tracks the total number of bits in the"
	echo "     visible and invisible parts of the FIFO. */"
	echo "  const tme_uint32_t *src_raw0;"
	echo "  tme_uint32_t src_fifo0, src_fifo0_next;"
	echo "  unsigned int src_fifo0_bits;"

	if test ${scale} = _h_; then
	    echo ""
	    echo "  /* since this function ${scale_name} the image, declare the"
	    echo "     source secondary bit FIFO.  these variables work in "
	    echo "     exactly the same way that their primary counterparts do: */"
	    echo "  const tme_uint32_t *src_raw1;"
	    echo "  tme_uint32_t src_fifo1, src_fifo1_next;"
	    echo "  unsigned int src_fifo1_bits;"
	fi

	echo ""
	echo "  /* declare the destination primary bit FIFO:"
	echo ""
	echo "     dst_raw0 points to the next aligned 32-bit word to be"
	echo "     written into the image buffer."
	echo ""
	echo "     dst_fifo0 is the visible part of the bit FIFO."
	echo ""
	echo "     dst_fifo0_next and dst_fifo0_bits are only used when the"
	echo "     visible part of the bit FIFO is not guaranteed to always"
	echo "     correspond to an aligned 32-bit word in the image buffer."
	echo "     dst_fifo0_next is the invisible part of the bit FIFO,"
	echo "     and dst_fifo0_bits tracks the total number of bits in the"
	echo "     invisible part of the FIFO. */"
	echo "  tme_uint32_t *dst_raw0;"
	echo "  tme_uint32_t dst_fifo0, dst_fifo0_next;"
	echo "  unsigned int dst_fifo0_bits;"

	if test ${scale} = _d_; then
	    echo ""
	    echo "  /* since this function ${scale_name} the image, declare"
	    echo "     the destination secondary bit FIFO.  these variables work"
	    echo "     in exactly the same way that their primary counterparts"
	    echo "     do: */"
	    echo "  tme_uint32_t *dst_raw1;"
	    echo "  tme_uint32_t dst_fifo1, dst_fifo1_next;"
	    echo "  unsigned int dst_fifo1_bits;"
	fi

	echo ""
	echo "  /* declare src_off and dst_off.  these are used when priming a"
	echo "     source or destination bit FIFO, to identify an initial aligned"
	echo "     32-bit word in the source or destination image buffer, and an"
	echo "     initial bit offset within that word: */"
	echo "  unsigned int src_off, dst_off;"

	echo ""
	echo "  /* declare src_fifo0_may_be_unaligned.  this is zero iff all"
	echo "     aligned 32-bit words in the source buffer contain a whole"
	echo "     number of displayed pixels.  this is *not* so if the source"
	echo "     buffer is not packed, or if there are 24 bits per pixel -"
	echo "     in this last case, it is possible for a pixel value to cross"
	echo "     a 32-bit boundary: */"
	value="(!src_packed || (src_bipp == 24))"
	if $src_bypl_var; then
	    echo "  const unsigned int src_fifo0_may_be_unaligned = ${value};"
	else
	    echo "#define src_fifo0_may_be_unaligned ${value}"
	    undef_macros="${undef_macros} src_fifo0_may_be_unaligned"
	fi

	if test $scale = _h_; then
	    echo ""
	    echo "  /* declare src_fifo1_may_be_unaligned.  this is zero iff all"
	    echo "     aligned 32-bit words in the source buffer contain a"
	    echo "     whole number of displayed pixels, *and* for every aligned"
	    echo "     32-bit word on one scanline all other scanlines have an"
	    echo "     aligned 32-bit word corresponding to the same x coordinate: */"
	    value="(src_fifo0_may_be_unaligned || (src_bypl & 3))"
	    if $src_bypl_var; then
		echo "  const unsigned int src_fifo1_may_be_unaligned = ${value};"
	    else
		echo "#define src_fifo1_may_be_unaligned ${value}"
		undef_macros="${undef_macros} src_fifo1_may_be_unaligned"
	    fi
	fi

	echo ""
	echo "  /* declare dst_fifo0_may_be_unaligned.  this is zero iff"
	echo "     src_fifo0_may_be_unaligned is zero, dst_bypl is 32-bit aligned"
	echo "     and we can guarantee that any initial dst_x will correspond to"
	echo "     an aligned 32-bit word in the destination buffer.  the motivation"
	echo "     is to only prime and shift the destination primary bit FIFO if"
	echo "     absolutely necessary."
	echo ""
	echo "     since we require that src_fifo0_may_be_unaligned is zero, we"
	echo "     know that the initial src_x = (Z * 32) / src_bipp for "
	echo "     some Z.  we also have the initial dst_x = src_x${scale_math}."
	echo "     the initial destination bit offset will then be:"
	echo ""
	echo "     (dst_skipx + dst_x) * dst_bipp"
	echo "     = (dst_skipx * dst_bipp) + (dst_x * dst_bipp)"
	echo ""
	echo "     if we additionally require that (dst_skipx * dst_bipp)"
	echo "     be 32-bit aligned, this reduces things to:"
	echo ""
	echo "     dst_x * dst_bipp"
	echo "     = (src_x${scale_math}) * dst_bipp"
	echo "     = (((Z * 32) / src_bipp)${scale_math}) * dst_bipp"
	echo ""
	echo "     which will be a multiple of 32 iff:"
	echo ""
	echo "      ((1 / src_bipp)${scale_math}) * dst_bipp >= 1 and integral"
	echo "  */"
	denom="src_bipp"
	numer="dst_bipp"
	case $scale in
	_) ;;
	_h_) denom="(${denom} * 2)" ;;
	_d_) numer="(${numer} * 2)" ;;
	esac
	value="(src_fifo0_may_be_unaligned || !dst_packed || (dst_bipp == 24) || (dst_bypl % 4) || ((dst_skipx * dst_bipp) % 32) || (${numer} % ${denom}))"
	if $src_bypl_var || $dst_bypl_var; then
	    echo "  const unsigned int dst_fifo0_may_be_unaligned = ${value};"
	else
	    echo "#define dst_fifo0_may_be_unaligned ${value}"
	    undef_macros="${undef_macros} dst_fifo0_may_be_unaligned"
	fi

	if test $scale = _d_; then
	    echo ""
	    echo "  /* declare dst_fifo1_may_be_unaligned.  this is zero iff all"
	    echo "     32-bit aligned values in the destination buffer contain a"
	    echo "     whole number of displayed pixels, and for every 32-bit"
	    echo "     aligned value on one scanline all other scanlines have a"
	    echo "     32-bit aligned value corresponding to the same x coordinate: */"
	    value="(dst_fifo0_may_be_unaligned || (dst_bypl & 3))"
	    if $src_bypl_var || $dst_bypl_var; then
		echo "  const unsigned int dst_fifo1_may_be_unaligned = ${value};"
	    else
		echo "#define dst_fifo1_may_be_unaligned ${value}"
		undef_macros="${undef_macros} dst_fifo1_may_be_unaligned"
	    fi
	fi
	
	echo ""
	echo "  /* declare src_raw0_end.  when treating the source image as"
	echo "     an array of aligned 32-bit words, this variable holds the"
	echo "     address of the first word after the real source image."
	echo "     if the fast, aligned 32-bit word comparison loop passes"
	echo "     this point, the entire source image has been processed and"
 	echo "     the function terminates: */"
	echo "  const tme_uint32_t *src_raw0_end;"

	echo ""
	echo "  /* declare xlat_run.  see the comment for the TME_FB_XLAT_RUN"
	echo "     macro for an explanation of what this variable does: */"
	echo "  int xlat_run;"

	echo ""
	echo "  /* this silences gcc -Wuninitialized: */"
	echo "  src_fifo0_next = 0;"
	echo "  src_fifo0_bits = 0;"
	if test ${scale} = _h_; then
	    echo "  src_fifo1_next = 0;"
	    echo "  src_fifo1_bits = 0;"
	fi
	echo "  dst_fifo0_next = 0;"
	echo "  dst_fifo0_bits = 0;"
	if test ${scale} = _d_; then
	    echo "  dst_fifo1_next = 0;"
	    echo "  dst_fifo1_bits = 0;"
	fi

	echo ""
	echo "  /* initialize src_raw0 and src_raw0_end for the fast aligned 32-bit"
	echo "     word comparison loop.  on entry to (and when continuing) that loop,"
	echo "     src_raw0 always points to the aligned 32-bit word *before* the"
	echo "     next word to check.  src_raw0_end always points after the last"
	echo "     word to check."
	echo ""
	echo "     src_raw0 is actually part of the source primary bit FIFO, which"
	echo "     is good, because when the fast comparison fails on a word, src_raw0"
	echo "     is already primed and ready to work for that bit FIFO: */"
	echo "  src_raw0 = ((const tme_uint32_t *) src->tme_fb_connection_buffer) - 1;"
	echo "  src_raw0_end = (const tme_uint32_t *) (src->tme_fb_connection_buffer + src_bypb_real);"

	echo ""
	echo "  /* initialize xlat_run to -1.  it can never go negative inside the"
	echo "     pixel translation loop, so if xlat_run stays negative for the"
	echo "     entire translation, it means that the source image hasn't changed"
	echo "     since the last translation.  this information is returned to the"
	echo "     caller to hopefully save more work in updating the display: */"
	echo "  xlat_run = -1;"
		
	echo ""
	echo "  /* this is the main translation loop, which contains the fast aligned"
	echo "     32-bit word comparison loop, and the pixel translation loop: */"
	echo "  for (;;) {"

	echo ""
	echo "    /* this is the fast aligned 32-bit word comparison loop.  it"
	echo "       terminates either when a word fails comparison, or when the"
	echo "       entire source image has been compared.  the if test that"
	echo "       follows checks for the latter case and breaks the main"
	echo "       translation loop: */"
	echo "    for (; (++src_raw0 < src_raw0_end"
	echo "            && *src_raw0 == *TME_FB_XLAT_SRC_OLD(src_raw0)); );"
	echo "    if (src_raw0 >= src_raw0_end) {"
	echo "      break;"
	echo "    }"

	echo ""
	echo "    /* calculate the byte offset into the source buffer of the"
	echo "       32-bit word that failed comparison: */"
	echo "    src_off = ((tme_uint8_t *) src_raw0) - src->tme_fb_connection_buffer;"

	echo ""
	echo "    /* calculate the source y pixel coordinate, and reduce"
	echo "       src_off from the byte offset into the buffer to the"
	echo "       byte offset into that scanline: */"
	echo "    src_y = src_off / src_bypl;"
	echo "    src_off = src_off % src_bypl;"

	echo ""
	echo "    /* while translating pixels, we use one or more \"bit FIFOs\","
	echo "       each composed of one or more 32-bit integers.  we load these"
	echo "       FIFOs 32 bits at a time. */"
	echo ""
	echo "    /* prime the visible part of the source primary bit FIFO: */"
	echo "    src_fifo0 = *src_raw0;"
	echo "    *TME_FB_XLAT_SRC_OLD(src_raw0) = src_fifo0;"
	echo "    src_raw0++;"
	echo "    src_fifo0 = ((src_order == TME_ENDIAN_BIG)"
	echo "                 ? tme_betoh_u32(src_fifo0)"
	echo "                 : tme_letoh_u32(src_fifo0));"
	echo ""
	echo "    /* if the source primary bit FIFO may be unaligned: */"
	echo "    if (src_fifo0_may_be_unaligned) {"
	echo ""
	echo "      /* prime the invisible part of the source primary bit FIFO and"
	echo "         assume that we will not have to shift it to finish: */"
	echo "      src_fifo0_next = *src_raw0;"
	echo "      *TME_FB_XLAT_SRC_OLD(src_raw0) = src_fifo0_next;"
	echo "      src_raw0++;"
	echo "      src_fifo0_next = ((src_order == TME_ENDIAN_BIG)"
	echo "                        ? tme_betoh_u32(src_fifo0_next)"
	echo "                        : tme_letoh_u32(src_fifo0_next));"
	echo "      src_fifo0_bits = 0;"
	echo ""
	echo "      /* if there are pixels that need to be skipped, the first 32 bits"
	echo "         we loaded into the FIFO may have first bits that belong to"
	echo "         those undisplayed (skipped) pixels.  it is *not* possible for"
	echo "         it to have first bits that belong to the scanline pad; there"
	echo "         might be pad bits in the *middle* of the first 32 bits, but any"
	echo "         first bits *must* belong to pixels, displayed or not: */"
	echo "      if (src_skipx > 0"
	echo "          && (src_off * 8) < (src_skipx * src_bipp)) {"
	echo ""
	echo "        /* see how many bits we will need to skip: */"
	echo "        src_fifo0_bits = (src_skipx * src_bipp) - (src_off * 8);"
	echo ""
	echo "        /* if it is more than 31 bits, this is an entire 32 bits of"
	echo "           undisplayed pixels.  just advance: */"
	echo "        if (src_fifo0_bits > 31) {"
	echo "          src_raw0--;"
	echo "          continue;"
	echo "        }"
	echo ""
	echo "        /* set the source x coordinate to zero: */"
	echo "        src_x = 0;"
	echo "      }"
	echo ""
	echo "      /* otherwise, the first 32 bits we load will have first bits for"
	echo "         a displayable pixel: */"
	echo "      else {"
	echo ""
	echo "        /* if the source bits per pixel is 24,  calculate the number of"
	echo "           bytes *before* the original src_raw0 of any split pixel, and"
	echo "           subtract this from src_off, to leave src_off as the byte offset"
	echo "           into the scanline of the beginning of a pixel: */"
	echo "        if (src_bipp == 24) {"
	echo "          src_fifo0_bits = (src_off % 3);"
	echo "          src_off -= src_fifo0_bits;"
	echo ""
	echo "          /* if this is a split pixel, we need to prime the source primary"
	echo "              bit FIFO starting with the part *before* the original src_raw0."
	echo "              we do not have to copy to the old; it passed comparison: */"
	echo "          if (src_fifo0_bits) {"
	echo "            src_raw0--;"
	echo "            src_fifo0_next = src_fifo0;"
	echo "            src_fifo0 = ((src_order == TME_ENDIAN_BIG)"
	echo "                         ? tme_betoh_u32(*(src_raw0 - 2))"
	echo "                         : tme_letoh_u32(*(src_raw0 - 2)));"
	echo "          }"
	echo "        }"
	echo ""
	echo "        /* calculate the source x coordinate: */"
	echo "        src_x = ((src_off * 8) / src_bipp) - src_skipx;"
	echo "      }"
	echo ""
	echo "      /* do any shifting to finish priming the source primary FIFO: */"
	echo "      if (src_fifo0_bits) {"
	echo "        if (src_order == TME_ENDIAN_BIG) {"
	echo "          src_fifo0 = (src_fifo0 << src_fifo0_bits) | (src_fifo0_next >> (32 - src_fifo0_bits));"
	echo "          src_fifo0_next <<= src_fifo0_bits;"
	echo "        }"
	echo "        else {"
	echo "          src_fifo0 = (src_fifo0 >> src_fifo0_bits) | (src_fifo0_next << (32 - src_fifo0_bits));"
	echo "          src_fifo0_next >>= src_fifo0_bits;"
	echo "        }"
	echo "      }"
	echo "      src_fifo0_bits = 64 - src_fifo0_bits;"
	echo "    }"
	echo ""
	echo "    /* otherwise, the source primary FIFO is aligned: */"
	echo "    else {"
	echo "      src_x = ((src_off * 8) / src_bipp) - src_skipx;"
	echo "    }"

	if test $scale = _h_; then
	    echo ""
	    echo "    /* when halving the image, we have a source secondary bit "
	    echo "       FIFO, providing pixel values at the same source x coordinate"
	    echo "       but on the \"other\" line.  prime the source secondary"
	    echo "       bit FIFO: */"
	    echo "    if (src_fifo1_may_be_unaligned) {"
	    echo ""
	    echo "      /* calculate the bit offset into the source buffer of"
	    echo "         this exact same pixel on the other line: */"
	    echo "      src_off = ((src_y ^ 1) * src_bypl * 8) + ((src_skipx + src_x) * src_bipp);"
	    echo ""
	    echo "      /* calculate how many bits offset from a 32-bit boundary the pixel is: */"
	    echo "      src_fifo1_bits = src_off % 32;"
	    echo ""
	    echo "      /* set src_raw1: */"
	    echo "      src_raw1 = (const tme_uint32_t *)"
	    echo "        (src->tme_fb_connection_buffer"
	    echo "         + ((src_off - src_fifo1_bits) / 8));"
	    echo ""
	    echo "      /* actually prime the FIFO by loading the first two words and"
	    echo "         shifting off any offset bits: */"
	    echo "      src_fifo1 = *src_raw1;"
	    echo "      *TME_FB_XLAT_SRC_OLD(src_raw1) = src_fifo1;"
	    echo "      src_raw1++;"
	    echo "      src_fifo1_next = *src_raw1;"
	    echo "      *TME_FB_XLAT_SRC_OLD(src_raw1) = src_fifo1_next;"
	    echo "      src_raw1++;"
	    echo "      if (src_order == TME_ENDIAN_BIG) {"
	    echo "        src_fifo1 = tme_betoh_u32(src_fifo1);"
	    echo "        src_fifo1_next = tme_betoh_u32(src_fifo1_next);"
	    echo "        if (src_fifo1_bits) {"
	    echo "          src_fifo1 = (src_fifo1 << src_fifo1_bits) | (src_fifo1_next >> (32 - src_fifo1_bits));"
	    echo "          src_fifo1_next <<= src_fifo1_bits;"
	    echo "        }"
	    echo "      }"
	    echo "      else {"
	    echo "        src_fifo1 = tme_letoh_u32(src_fifo1);"
	    echo "        src_fifo1_next = tme_letoh_u32(src_fifo1_next);"
	    echo "        if (src_fifo1_bits) {"
	    echo "          src_fifo1 = (src_fifo1 >> src_fifo1_bits) | (src_fifo1_next << (32 - src_fifo1_bits));"
	    echo "          src_fifo1_next >>= src_fifo1_bits;"
	    echo "        }"
	    echo "      }"
	    echo "      src_fifo1_bits = 64 - src_fifo1_bits;"
	    echo "    }"
	    echo ""
	    echo "    /* otherwise the source secondary FIFO is aligned: */"
	    echo "    else {"
	    echo "      src_raw1 = (const tme_uint32_t *)"
	    echo "        (((tme_uint8_t *) src_raw0)"
	    echo "         + (("
	    echo "             /* if src_y is even, this addend is now src_bypl * 4,"
	    echo "                else if src_y is odd, this addend is now src_bypl * 2: */"
	    echo "             ((src_bypl * 4) >> (src_y & 1))"
	    echo "             /* if src_y is even, this addend is now src_bypl,"
	    echo "                else if src_y is odd, this addend is now -src_bypl: */"
	    echo "             - (src_bypl * 3))"
	    echo "            /* this -4 compensates for src_raw0 already having"
	    echo "               been advanced by one: */"
	    echo "            - 4));"
	    echo "      src_fifo1 = *src_raw1;"
	    echo "      *TME_FB_XLAT_SRC_OLD(src_raw1) = src_fifo1;"
	    echo "      src_raw1++;"
	    echo "      src_fifo1 = ((src_order == TME_ENDIAN_BIG)"
	    echo "                   ? tme_betoh_u32(src_fifo1)"
	    echo "                   : tme_letoh_u32(src_fifo1));"
	    echo "    }"
	fi

	if test $scale != _; then
	    echo ""
	    echo "    /* calculate the destination coordinates: */"
	    echo "    dst_y = src_y${scale_math};"
	    echo "    dst_x = src_x${scale_math};"
	fi

	echo ""
	echo "    /* prime the destination primary bit FIFO: */"
	echo "    dst_fifo0 = 0;"
	echo "    if (dst_fifo0_may_be_unaligned) {"
	echo ""
	echo "      /* calculate the bit offset into the destination buffer of"
	echo "         the destination pixel: */"
	echo "      dst_off = (dst_y * dst_bypl * 8) + ((dst_skipx + dst_x) * dst_bipp);"
	echo ""
	echo "      /* calculate the number of bits that will be in the primed FIFO: */"
	echo "      dst_fifo0_bits = dst_off % 32;"
	echo ""
	echo "      /* set dst_raw0: */"
	echo "      dst_raw0 = (tme_uint32_t *)"
	echo "        (dst->tme_fb_connection_buffer"
	echo "         + ((dst_off - dst_fifo0_bits) / 8));"
	echo ""
	echo "      /* prime the primary destination FIFO: */"
	echo "      dst_fifo0_next = 0;"
	echo "      if (dst_fifo0_bits) {"
	echo "        dst_fifo0_next = (src_order == TME_ENDIAN_BIG"
	echo "                          ? (tme_betoh_u32(*dst_raw0) & (0xffffffffUL << (32 - dst_fifo0_bits)))"
	echo "                          : (tme_letoh_u32(*dst_raw0) & (0xffffffffUL >> (32 - dst_fifo0_bits))));"
	echo "      }"
	echo "    }"
	echo ""
	echo "    /* otherwise the destination primary FIFO is aligned: */"
	echo "    else {"
	echo "      dst_off = (dst_y * dst_bypl) + (((dst_skipx + dst_x) * dst_bipp) / 8);"
	echo "      dst_raw0 = (tme_uint32_t *) (dst->tme_fb_connection_buffer + dst_off);"
	echo "    }"

	if test $scale = _d_; then
	    echo ""
	    echo "    /* when doubling the image, we have a destination secondary bit "
	    echo "       FIFO, for pixel values at the same source x coordinate"
	    echo "       but on the \"other\" line.  prime the destination secondary"
	    echo "       bit FIFO: */"
	    echo "    dst_fifo1 = 0;"
	    echo "    if (dst_fifo1_may_be_unaligned) {"
	    echo ""
	    echo "      /* calculate the bit offset into the destination buffer of"
	    echo "         the destination pixel: */"
	    echo "      dst_off = ((dst_y + 1) * dst_bypl * 8) + ((dst_skipx + dst_x) * dst_bipp);"
	    echo ""
	    echo "      /* calculate the number of bits that will be in the primed FIFO: */"
	    echo "      dst_fifo1_bits = dst_off % 32;"
	    echo ""
	    echo "      /* set dst_raw1: */"
	    echo "      dst_raw1 = (tme_uint32_t *)"
	    echo "        (dst->tme_fb_connection_buffer"
	    echo "         + ((dst_off - dst_fifo1_bits) / 8));"
	    echo ""
	    echo "      /* prime the primary destination FIFO: */"
	    echo "      dst_fifo1_next = 0;"
	    echo "      if (dst_fifo1_bits) {"
	    echo "        dst_fifo1_next = (src_order == TME_ENDIAN_BIG"
	    echo "                          ? (tme_betoh_u32(*dst_raw1) & (0xffffffffUL << (32 - dst_fifo1_bits)))"
	    echo "                          : (tme_letoh_u32(*dst_raw1) & (0xffffffffUL >> (32 - dst_fifo1_bits))));"
	    echo "      }"
	    echo "    }"
	    echo ""
	    echo "    /* otherwise, the destination secondary FIFO is aligned: */"
	    echo "    else {"
	    echo "      dst_raw1 = (tme_uint32_t *)"
	    echo "        (((tme_uint8_t *) dst_raw0)"
	    echo "         + dst_bypl);"
	    echo "    }"
	fi

	# we want to unroll the pixel translation loop to read as many
	# source pixels as possible out of the source bit FIFOs and
	# write as many destination pixels as possible into the
	# destination bit FIFOs before shifting them.
	#
	# it is very common to want to unroll the translation loop a
	# different number of times on source and destination pixels,
	# so we always unroll the maximum possible, tracking within
	# each unrolled iteration the relative unrolled iteration for
	# the source and destination bit FIFOs.
	# 

	# src_unroll and dst_unroll are the number of times we will
	# unroll the translation loop on source and destination
	# pixels, respectively.  assume no unrolling:
	#
	src_unroll=1
	dst_unroll=1

	# src_iter_scale is the number of source pixels read out of a
	# source bit FIFO in one unrolled iteration of the translation
	# loop.  if this function halves the source image, this is
	# two, otherwise this is one.
	#
	# dst_iter_scale is the number of destination pixels written
	# to a destination bit FIFO in one unrolled iteration of the
	# translation loop.  if this function doubles the source
	# image, this is two, otherwise this is one.
	#
	# src_fifo_shift is the number of bits that the source bit
	# FIFO(s) must be shifted after all unrolled iterations on
	# source pixels.  assume no unrolling, so if this function
	# halves the source image and src_bipp is less than 24, two
	# pixels' bits must be shifted, otherwise one pixel's bits
	# must be shifted.
	#
	# dst_fifo_shift is the number of bits that the destination bit
	# FIFO(s) must be shifted after all unrolled iterations on
	# destination pixels.  we are assuming no unrolling, so if this
	# function doubles the source image and dst_bipp is less than
	# 24, two pixels' bits must be shifted, otherwise one pixel's
	# bits must be shifted:
	#
	if test $scale = _h_; then
	    src_iter_scale=2
	    src_fifo_shift="(src_bipp < 24 ? (src_bipp * 2) : src_bipp)"
	else
	    src_iter_scale=1
	    src_fifo_shift="src_bipp"
	fi
	if test $scale = _d_; then
	    dst_iter_scale=2
	    dst_fifo_shift="(dst_bipp < 24 ? (dst_bipp * 2) : dst_bipp)"
	else
	    dst_iter_scale=1
	    dst_fifo_shift="dst_bipp"
	fi

	# if src_bipp is known now, see how many times we can unroll the
	# translation loop on source pixels:
	#
	if test ${src_bipp} != 0; then

	    # in general, we can unroll once for each pixel in the
	    # visible part of a source bit FIFO:
	    #
	    src_unroll=`expr 32 / ${src_bipp}`

	    # if this function halves the source image, we can unroll
	    # half as many times:
	    #
	    src_unroll=`expr ${src_unroll} / ${src_iter_scale}`

	    # if we think we can unroll zero times, this means that
	    # src_bipp is greater than 16 and this function halves the
	    # source image - the zero is telling us that two whole
	    # pixels cannot fit in the visible part of a source bit
	    # FIFO.  so we unroll once on source pixels, and shift
	    # only one pixel's bits after the unrolling (the halving
	    # code will be shifting one pixel's worth of bits to get
	    # to the next pixel it has to read):
	    #
	    if test ${src_unroll} = 0; then
		src_unroll=1
		src_fifo_shift=${src_bipp}

	    # otherwise, we will be shifting the entire visible part
	    # of the source bit FIFO(s) after all unrolled iterations:
	    #
	    else
		src_fifo_shift=32
	    fi

	    echo ""
	    echo "    /* since src_bipp is known at code-generation time, the"
	    echo "       pixel translation loop is unrolled to translate all"
	    echo "       source pixels in the 32-bit visible part of the source"
	    echo "       bit FIFO(s) before shifting."
	    echo ""
	    echo "       in this case, src_bipp is known to be ${src_bipp}, so "`expr ${src_unroll} \* ${src_iter_scale}`" pixels will"
	    echo "       be read out of the source bit FIFO(s) before shifting, and"
	    echo "       when the source bit FIFO(s) are shifted, they are shifted"
	    echo "       ${src_fifo_shift} bits at a time: */"
	fi
	
	# if dst_bipp is known now, see how many times we can unroll the
	# translation loop on destination pixels:
	#
	if test ${dst_bipp} != 0; then

	    # in general, we can unroll once for each pixel in the
	    # visible part of a destination bit FIFO:
	    #
	    dst_unroll=`expr 32 / ${dst_bipp}`

	    # if this function doubles the source image, we can unroll
	    # half as many times:
	    #
	    dst_unroll=`expr ${dst_unroll} / ${dst_iter_scale}`

	    # if we think we can unroll zero times, this means that
	    # dst_bipp is greater than 16 and this function doubles the
	    # source image - the zero is telling us that two whole
	    # pixels cannot fit in the visible part of a destination bit
	    # FIFO.  so we unroll once on destination pixels, and shift
	    # only one pixel's bits after the unrolling (the doubling
	    # code will be shifting one pixel's worth of bits to get
	    # out the first pixel it has to write):
	    #
	    if test ${dst_unroll} = 0; then
		dst_unroll=1
		dst_fifo_shift=${dst_bipp}

	    # otherwise, we will be shifting the entire visible part
	    # of the source bit FIFO(s) after all unrolled iterations:
	    #
	    else
		dst_fifo_shift=32
	    fi

	    echo ""
	    echo "    /* since dst_bipp is known at code-generation time, the pixel"
	    echo "       translation loop is unrolled to translate all destination"
	    echo "       pixels in the 32-bit visible part of the destination bit"
	    echo "       FIFO(s) before shifting."
	    echo ""
	    echo "       in this case, dst_bipp is known to be ${dst_bipp}, so "`expr ${dst_unroll} \* ${dst_iter_scale}`" pixels will"
	    echo "       be written into the destination bit FIFO(s) before shifting,"
	    echo "       and when the destination bit FIFO(s) are shifted, they are"
	    echo "       shifted ${dst_fifo_shift} bits at a time: */"
	fi

	# unroll the translation loop the maximum number amount of times:
	#
	if test `expr ${src_unroll} \> ${dst_unroll}` = 1; then
	    unroll=${src_unroll}
	else
	    unroll=${dst_unroll}
	fi
	
	echo ""
	echo "    /* src_unroll = ${src_unroll}, src_iter_scale = ${src_iter_scale}"
	echo "       dst_unroll = ${dst_unroll}, dst_iter_scale = ${dst_iter_scale} */"
	echo "    for (xlat_run = TME_FB_XLAT_RUN;"
	echo "         xlat_run > 0; ) {"
	iter=0

	while test `expr ${iter} \< ${unroll}` = 1; do
	    echo ""
	    echo "      /* iter #${iter} */"
	    echo ""
	    iter_next=`expr ${iter} + 1`

	    # the number of bits to skip in a source FIFO to get to
	    # this iteration's pixel:
	    src_shift=`expr ${iter} % ${src_unroll}`
	    src_shift='('`expr ${src_shift} \* ${src_iter_scale}`' * src_bipp)'

	    echo "      /* get a pixel from the source primary FIFO: */"
	    echo "      pixel ="
	    echo "        ((src_fifo0"
	    echo "          >> (src_order == TME_ENDIAN_BIG"
	    echo "              ? (32 - (src_bipp + ${src_shift}))"
	    echo "              : ${src_shift}))"
	    echo "         & src_mask);"
	    
	    if test $scale = _; then

		echo ""
		echo "      /* map the pixel value from source to destination: */"
		echo "      if (src_depth == 1) {"
		echo "        pixel = dst->tme_fb_connection_depth1_map[pixel];"
		echo "      }"

	    elif test $scale = _h_; then

		echo ""
		echo "      /* get a second pixel, from the source secondary FIFO: */"
		if test $src_depth != 1; then
		    echo "      src_pixel1 ="
		else
		    echo "      pixel +="
		fi
		echo "        ((src_fifo1"
		echo "          >> (src_order == TME_ENDIAN_BIG"
		echo "              ? (32 - (src_bipp + ${src_shift}))"
		echo "              : ${src_shift}))"
		echo "         & src_mask);"

		echo ""
		echo "      /* get third and fourth pixels, from the source primary"
		echo "         FIFO and source secondary FIFO, respectively.  if"
		echo "         src_bipp is 24 or greater, these pixels are not yet"
		echo "         entirely in the first parts of the FIFOs, so we need"
		echo "         to shift: */"
		echo "      if (src_bipp >= 24) {"
		echo "        TME_FB_XLAT_SHIFT_SRC(src_fifo0_may_be_unaligned,"
		echo "                              src_fifo0,"
		echo "                              src_fifo0_next,"
		echo "                              src_fifo0_bits,"
		echo "                              src_bipp,"
		echo "                              src_raw0,"
		echo "                              src_order);"
		echo "        TME_FB_XLAT_SHIFT_SRC(src_fifo1_may_be_unaligned,"
		echo "                              src_fifo1,"
		echo "                              src_fifo1_next,"
		echo "                              src_fifo1_bits,"
		echo "                              src_bipp,"
		echo "                              src_raw1,"
		echo "                              src_order);"
		if test $src_depth != 1; then
		    echo "        src_pixel2 ="
		else
		    echo "        pixel +="
		fi
		echo "          ((src_fifo0"
		echo "            >> (src_order == TME_ENDIAN_BIG"
		echo "                ? (32 - (src_bipp + ${src_shift}))"
		echo "                : ${src_shift}))"
		echo "           & src_mask);"
		if test $src_depth != 1; then
		    echo "        src_pixel3 ="
		else
		    echo "        pixel +="
		fi
		echo "          ((src_fifo1"
		echo "            >> (src_order == TME_ENDIAN_BIG"
		echo "                ? (32 - (src_bipp + ${src_shift}))"
		echo "                : ${src_shift}))"
		echo "           & src_mask);"
		echo "      }"
		echo ""
		echo "      /* otherwise, src_pixel2 and src_pixel3 are already in the"
		echo "         visible parts of the source bit FIFOs; we just have to"
		echo "         reach over the pixels we already read to get at them: */"
		echo "      else {"
		if test $src_depth != 1; then
		    echo "        src_pixel2 ="
		else
		    echo "        pixel +="
		fi
		echo "          ((src_fifo0"
		echo "            >> (src_order == TME_ENDIAN_BIG"
		echo "                ? (32 - (src_bipp + (${src_shift} + src_bipp)))"
		echo "                : (${src_shift} + src_bipp)))"
		echo "           & src_mask);"
		if test $src_depth != 1; then
		    echo "        src_pixel3 ="
		else
		    echo "        pixel +="
		fi
		echo "          ((src_fifo1"
		echo "            >> (src_order == TME_ENDIAN_BIG"
		echo "                ? (32 - (src_bipp + (${src_shift} + src_bipp)))"
		echo "                : (${src_shift} + src_bipp)))"
		echo "           & src_mask);"
		echo "      }"

		echo ""
		echo "      /* map all four src_pixels into dst_pixel."
		echo "         if src_depth is one, this is simple - add all of the"
		echo "         single-bit pixel values together and look up the"
		echo "         destination pixel value for that intensity: */"
		if test $src_depth = 1; then
		    echo "      pixel = dst->tme_fb_connection_depth1_map[pixel];"
		else
		    echo "      if (src_depth == 1) {"
		    echo "        pixel = dst->tme_fb_connection_depth1_map[pixel"
		    echo "                                                  + src_pixel1"
		    echo "                                                  + src_pixel2"
		    echo "                                                  + src_pixel3];"
		    echo "      }"
		    echo ""
		    echo "      /* otherwise, this is trickier - we need to get the"
		    echo "         R, G, and B values for all four pixels, average"
		    echo "         them together, and look up the (closest) destination"
		    echo "         pixel value for the resulting color: */"
		    echo "      else {"
		    echo "        /* TBD: */"
		    echo "        abort();"
		    echo "      }"
		fi
	    fi

	    echo ""
	    if test $scale = _h_; then

		echo "      /* if we just read the last pixels on these"
		echo "         source scanlines: */"
		if test `expr ${iter_next} % ${src_unroll}` = 0; then
		    echo "      if ((src_x +="
		    echo "           (src_packed"
		    echo "            ? `expr ${src_unroll} \* 2`"
		    echo "            : 2))"
		    echo "           == src_width) {"
		else
		    echo "      if (!src_packed"
		    echo "          && (src_x += 2) == src_width) {"
		fi
		echo ""
		echo "        /* we need to rapidly shift the source FIFOs"
		echo "           to skip not only pad bits and undisplayed"
		echo "           pixels on the next line, but actually the"
		echo "           *entire* next line."
		echo ""
		echo "           note that this sounds like when we're done,"
		echo "           the bits at the fronts of the FIFOs will be"
		echo "           the *first* pixels on the next scanlines."
		echo ""
		echo "           but the bits at the fronts of the FIFOs now"
		echo "           are the *last* pixels on the current scanlines -"
		echo "           they haven't been shifted off yet.  so when"
		echo "           we're done, we want one pixel's worth of bits"
		echo "           in the FIFOs before the first pixel on the"
		echo "           next scanlines: */"
		echo ""
		echo "        /* calculate the number of bits that we need"
		echo "           to shift the source primary FIFO, after"
		echo "           discarding any bits in it now: */"
		echo "        src_off = ((src_bypl * 8) - (src_width * src_bipp)) + (src_bypl * 8);"
		echo "        src_off -= (src_fifo0_may_be_unaligned"
		echo "                    ? src_fifo0_bits"
		echo "                    : 32);"
		echo ""
		echo "        /* rapidly advance src_raw0 by the number of"
		echo "           whole 32-bit words.  this will leave it pointing"
		echo "           to the 32-bit word that has the first bit that"
		echo "           we want to end up as the first bit in the source"
		echo "           primary FIFO: */"
		echo "        src_raw0 += (src_off / 32);"
		echo ""
		echo "        /* reprime the source primary FIFO: */"
		echo "        src_fifo0 = *src_raw0;"
		echo "        *TME_FB_XLAT_SRC_OLD(src_raw0) = src_fifo0;"
		echo "        src_raw0++;"
		echo "        src_fifo0 = ((src_order == TME_ENDIAN_BIG)"
		echo "                     ? tme_betoh_u32(src_fifo0)"
		echo "                     : tme_letoh_u32(src_fifo0));"
		echo ""
		echo "        /* if the source primary FIFO may be unaligned: */"
		echo "        if (src_fifo0_may_be_unaligned) {"
		echo ""
		echo "          /* reprime the top half of the FIFO, leaving a"
		echo "             total of 64 bits in it: */"
		echo "          src_fifo0_next = *src_raw0;"
		echo "          *TME_FB_XLAT_SRC_OLD(src_raw0) = src_fifo0_next;"
		echo "          src_raw0++;"
		echo "          src_fifo0_next = ((src_order == TME_ENDIAN_BIG)"
		echo "                            ? tme_betoh_u32(src_fifo0_next)"
		echo "                            : tme_letoh_u32(src_fifo0_next));"
		echo "          src_fifo0_bits = 64;"
		echo ""
		echo "          /* if we have to shift off bits left over"
		echo "             from rapidly advancing whole 32-bit words: */"
		echo "          src_off %= 32;"
		echo "          if (src_off > 0) {"
		echo "            TME_FB_XLAT_SHIFT_SRC(src_fifo0_may_be_unaligned,"
		echo "                                  src_fifo0,"
		echo "                                  src_fifo0_next,"
		echo "                                  src_fifo0_bits,"
		echo "                                  src_off,"
		echo "                                  src_raw0,"
		echo "                                  src_order);"
		echo "          }"
		echo "        }"
		echo ""
		echo "        /* otherwise, the source primary FIFO is always aligned: */"
		echo "        else {"
		echo "          assert ((src_off % 32) == 0);"
		echo "        }"
		echo ""
		echo "        /* calculate the number of bits that we need"
		echo "           to shift the source secondary FIFO, after"
		echo "           discarding any bits in it now: */"
		echo "        src_off = ((src_bypl * 8) - (src_width * src_bipp)) + (src_bypl * 8);"
		echo "        src_off -= (src_fifo1_may_be_unaligned"
		echo "                    ? src_fifo1_bits"
		echo "                    : 32);"
		echo ""
		echo "        /* rapidly advance src_raw1 by the number of"
		echo "           whole 32-bit words.  this will leave it pointing"
		echo "           to the 32-bit word that has the first bit that"
		echo "           we want to end up as the first bit in the source"
		echo "           secondary FIFO: */"
		echo "        src_raw1 += (src_off / 32);"
		echo ""
		echo "        /* reprime the source secondary FIFO: */"
		echo "        src_fifo1 = *src_raw1;"
		echo "        *TME_FB_XLAT_SRC_OLD(src_raw1) = src_fifo1;"
		echo "        src_raw1++;"
		echo "        src_fifo1 = ((src_order == TME_ENDIAN_BIG)"
		echo "                     ? tme_betoh_u32(src_fifo1)"
		echo "                     : tme_letoh_u32(src_fifo1));"
		echo ""
		echo "        /* if the source secondary FIFO may be unaligned: */"
		echo "        if (src_fifo1_may_be_unaligned) {"
		echo ""
		echo "          /* reprime the top half of the FIFO, leaving a"
		echo "             total of 64 bits in it: */"
		echo "          src_fifo1_next = *src_raw1;"
		echo "          *TME_FB_XLAT_SRC_OLD(src_raw1) = src_fifo1_next;"
		echo "          src_raw1++;"
		echo "          src_fifo1_next = ((src_order == TME_ENDIAN_BIG)"
		echo "                            ? tme_betoh_u32(src_fifo1_next)"
		echo "                            : tme_letoh_u32(src_fifo1_next));"
		echo "          src_fifo1_bits = 64;"
		echo ""
		echo "          /* if we have to shift off bits left over"
		echo "             from rapidly advancing whole 32-bit words: */"
		echo "          src_off %= 32;"
		echo "          if (src_off > 0) {"
		echo "            TME_FB_XLAT_SHIFT_SRC(src_fifo1_may_be_unaligned,"
		echo "                                  src_fifo1,"
		echo "                                  src_fifo1_next,"
		echo "                                  src_fifo1_bits,"
		echo "                                  src_off,"
		echo "                                  src_raw1,"
		echo "                                  src_order);"
		echo "          }"
		echo "        }"
		echo ""
		echo "        /* otherwise, the source secondary FIFO is always aligned: */"
		echo "        else {"
		echo "          assert ((src_off % 32) == 0);"
		echo "        }"
		echo ""
		echo "        /* we are now on the first pixel of the next scanline: */"
		echo "        src_x = 0;"
		echo "      }"
	    else
		echo "      /* if the source buffer is not packed, and we just"
		echo "         read the last pixel on this source scanline: */"
		echo "      if (!src_packed"
		echo "          && ++src_x == src_width) {"
		echo ""
		echo "        /* calculate the number of bits between the"
		echo "           last bit of the last pixel and the first bit"
		echo "           of the first displayed pixel on the next"
		echo "           scanline.  this is equal to the number of"
		echo "           pad bits plus bits for undisplayed pixels: */"
		echo "        src_off = ((src_bypl * 8) - (src_width * src_bipp));"
		echo ""
		echo "        /* while there are bits to shift: */"
		echo "        for (; src_off > 0; src_off -= TME_MIN(src_off, 32)) {"
		echo ""
		echo "          /* shift the source primary FIFO: */"
		echo "          TME_FB_XLAT_SHIFT_SRC(src_fifo0_may_be_unaligned,"
		echo "                                src_fifo0,"
		echo "                                src_fifo0_next,"
		echo "                                src_fifo0_bits,"
		echo "                                TME_MIN(src_off, 32),"
		echo "                                src_raw0,"
		echo "                                src_order);"
		echo "        }"
		echo ""
		echo "        /* we are now on the first pixel of the next scanline: */"
		echo "        src_x = 0;"
		echo "      }"
	    fi

	    if test `expr ${iter_next} % ${src_unroll}` = 0; then
		echo ""
		echo "      /* shift the source primary FIFO: */"
		echo "      TME_FB_XLAT_SHIFT_SRC(src_fifo0_may_be_unaligned,"
		echo "                            src_fifo0,"
		echo "                            src_fifo0_next,"
		echo "                            src_fifo0_bits,"
		echo "                            ${src_fifo_shift},"
		echo "                            src_raw0,"
		echo "                            src_order);"
		if test $scale = _h_; then
		    echo ""
		    echo "      /* shift the source secondary FIFO: */"
		    echo "      TME_FB_XLAT_SHIFT_SRC(src_fifo1_may_be_unaligned,"
		    echo "                            src_fifo1,"
		    echo "                            src_fifo1_next,"
		    echo "                            src_fifo1_bits,"
		    echo "                            ${src_fifo_shift},"
		    echo "                            src_raw1,"
		    echo "                            src_order);"
		fi
		echo "          xlat_run--;"
	    fi
	    
	    # the number of bits to skip in a destination FIFO to get to
	    # this iteration's pixel:
	    dst_shift=`expr ${iter} % ${dst_unroll}`
	    dst_shift='('`expr ${dst_shift} \* ${dst_iter_scale}`' * dst_bipp)'

	    echo ""
	    echo "      /* put the pixel into the destination primary FIFO: */"
	    echo "      dst_fifo0 |="
	    echo "        (pixel"
	    echo "         << (dst_order == TME_ENDIAN_BIG"
	    echo "             ? ((32 - dst_bipp) - ${dst_shift})"
	    echo "             : ${dst_shift}));"

	    if test $scale = _d_; then

		echo ""
		echo "      /* put the pixel into the destination secondary FIFO: */"
		echo "      dst_fifo1 |="
		echo "        (pixel"
		echo "         << (dst_order == TME_ENDIAN_BIG"
		echo "             ? ((32 - dst_bipp) - ${dst_shift})"
		echo "             : ${dst_shift}));"

		echo ""
		echo "      /* put the pixel into both FIFOs again.  if"
		echo "         dst_bipp is 24 or greater, the FIFOs can"
		echo "         not entirely take these further pixels,"
		echo "         so we need to shift: */"
		echo "      if (dst_bipp >= 24) {"
		echo "        TME_FB_XLAT_SHIFT_DST(dst_fifo0_may_be_unaligned,"
		echo "                              dst_fifo0,"
		echo "                              dst_fifo0_next,"
		echo "                              dst_fifo0_bits,"
		echo "                              dst_bipp,"
		echo "                              dst_raw0,"
		echo "                              dst_order);"
		echo "        TME_FB_XLAT_SHIFT_DST(dst_fifo1_may_be_unaligned,"
		echo "                              dst_fifo1,"
		echo "                              dst_fifo1_next,"
		echo "                              dst_fifo1_bits,"
		echo "                              dst_bipp,"
		echo "                              dst_raw1,"
		echo "                              dst_order);"
		echo "        dst_fifo0 |="
		echo "          (pixel"
		echo "           << (dst_order == TME_ENDIAN_BIG"
		echo "               ? ((32 - dst_bipp) - ${dst_shift})"
		echo "               : ${dst_shift}));"
		echo "        dst_fifo1 |="
		echo "          (pixel"
		echo "           << (dst_order == TME_ENDIAN_BIG"
		echo "               ? ((32 - dst_bipp) - ${dst_shift})"
		echo "               : ${dst_shift}));"
		echo "      }"
		echo ""
		echo "      /* otherwise, the FIFOs can take these further pixels: */"
		echo "      else {"
		echo "        dst_fifo0 |="
		echo "          (pixel"
		echo "           << (dst_order == TME_ENDIAN_BIG"
		echo "               ? ((32 - dst_bipp) - (${dst_shift} + dst_bipp))"
		echo "               : (${dst_shift} + dst_bipp)));"
		echo "        dst_fifo1 |="
		echo "          (pixel"
		echo "           << (dst_order == TME_ENDIAN_BIG"
		echo "               ? ((32 - dst_bipp) - (${dst_shift} + dst_bipp))"
		echo "               : (${dst_shift} + dst_bipp)));"
		echo "      }"
	    fi

	    echo ""
	    echo "      /* if the destination buffer is not packed, and we just"
	    echo "         wrote the last pixel on this destination scanline: */"
	    if test $scale = _d_; then value=2; else value=1; fi
	    echo "      if (!dst_packed"
	    echo "          && (dst_x += ${value}) == dst_width) {"
	    echo ""
	    echo "        /* calculate the number of bits between the"
	    echo "           last bit of the last pixel and the first bit"
	    echo "           of the first displayed pixel on the next"
	    echo "           scanline.  this is equal to the number of"
	    echo "           pad bits plus bits for undisplayed pixels: */"
	    echo "        dst_off = ((dst_bypl * 8) - (dst_width * dst_bipp));"
	    echo ""
	    echo "        /* while there are bits to shift: */"
	    echo "        for (; dst_off > 0; dst_off -= TME_MIN(dst_off, 32)) {"
	    echo ""
	    echo "          /* shift the destination primary FIFO: */"
	    echo "          TME_FB_XLAT_SHIFT_DST(dst_fifo0_may_be_unaligned,"
	    echo "                                dst_fifo0,"
	    echo "                                dst_fifo0_next,"
	    echo "                                dst_fifo0_bits,"
	    echo "                                TME_MIN(dst_off, 32),"
	    echo "                                dst_raw0,"
	    echo "                                dst_order);"
	    if test $scale = _d_; then
		echo ""
		echo "          /* shift the destination secondary FIFO: */"
		echo "          TME_FB_XLAT_SHIFT_DST(dst_fifo1_may_be_unaligned,"
		echo "                                dst_fifo1,"
		echo "                                dst_fifo1_next,"
		echo "                                dst_fifo1_bits,"
		echo "                                TME_MIN(dst_off, 32),"
		echo "                                dst_raw1,"
		echo "                                dst_order);"
	    fi
	    echo "        }"
	    echo ""
	    echo "        /* we are now on the first pixel of the next scanline: */"
	    echo "        dst_x = 0;"
	    echo "      }"

	    if test `expr ${iter_next} % ${dst_unroll}` = 0; then
		echo ""
		echo "      /* shift the destination primary FIFO: */"
		echo "      TME_FB_XLAT_SHIFT_DST(dst_fifo0_may_be_unaligned,"
		echo "                            dst_fifo0,"
		echo "                            dst_fifo0_next,"
		echo "                            dst_fifo0_bits,"
		echo "                            ${dst_fifo_shift},"
		echo "                            dst_raw0,"
		echo "                            dst_order);"
		if test $scale = _d_; then
		    echo ""
		    echo "      /* shift the destination secondary FIFO: */"
		    echo "      TME_FB_XLAT_SHIFT_DST(dst_fifo1_may_be_unaligned,"
		    echo "                            dst_fifo1,"
		    echo "                            dst_fifo1_next,"
		    echo "                            dst_fifo1_bits,"
		    echo "                            ${dst_fifo_shift},"
		    echo "                            dst_raw1,"
		    echo "                            dst_order);"
		fi
	    fi

	    iter=${iter_next}
	done
	echo ""
	echo "    }"

	if test `expr ${iter} % ${dst_unroll}` != 0; then
	    echo "$PROG internal error - the last iter did not shift the destination FIFOs ($iter and $dst_unroll)" 1>&2
	    exit 1
	fi
	
	echo ""
	echo "    /* if the destination FIFOs may be unaligned, there"
	echo "       may be bits left in the FIFO that we need to flush: */"
	echo "    if (dst_fifo0_may_be_unaligned"
	echo "        && dst_fifo0_bits > 0) {"
	echo "      dst_fifo0 = *dst_raw0;"
	echo "      if (dst_order == TME_ENDIAN_BIG) {"
	echo "        dst_fifo0_next |= (tme_betoh_u32(dst_fifo0) & (0xffffffff >> dst_fifo0_bits));"
	echo "        dst_fifo0_next = tme_htobe_u32(dst_fifo0_next);"
	echo "      }"
	echo "      else {"
	echo "        dst_fifo0_next |= (tme_letoh_u32(dst_fifo0) & (0xffffffff << dst_fifo0_bits));"
	echo "        dst_fifo0_next = tme_htole_u32(dst_fifo0_next);"
	echo "      }"
	echo "      *dst_raw0 = dst_fifo0;"
	echo "    }"
	if test $scale = _d_; then
	    echo "    if (dst_fifo1_may_be_unaligned"
	    echo "        && dst_fifo1_bits > 0) {"
	    echo "      dst_fifo1 = *dst_raw1;"
	    echo "      if (dst_order == TME_ENDIAN_BIG) {"
	    echo "        dst_fifo1_next |= (tme_betoh_u32(dst_fifo1) & (0xffffffff >> dst_fifo1_bits));"
	    echo "        dst_fifo1_next = tme_htobe_u32(dst_fifo1_next);"
	    echo "      }"
	    echo "      else {"
	    echo "        dst_fifo1_next |= (tme_letoh_u32(dst_fifo1) & (0xffffffff << dst_fifo1_bits));"
	    echo "        dst_fifo1_next = tme_htole_u32(dst_fifo1_next);"
	    echo "      }"
	    echo "      *dst_raw1 = dst_fifo1;"
	    echo "    }"
	fi

	echo ""
	echo "    /* loop back to compare more 32-bit words: */"
	echo "    src_raw0--;"
	echo "  }"

	echo ""
	echo "  /* return nonzero iff we did some translating: */"
	echo "  return (xlat_run >= 0);"
	
	echo ""
	for macro in ${undef_macros}; do
	    echo "#undef ${macro}"
	done
	echo "}"

    done
done

echo ""
echo "/* the xlat function array: */"
echo "static const struct tme_fb_xlat tme_fb_xlats[] = {$xlat_array
};"

# done:
exit 0
