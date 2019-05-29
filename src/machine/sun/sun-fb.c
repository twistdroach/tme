/* $Id: sun-fb.c,v 1.1 2004/08/19 11:34:13 fredette Exp $ */

/* machine/sun/sun-fb.c - Sun framebuffer emulation support: */

/*
 * Copyright (c) 2004 Matt Fredette
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
_TME_RCSID("$Id: sun-fb.c,v 1.1 2004/08/19 11:34:13 fredette Exp $");

/* includes: */
#include "sun-fb.h"

/* this returns the P4 size value for the given resolution: */
tme_uint32_t 
tme_sun_fb_p4_size(const char *size)
{
  if (TME_ARG_IS(size, "1600x1280")) {
    return (TME_SUN_P4_SIZE_1600_1280);
  }
  else if (TME_ARG_IS(size, "1152x900")) {
    return (TME_SUN_P4_SIZE_1152_900);
  }
  else if (TME_ARG_IS(size, "1024x1024")) {
    return (TME_SUN_P4_SIZE_1024_1024);
  }
  else if (TME_ARG_IS(size, "1280x1024")) {
    return (TME_SUN_P4_SIZE_1280_1024);
  }
  else if (TME_ARG_IS(size, "1440x1440")) {
    return (TME_SUN_P4_SIZE_1440_1440);
  }
  else if (TME_ARG_IS(size, "640x480")) {
    return (TME_SUN_P4_SIZE_640_480);
  }
  return (TME_SUN_P4_SIZE_NULL);
}

/* this returns the width for the given P4 size value: */
tme_uint32_t
tme_sun_fb_p4_size_width(tme_uint32_t p4_size)
{
  switch (p4_size) {
  case TME_SUN_P4_SIZE_1600_1280: return(1600);
  case TME_SUN_P4_SIZE_1152_900: return(1152);
  case TME_SUN_P4_SIZE_1024_1024: return(1024);
  case TME_SUN_P4_SIZE_1280_1024: return(1280);
  case TME_SUN_P4_SIZE_1440_1440: return(1440);
  case TME_SUN_P4_SIZE_640_480: return(640);
  default: abort();
  }
}

/* this returns the height for the given P4 size value: */
tme_uint32_t
tme_sun_fb_p4_size_height(tme_uint32_t p4_size)
{
  switch (p4_size) {
  case TME_SUN_P4_SIZE_1600_1280: return(1280);
  case TME_SUN_P4_SIZE_1152_900: return(900);
  case TME_SUN_P4_SIZE_1024_1024: return(1024);
  case TME_SUN_P4_SIZE_1280_1024: return(1024);
  case TME_SUN_P4_SIZE_1440_1440: return(1440);
  case TME_SUN_P4_SIZE_640_480: return(480);
  default: abort();
  }
}
