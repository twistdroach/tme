/* $Id: sun-fb.h,v 1.1 2004/08/19 11:34:14 fredette Exp $ */

/* machine/sun/sun-fb.h - header file for Sun framebuffer emulation: */

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

#ifndef _MACHINE_SUN_FB_H
#define _MACHINE_SUN_FB_H

#include <tme/common.h>
_TME_RCSID("$Id: sun-fb.h,v 1.1 2004/08/19 11:34:14 fredette Exp $");

/* includes: */
#include <tme/generic/bus.h>

/* macros: */

/* P4 register framebuffer identifiers: */
#define TME_SUN_P4_ID_MASK		(0xff000000)
#define  TME_SUN_P4_ID_BWTWO		(0x00000000)
#define  TME_SUN_P4_ID_CGFOUR		(0x40000000)
#define  TME_SUN_P4_ID_CGEIGHT		(0x45000000)
#define  TME_SUN_P4_ID_CGSIX		(0x60000000)

/* P4 register framebuffer sizes: */
#define TME_SUN_P4_SIZE_NULL		(0xffffffff)
#define TME_SUN_P4_SIZE_MASK		(0x0f000000)
#define  TME_SUN_P4_SIZE_1600_1280	(0x00000000)
#define  TME_SUN_P4_SIZE_1152_900	(0x01000000)
#define  TME_SUN_P4_SIZE_1024_1024	(0x02000000)
#define  TME_SUN_P4_SIZE_1280_1024	(0x03000000)
#define  TME_SUN_P4_SIZE_1440_1440	(0x04000000)
#define  TME_SUN_P4_SIZE_640_480	(0x05000000)

/* P4 register bits: */
					/* 0x00000080 is the diagnostic bit (?) */
					/* 0x00000040 is the readback bit (?) */
#define TME_SUN_P4_REG_ENABLE_VIDEO	(0x00000020)
#define TME_SUN_P4_REG_SYNC_RAMDAC	(0x00000010)
#define TME_SUN_P4_REG_IN_VTRACE	(0x00000008)
#define TME_SUN_P4_REG_INT_ACTIVE	(0x00000004)
#define TME_SUN_P4_REG_INT_RESET	(0x00000004)
#define TME_SUN_P4_REG_ENABLE_INT	(0x00000002)
#define TME_SUN_P4_REG_IN_VTRACE_1H	(0x00000001)
#define TME_SUN_P4_REG_RESET		(0x00000001)

/* P4 register read-only bits: */
#define TME_SUN_P4_RO_MASK		(TME_SUN_P4_ID_MASK \
					 | TME_SUN_P4_SIZE_MASK \
					 | TME_SUN_P4_REG_IN_VTRACE \
					 | TME_SUN_P4_REG_INT_ACTIVE \
					 | TME_SUN_P4_REG_IN_VTRACE_1H)

/* offsets from a P4 register: */
#define	TME_SUN_P4_OFFSET_BWTWO		(0x00100000)

/* prototypes: */
tme_uint32_t tme_sun_fb_p4_size _TME_P((const char *));
tme_uint32_t tme_sun_fb_p4_size_width _TME_P((tme_uint32_t));
tme_uint32_t tme_sun_fb_p4_size_height _TME_P((tme_uint32_t));

#endif /* !_MACHINE_SUN_FB_H */
