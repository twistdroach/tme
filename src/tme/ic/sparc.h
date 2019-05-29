/* $Id: sparc.h,v 1.1 2006/09/30 12:56:01 fredette Exp $ */

/* tme/ic/sparc.h - public header file for SPARC emulation */

/*
 * Copyright (c) 2005 Matt Fredette
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

#ifndef _TME_IC_SPARC_H
#define _TME_IC_SPARC_H

#include <tme/common.h>
_TME_RCSID("$Id: sparc.h,v 1.1 2006/09/30 12:56:01 fredette Exp $");

/* includes: */
#include <tme/element.h>
#include <tme/generic/bus.h>

/* macros: */

/* the sparc32 address spaces: */
#define TME_SPARC32_ASI_UI			(0x08)
#define TME_SPARC32_ASI_SI			(0x09)
#define TME_SPARC32_ASI_UD			(0x0A)
#define TME_SPARC32_ASI_SD			(0x0B)

/* the sparc64 address spaces: */
#define TME_SPARC64_ASI_PRIMARY			(0x80)
#define TME_SPARC64_ASI_PRIMARY_LITTLE		(0x88)

/* the sparc address space masks: */
#define TME_SPARC_ASI_MASK(asi)			((tme_uint32_t) (asi))
#define TME_SPARC_ASI_MASK_WHICH(asi_mask)	((asi_mask) & 0xff)
#define _TME_SPARC_ASI_MASK_BIT(x)		(TME_BIT(16) | TME_BIT(15 - (x)))
#define TME_SPARC32_ASI_MASK_UI			_TME_SPARC_ASI_MASK_BIT(0)
#define TME_SPARC32_ASI_MASK_SI			_TME_SPARC_ASI_MASK_BIT(1)
#define TME_SPARC32_ASI_MASK_UD			_TME_SPARC_ASI_MASK_BIT(2)
#define TME_SPARC32_ASI_MASK_SD			_TME_SPARC_ASI_MASK_BIT(3)
#define TME_SPARC64_ASI_MASK_PRIMARY		_TME_SPARC_ASI_MASK_BIT(4)
#define TME_SPARC64_ASI_MASK_PRIMARY_LITTLE	_TME_SPARC_ASI_MASK_BIT(5)

/* this evaluates to nonzero iff the two ASI masks overlap: */
#define TME_SPARC_ASI_MASK_OVERLAP(asi_mask0, asi_mask1)	\
  (((asi_mask1) & _TME_SPARC_ASI_MASK_BIT(-1))			\
   ? (((asi_mask0) & (asi_mask1)) > _TME_SPARC_ASI_MASK_BIT(-1))\
   : ((asi_mask0) == (asi_mask1)))

/* this evaluates to nonzero iff this TLB entry allows access to an
   ASI, given by mask: */
#define TME_SPARC_TLB_ASI_MASK_OK(tlb, asi_mask)		\
  ((((tlb)->tme_sparc_tlb_asi_mask				\
     ^ (asi_mask))						\
    & ((asi_mask)						\
       | _TME_SPARC_ASI_MASK_BIT(-1)				\
       | TME_SPARC_ASI_MASK(0xff))) == 0)

/* these busy and unbusy a TLB entry: */
#define tme_sparc_tlb_busy(tlb)					\
  tme_bus_tlb_busy(&(tlb)->tme_sparc_tlb_bus_tlb)
#define tme_sparc_tlb_unbusy(tlb)				\
  tme_bus_tlb_unbusy(&(tlb)->tme_sparc_tlb_bus_tlb)

/* the minimum and maximum IPL levels: */
#define TME_SPARC_IPL_NONE	(0)
#define TME_SPARC_IPL_MIN	(1)
#define TME_SPARC_IPL_MAX	(15)
#define TME_SPARC_IPL_NMI	(15)

/* this indexes an sparc bus router array for an sparc with a port size
   of 8 * (2 ^ siz_lg2) bits: */
#define TME_SPARC_BUS_ROUTER_INDEX(siz_lg2, cycle_size, address)\
(((								\
   /* by the maximum cycle size: */				\
   ((cycle_size) - 1)						\
								\
   /* by the address alignment: */				\
   << siz_lg2)							\
  + ((address) & ((1 << (siz_lg2)) - 1)))			\
								\
 /* factor in the size of the generic bus router array: */	\
 * TME_BUS_ROUTER_SIZE(siz_lg2))

/* this gives the number of entries that must be in a generic bus
   router array for a device with a bus size of 8 * (2 ^ siz_lg2)
   bits: */
#define TME_SPARC_BUS_ROUTER_SIZE(siz_lg2)			\
  TME_SPARC_BUS_ROUTER_INDEX(siz_lg2, (1 << (siz_lg2)) + 1, 0)

/* structures: */

/* an sparc TLB entry: */
struct tme_sparc_tlb {

  /* the generic bus TLB associated with this TLB entry: */
  struct tme_bus_tlb tme_sparc_tlb_bus_tlb;
#define tme_sparc_tlb_addr_first tme_sparc_tlb_bus_tlb.tme_bus_tlb_addr_first
#define tme_sparc_tlb_addr_last tme_sparc_tlb_bus_tlb.tme_bus_tlb_addr_last
#define tme_sparc_tlb_bus_rwlock tme_sparc_tlb_bus_tlb.tme_bus_tlb_rwlock
#define tme_sparc_tlb_cycles_ok tme_sparc_tlb_bus_tlb.tme_bus_tlb_cycles_ok
#define tme_sparc_tlb_addr_offset tme_sparc_tlb_bus_tlb.tme_bus_tlb_addr_offset
#define tme_sparc_tlb_addr_shift tme_sparc_tlb_bus_tlb.tme_bus_tlb_addr_shift
#define tme_sparc_tlb_emulator_off_read tme_sparc_tlb_bus_tlb.tme_bus_tlb_emulator_off_read
#define tme_sparc_tlb_emulator_off_write tme_sparc_tlb_bus_tlb.tme_bus_tlb_emulator_off_write

  /* an ASI mask: */
  tme_uint32_t tme_sparc_tlb_asi_mask;
};

/* an sparc bus connection: */
struct tme_sparc_bus_connection {

  /* a generic bus connection: */
  struct tme_bus_connection tme_sparc_bus_connection;

  /* the sparc interrupt function: */
  int (*tme_sparc_bus_interrupt) _TME_P((struct tme_sparc_bus_connection *, unsigned int));

  /* the sparc TLB entry filler: */
  int (*tme_sparc_bus_tlb_fill) _TME_P((struct tme_sparc_bus_connection *, struct tme_sparc_tlb *,
					tme_uint32_t, tme_bus_addr_t, unsigned int));

  /* the sparc FPU strict-compliance enabler: */
  void (*tme_sparc_bus_fpu_strict) _TME_P((struct tme_sparc_bus_connection *, unsigned int));
};

/* globals: */
extern _tme_const tme_bus_lane_t tme_sparc_router_32[];

#endif /* !_TME_IC_SPARC_H */
