/* $Id: bus.h,v 1.5 2003/06/27 21:06:28 fredette Exp $ */

/* tme/generic/bus.h - header file for generic bus support: */

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

#ifndef _TME_GENERIC_BUS_H
#define _TME_GENERIC_BUS_H

#include <tme/common.h>
_TME_RCSID("$Id: bus.h,v 1.5 2003/06/27 21:06:28 fredette Exp $");

/* includes: */
#include <tme/element.h>
#include <tme/threads.h>
#include <tme/atomics.h>

/* macros: */

/* the log2 of various bus sizes, named by number of bits but really
   in terms of bytes: */
#define TME_BUS8_LOG2		(0)
#define TME_BUS16_LOG2		(1)
#define TME_BUS32_LOG2		(2)

/* bus signals: */
#define TME_BUS_SIGNAL_LEVEL_MASK	(0x03)
#define  TME_BUS_SIGNAL_LEVEL_LOW	(0x00)
#define  TME_BUS_SIGNAL_LEVEL_HIGH	(0x01)
#define  TME_BUS_SIGNAL_LEVEL_NEGATED	(0x02)
#define  TME_BUS_SIGNAL_LEVEL_ASSERTED	(0x03)
#define  TME_BUS_SIGNAL_LEVEL_NORMAL	(0x00)
#define  TME_BUS_SIGNAL_LEVEL_INVERTED	(0x03)
#define TME_BUS_SIGNAL_EDGE		(0x04)
#define _TME_BUS_SIGNAL_BITS		(3)
#define TME_BUS_SIGNAL_INT(x)		((x) << _TME_BUS_SIGNAL_BITS)
#define TME_BUS_SIGNAL_IS_INT(x)	((x) < (32 << _TME_BUS_SIGNAL_BITS))
#define TME_BUS_SIGNAL_WHICH_INT(x)	((x) >> _TME_BUS_SIGNAL_BITS)
#define TME_BUS_SIGNAL_INT_UNSPEC	(32 << _TME_BUS_SIGNAL_BITS)
#define TME_BUS_SIGNAL_HALT		(33 << _TME_BUS_SIGNAL_BITS)
#define TME_BUS_SIGNAL_RESET		(34 << _TME_BUS_SIGNAL_BITS)
#define TME_BUS_SIGNAL_IGNORE		(35 << _TME_BUS_SIGNAL_BITS)
#define TME_BUS_SIGNAL_ABORT		(36 << _TME_BUS_SIGNAL_BITS)
#define _TME_BUS_SIGNAL_COUNT		(37)
#define TME_BUS_SIGNAL_WHICH(x)		((x) >> _TME_BUS_SIGNAL_BITS)
#define TME_BUS_SIGNAL_X(x)		(((x) + _TME_BUS_SIGNAL_COUNT) << _TME_BUS_SIGNAL_BITS)
#define TME_BUS_SIGNAL_IS_X(x)		((x) >= (_TME_BUS_SIGNAL_COUNT << _TME_BUS_SIGNAL_BITS))

/* this gets the index and mask of a bus signal bit in a byte array: */
#define TME_BUS_SIGNAL_BIT_INDEX(x)	(((x) >> _TME_BUS_SIGNAL_BITS) >> 3)
#define TME_BUS_SIGNAL_BIT_MASK(x)	TME_BIT(((x) >> _TME_BUS_SIGNAL_BITS) & 7)

/* the undefined interrupt vector: */
#define TME_BUS_INTERRUPT_VECTOR_UNDEF	(-1)

/* bus cycles: */
#define TME_BUS_CYCLE_UNDEF	(0)
#define TME_BUS_CYCLE_READ	TME_BIT(0)
#define TME_BUS_CYCLE_WRITE	TME_BIT(1)

/* the maximum number of fault handlers on a TLB entry: */
#define TME_BUS_TLB_FAULT_HANDLERS	(4)

/* given a TLB entry and an address range, this evaluates to nonzero
   iff the TLB entry is valid and covers the address range: */
#define _TME_BUS_TLB_OK(tlb, address_first, address_last)				\
  ((address_first) >= TME_ATOMIC_READ(tme_bus_addr_t, (tlb)->tme_bus_tlb_addr_first)	\
   && (address_last) <= TME_ATOMIC_READ(tme_bus_addr_t, (tlb)->tme_bus_tlb_addr_last))

/* given a TLB entry and an address range, this evaulates to nonzero
   iff the TLB entry is valid, covers the address range, and allows
   fast (memory) read access: */
#define TME_BUS_TLB_OK_FAST_READ(tlb, address_first, address_last)	\
  (_TME_BUS_TLB_OK(tlb, address_first, address_last)			\
   && (tlb)->tme_bus_tlb_emulator_off_read != TME_EMULATOR_OFF_UNDEF)

/* given a TLB entry and an address range, this evaulates to nonzero
   iff the TLB entry is valid, covers the address range, and allows
   fast (memory) write access: */
#define TME_BUS_TLB_OK_FAST_WRITE(tlb, address_first, address_last)	\
  (_TME_BUS_TLB_OK(tlb, address_first, address_last)			\
   && (tlb)->tme_bus_tlb_emulator_off_write != TME_EMULATOR_OFF_UNDEF)

/* given a TLB entry and an address range, this evaulates to nonzero
   iff the TLB entry is valid, covers the address range, and allows
   slow (function call) read access: */
#define TME_BUS_TLB_OK_SLOW_READ(tlb, address_first)		\
  (_TME_BUS_TLB_OK(tlb, address_first, address_first)		\
   && ((tlb)->tme_bus_tlb_cycles_ok & TME_BUS_CYCLE_READ))

/* given a TLB entry and an address range, this evaulates to nonzero
   iff the TLB entry is valid, covers the address range, and allows
   slow (function call) write access: */
#define TME_BUS_TLB_OK_SLOW_WRITE(tlb, address_first)		\
  (_TME_BUS_TLB_OK(tlb, address_first, address_first)		\
   && ((tlb)->tme_bus_tlb_cycles_ok & TME_BUS_CYCLE_WRITE))

/* this adds a fault handler to a TLB entry: */
#define TME_BUS_TLB_FAULT_HANDLER(tlb, func, private)	\
do {							\
  assert(tlb->tme_bus_tlb_fault_handler_count		\
	 < TME_BUS_TLB_FAULT_HANDLERS);			\
  tlb->tme_bus_tlb_fault_handlers			\
    [tlb->tme_bus_tlb_fault_handler_count]		\
    .tme_bus_tlb_fault_handler_private = private;	\
  tlb->tme_bus_tlb_fault_handlers			\
    [tlb->tme_bus_tlb_fault_handler_count]		\
    .tme_bus_tlb_fault_handler = func;			\
  tlb->tme_bus_tlb_fault_handler_count++;		\
} while (/* CONSTCOND */ 0)

/* this indexes a generic bus router array for a device with a port
   size of 8 * (2 ^ siz_lg2) bits: */
#define TME_BUS_ROUTER_INDEX(siz_lg2, other_port_siz_lg2, other_port_lane_least) \
(((							\
   /* by the (overlapping) other port size: */		\
   (other_port_siz_lg2)					\
							\
   /* by the (overlapping) other port least lane: */	\
    << (siz_lg2))					\
   + other_port_lane_least)				\
							\
  /* by lane number, which we add later: */		\
  << (siz_lg2))

/* this gives the number of entries that must be in a generic bus
   router array for a device with a bus size of 8 * (2 ^ siz_lg2)
   bits: */
#define TME_BUS_ROUTER_SIZE(siz_lg2)				\
  TME_BUS_ROUTER_INDEX(siz_lg2, (siz_lg2) + 1, 0)

/* bus lane routing entries: */
#define TME_BUS_LANE_WARN			TME_BIT(7)
#define TME_BUS_LANE_ABORT			(0x7f)
#define TME_BUS_LANE_UNDEF			(0x7e)
#define TME_BUS_LANE_ROUTE_WRITE_IGNORE		TME_BIT(6)
#define TME_BUS_LANE_ROUTE(x)			(x)

/* types: */
struct tme_bus_tlb;

/* XXX we need a configure switch that can force this to
   tme_uint64_t when an emulated architecture has a 64-bit bus: */
typedef tme_uint32_t		tme_bus_addr_t;

/* a bus byte lane routing entry: */
typedef tme_uint8_t		tme_bus_lane_t;

/* a bus cycle: */
struct tme_bus_cycle {

  /* the bus cycle data buffer pointer.  this points to the byte
     associated with the bus address given below: */
  tme_uint8_t *tme_bus_cycle_buffer;

  /* how bus byte lanes are connected to the bus cycle data buffer: */
  _tme_const tme_bus_lane_t *tme_bus_cycle_lane_routing;

  /* the bus address: */
  tme_bus_addr_t tme_bus_cycle_address;

  /* when adding one to the bus address, add this to the bus cycle
     data buffer pointer to get a pointer to the byte associated with
     the new bus address: */
  tme_int8_t tme_bus_cycle_buffer_increment;

  /* the type of bus cycle: */
  tme_uint8_t tme_bus_cycle_type;

  /* the maximum number of addresses that could be covered by this
     cycle.  depending on where and how wide the initiator and
     responder ports overlap, the number of addresses actually covered
     may be less: */
  tme_uint8_t tme_bus_cycle_size;

  /* the starting lane and size of this device's port.  bits 0-2 are
     the log2 of the lane size of the port.  zero corresponds to a
     one-lane (8-bit) port, one to a two-lane (16-bit) port, etc.
     bits 3-7 are the least byte lane in this device's port.  zero
     corresponds to D7-D0, one to D15-D8, etc.: */
  tme_uint8_t tme_bus_cycle_port;
#define TME_BUS_CYCLE_PORT(lane_least, lane_size_lg2) \
  (((lane_least) << 3) | (lane_size_lg2))
#define TME_BUS_CYCLE_PORT_SIZE_LG2(port) \
  TME_FIELD_EXTRACTU(port, 0, 3)
#define TME_BUS_CYCLE_PORT_LANE_LEAST(port) \
  TME_FIELD_EXTRACTU(port, 3, 5)
};

/* a bus cycle handler: */
typedef int (*tme_bus_cycle_handler) _TME_P((void *, struct tme_bus_cycle *));

/* a bus fault handler: */
typedef int (*tme_bus_fault_handler) _TME_P((void *, struct tme_bus_tlb *, struct tme_bus_cycle *, int));

/* a bus TLB entry: */
struct tme_bus_tlb {

  /* the bus address region covered by this TLB entry: */
  TME_ATOMIC(tme_bus_addr_t, tme_bus_tlb_addr_first);
  TME_ATOMIC(tme_bus_addr_t, tme_bus_tlb_addr_last);

  /* when one or both of these pointers are not TME_EMULATOR_OFF_UNDEF, 
     this TLB entry allows fast (memory) reads of and/or writes to the
     bus region.  adding an address in the bus region to one of these
     pointers yields the desired host memory address: */
  tme_uint8_t *tme_bus_tlb_emulator_off_read;
  tme_uint8_t *tme_bus_tlb_emulator_off_write;

  /* fast (memory) reads and writes are protected by this rwlock: */
  tme_rwlock_t *tme_bus_tlb_rwlock;

  /* when one or both of TLB_BUS_CYCLE_READ and TLB_BUS_CYCLE_WRITE
     are set in this value, this TLB entry allows slow (function call)
     reads of and/or writes to the bus region: */
  unsigned int tme_bus_tlb_cycles_ok;

  /* adding an address in the bus region to this offset, and then
     shifting that result to the right (shift > 0) or to the left
     (shift < 0) yields an address for the bus cycle handler: */
  tme_bus_addr_t tme_bus_tlb_addr_offset;
  int tme_bus_tlb_addr_shift;

  /* the bus cycle handler: */
  void *tme_bus_tlb_cycle_private;
  tme_bus_cycle_handler tme_bus_tlb_cycle;

  /* the bus fault handlers: */
  unsigned int tme_bus_tlb_fault_handler_count;
  struct {
    void *tme_bus_tlb_fault_handler_private;
    tme_bus_fault_handler tme_bus_tlb_fault_handler;
  } tme_bus_tlb_fault_handlers[TME_BUS_TLB_FAULT_HANDLERS];
};

/* a bus connection: */
struct tme_bus_connection {

  /* the generic connection side: */
  struct tme_connection tme_bus_connection;

  /* the subregions on the bus for this connection.  most connections
     will only have one subregion, with a first address of zero and a
     last address of their size on the bus: */
  struct tme_bus_subregion {

    /* the first and last addresses, starting from zero, of this
       subregion: */
    tme_bus_addr_t tme_bus_subregion_address_first;
    tme_bus_addr_t tme_bus_subregion_address_last;

    /* any other subregions for this bus connection: */
    _tme_const struct tme_bus_subregion *tme_bus_subregion_next;
  } tme_bus_subregions;

  /* the bus signal handler: */
  int (*tme_bus_signal) _TME_P((struct tme_bus_connection *, unsigned int));

  /* the bus interrupt acknowledge handler: */
  int (*tme_bus_intack) _TME_P((struct tme_bus_connection *, unsigned int, int *));

  /* the bus TLB set allocator: */
  int (*tme_bus_tlb_set_allocate) _TME_P((struct tme_bus_connection *, unsigned int, unsigned int, TME_ATOMIC_POINTER_TYPE(struct tme_bus_tlb **)));

  /* the bus TLB entry filler: */
  int (*tme_bus_tlb_fill) _TME_P((struct tme_bus_connection *, struct tme_bus_tlb *,
				  tme_bus_addr_t, unsigned int));
};

/* internal information about a bus connection: */
struct tme_bus_connection_int {

  /* the external bus connection: */
  struct tme_bus_connection tme_bus_connection_int;

  /* this is nonzero iff the bus connection is addressable: */
  int tme_bus_connection_int_addressable;

  /* the first and last addresses of this connection.  most code
     should never use the last-address value, and instead should honor
     the connection's subregions: */
  tme_bus_addr_t tme_bus_connection_int_address;
  tme_bus_addr_t tme_bus_connection_int_address_last;

  /* the mask added to addresses sourced by this connection: */
  tme_bus_addr_t tme_bus_connection_int_sourced;

  /* the single interrupt signal used by this connection, when
     the connection doesn't know already: */
  int tme_bus_connection_int_signal_int;

  /* nonzero iff we've already logged an unconfigured interrupt
     signal: */
  int tme_bus_connection_int_logged_int;

  /* the current status of the bus signals for this connection: */
  tme_uint8_t tme_bus_connection_int_signals[(_TME_BUS_SIGNAL_COUNT + 7) >> 3];
};

/* a generic bus: */
struct tme_bus {

  /* the optional rwlock protecting this bus: */
  tme_rwlock_t tme_bus_rwlock;

  /* the address mask used on this bus: */
  tme_bus_addr_t tme_bus_address_mask;

  /* all connections to this bus: */
  struct tme_bus_connection_int *tme_bus_connections;

  /* the number of addressable connections to this bus: */
  int tme_bus_addressables_count;

  /* the size of the addressable connections array: */
  int tme_bus_addressables_size;

  /* the addressable connections array: */
  struct tme_bus_addressable {
    struct tme_bus_connection_int *tme_bus_addressable_connection;
    _tme_const struct tme_bus_subregion *tme_bus_addressable_subregion;
  } *tme_bus_addressables;

  /* the number of devices asserting the various bus signals: */
  unsigned int tme_bus_signal_asserts[_TME_BUS_SIGNAL_COUNT];
};

/* prototypes: */
int tme_bus_address_search _TME_P((struct tme_bus *, tme_bus_addr_t));
int tme_bus_connection_ok _TME_P((struct tme_bus *,
				  struct tme_bus_connection_int *));
int tme_bus_connection_make _TME_P((struct tme_bus *,
				    struct tme_bus_connection_int *,
				    unsigned int));
int tme_bus_connection_break _TME_P((struct tme_bus *,
				     struct tme_bus_connection_int *,
				     unsigned int));
int tme_bus_tlb_fill _TME_P((struct tme_bus *, 
			     struct tme_bus_connection_int *, 
			     struct tme_bus_tlb *, tme_bus_addr_t, unsigned int));
int tme_bus_tlb_set_allocate _TME_P((struct tme_bus *,
				     struct tme_bus_connection_int *,
				     unsigned int,
				     unsigned int, 
				     TME_ATOMIC_POINTER_TYPE(struct tme_bus_tlb **)));
void tme_bus_tlb_map _TME_P((struct tme_bus_tlb *, tme_bus_addr_t, _tme_const struct tme_bus_tlb *, tme_bus_addr_t));
void tme_bus_tlb_invalidate _TME_P((struct tme_bus_tlb *));
void tme_bus_tlb_initialize _TME_P((struct tme_bus_tlb *));
int tme_bus_tlb_fault _TME_P((struct tme_bus_tlb *, struct tme_bus_cycle *, int));
tme_bus_addr_t tme_bus_addr_parse _TME_P((_tme_const char *, tme_bus_addr_t));
tme_bus_addr_t tme_bus_addr_parse_any _TME_P((_tme_const char *, int *));
void tme_bus_cycle_xfer _TME_P((struct tme_bus_cycle *, struct tme_bus_cycle *));
void tme_bus_cycle_xfer_memory _TME_P((struct tme_bus_cycle *, tme_uint8_t *, tme_bus_addr_t));

#endif /* !_TME_GENERIC_BUS_H */
