/* $Id: bus.h,v 1.12 2005/04/30 15:07:46 fredette Exp $ */

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
_TME_RCSID("$Id: bus.h,v 1.12 2005/04/30 15:07:46 fredette Exp $");

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

/* bus signal flags and form: */
#define TME_BUS_SIGNAL_LEVEL_MASK	(0x03)
#define  TME_BUS_SIGNAL_LEVEL_LOW	(0x00)
#define  TME_BUS_SIGNAL_LEVEL_HIGH	(0x01)
#define  TME_BUS_SIGNAL_LEVEL_NEGATED	(0x02)
#define  TME_BUS_SIGNAL_LEVEL_ASSERTED	(0x03)
#define TME_BUS_SIGNAL_EDGE		(0x04)
#define _TME_BUS_SIGNAL_BITS		(5)
#define TME_BUS_SIGNAL_WHICH(x)		((x) & ~((1 << _TME_BUS_SIGNAL_BITS) - 1))
#define TME_BUS_SIGNAL_INDEX(x)		((x) >> _TME_BUS_SIGNAL_BITS)
#define TME_BUS_SIGNAL_X(x)		((x) << _TME_BUS_SIGNAL_BITS)

/* all bus signal set identifiers: */
#define TME_BUS_SIGNALS_ID_GENERIC	(0)
#define TME_BUS_SIGNALS_ID_I825X6	(1)

/* the generic bus signal set: */
#define TME_BUS_SIGNAL_INT(x)		TME_BUS_SIGNAL_X(x)
#define TME_BUS_SIGNAL_IS_INT(x)	(TME_BUS_SIGNAL_INDEX(x) < 256)
#define TME_BUS_SIGNAL_INDEX_INT(x)	TME_BUS_SIGNAL_INDEX(x)
#define TME_BUS_SIGNAL_INT_UNSPEC	TME_BUS_SIGNAL_X(256)
#define TME_BUS_SIGNAL_HALT		TME_BUS_SIGNAL_X(257)
#define TME_BUS_SIGNAL_RESET		TME_BUS_SIGNAL_X(258)
#define TME_BUS_SIGNAL_IGNORE		TME_BUS_SIGNAL_X(259)
#define TME_BUS_SIGNAL_ABORT		TME_BUS_SIGNAL_X(260)
#define TME_BUS_SIGNAL_DRQ		TME_BUS_SIGNAL_X(261)
#define TME_BUS_SIGNAL_DACK		TME_BUS_SIGNAL_X(262)
#define TME_BUS_SIGNALS_GENERIC		{ TME_BUS_SIGNALS_ID_GENERIC, TME_BUS_VERSION, 384, 0 }

/* this gets the index and mask of a bus signal bit in a byte array: */
#define TME_BUS_SIGNAL_BIT_INDEX(x)	(TME_BUS_SIGNAL_INDEX(x) >> 3)
#define TME_BUS_SIGNAL_BIT_MASK(x)	TME_BIT(TME_BUS_SIGNAL_INDEX(x) & 7)

/* this gets the number of bytes in a byte array of bus signal bits: */
#define TME_BUS_SIGNAL_BIT_BYTES(count)	(((count) + 7) >> 3)

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

/* this is a special bus cycle return value.  it doesn't indicate a
   fault, but instead tells the initiator that some other event has
   happened on the bus, synchronous with the bus cycle, that the
   initiator should handle: */
#define TME_BUS_CYCLE_SYNCHRONOUS_EVENT		(EINTR)

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

  /* this TLB entry pointer is meant to make TLB filling more
     thread-safe.  since callers of TLB fill functions should release
     all locks before calling out, caller code could run in *another*
     thread during the TLB fill, and see an inconsistent TLB entry,
     or, worse, call out a TLB fill of its own on the same entry.

     in general, to avoid this kind of problem, connection interface
     functions that take pointers to data structures to read or write,
     should almost always be passed pointers to data structures local
     to the thread - usually automatic storage on the stack.

     so, TLB fill functions should expect to be filling TLB entries
     on the stack.  however, since TLB entries may also be invalidated by
     the filler at some later time, the caller must provide a pointer to
     its "global" TLB entry that backs the local TLB entry filled.
     this is that pointer.

     additionally, this field is used in "global" backing TLB entries
     as a sort of reservation field.  before a TLB fill callout is
     made, the caller stores the pointer to the local TLB entry being
     filled.  after the TLB fill returns and the caller relocks its
     data structures, it can test to see if its reservation on the
     global backing TLB entry has been broken or not.  it will only be
     broken by a concurrent TLB fill by caller code, or by a regular
     TLB invalidation (possibly an invalidation of the TLB entry just
     filled!)  only if the reservation has not been broken will the
     local TLB entry be copied back into the backing TLB entry: */
  TME_ATOMIC(struct tme_bus_tlb *, tme_bus_tlb_backing_reservation);

  /* when one or both of these pointers are not TME_EMULATOR_OFF_UNDEF, 
     this TLB entry allows fast (memory) reads of and/or writes to the
     bus region.  adding an address in the bus region to one of these
     pointers yields the desired host memory address: */
  _tme_const tme_uint8_t *tme_bus_tlb_emulator_off_read;
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

/* a bus signals set: */
struct tme_bus_signals {

  /* the bus signals set identifier: */
  tme_uint32_t tme_bus_signals_id;

  /* the version of the bus signals: */
  tme_uint32_t tme_bus_signals_version;

  /* the maximum number of bus signals in the set: */
  tme_uint32_t tme_bus_signals_count;

  /* the first signal in the bus signals set: */
  tme_uint32_t tme_bus_signals_first;
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

  /* the bus signal set adder: */
  int (*tme_bus_signals_add) _TME_P((struct tme_bus_connection *,
				     struct tme_bus_signals *));

  /* the bus signal handler: */
  int (*tme_bus_signal) _TME_P((struct tme_bus_connection *, unsigned int));

  /* the bus interrupt acknowledge handler: */
  int (*tme_bus_intack) _TME_P((struct tme_bus_connection *, unsigned int, int *));

  /* the bus TLB set allocator: */
  int (*tme_bus_tlb_set_allocate) _TME_P((struct tme_bus_connection *, unsigned int, unsigned int, TME_ATOMIC_POINTER_TYPE(struct tme_bus_tlb *)));

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

  /* the single interrupt vector used by this connection, when the
     connection doesn't know already: */
  int tme_bus_connection_int_vector_int;

  /* nonzero iff we've already logged an unconfigured interrupt
     signal: */
  int tme_bus_connection_int_logged_int;

  /* the current status of the bus signals for this connection: */
  tme_uint8_t *tme_bus_connection_int_signals;
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

  /* the bus signal sets on this bus: */
  unsigned int tme_bus_signals_count;
  struct tme_bus_signals *tme_bus_signals;

  /* the number of devices asserting the various bus signals: */
  unsigned int *tme_bus_signal_asserts;
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
				     TME_ATOMIC_POINTER_TYPE(struct tme_bus_tlb *)));
void tme_bus_tlb_map _TME_P((struct tme_bus_tlb *, tme_bus_addr_t, _tme_const struct tme_bus_tlb *, tme_bus_addr_t));
void tme_bus_tlb_reserve _TME_P((struct tme_bus_tlb *, struct tme_bus_tlb *));
void tme_bus_tlb_back _TME_P((_tme_const struct tme_bus_tlb *));
void tme_bus_tlb_invalidate _TME_P((struct tme_bus_tlb *));
void tme_bus_tlb_initialize _TME_P((struct tme_bus_tlb *));
int tme_bus_tlb_fault _TME_P((struct tme_bus_tlb *, struct tme_bus_cycle *, int));
tme_bus_addr_t tme_bus_addr_parse _TME_P((_tme_const char *, tme_bus_addr_t));
tme_bus_addr_t tme_bus_addr_parse_any _TME_P((_tme_const char *, int *));
void tme_bus_cycle_xfer _TME_P((struct tme_bus_cycle *, struct tme_bus_cycle *));
void tme_bus_cycle_xfer_memory _TME_P((struct tme_bus_cycle *, tme_uint8_t *, tme_bus_addr_t));
void tme_bus_cycle_xfer_reg _TME_P((struct tme_bus_cycle *, void *, unsigned int));

#endif /* !_TME_GENERIC_BUS_H */
