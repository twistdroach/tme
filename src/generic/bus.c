/* $Id: bus.c,v 1.5 2003/10/16 02:48:18 fredette Exp $ */

/* generic/gen-bus.c - generic bus support: */

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
_TME_RCSID("$Id: bus.c,v 1.5 2003/10/16 02:48:18 fredette Exp $");

/* includes: */
#include <tme/generic/bus.h>
#include <stdlib.h>
#include <string.h>

/* this does a binary search of the addressable connections: */
int
tme_bus_address_search(struct tme_bus *bus, tme_bus_addr_t address)
{
  int left, right, pivot;
  struct tme_bus_connection_int *conn_int;
  const struct tme_bus_subregion *subregion;

  /* initialize for the search: */
  left = 0;
  right = bus->tme_bus_addressables_count - 1;
  
  /* do the search: */
  pivot = 0;
  for (; left <= right; ) {

    /* get the pivot: */
    pivot = (left + right) / 2;
    conn_int = bus->tme_bus_addressables[pivot].tme_bus_addressable_connection;
    subregion = bus->tme_bus_addressables[pivot].tme_bus_addressable_subregion;

    /* if we have to move left: */
    if (address
	< (conn_int->tme_bus_connection_int_address
	   + subregion->tme_bus_subregion_address_first)) {
      /* if we're done searching, pivot is already the index of the
	 first element we need to shift to the right in order to
	 insert a new element: */
      right = pivot - 1;
    }

    /* if we have to move right: */
    else if (address
	     > (conn_int->tme_bus_connection_int_address
		+ subregion->tme_bus_subregion_address_last)) {
      /* if we're done searching, pivot + 1 is the index of the
	 first element we need to shift to the right in order to
	 insert a new element: */
      left = ++pivot;
    }

    /* we found the addressable: */
    else {
      return (pivot);
    }
  }

  /* we failed to find an addressable that covers the address: */
  return (-1 - pivot);
}

/* this fills a TLB entry: */
int
tme_bus_tlb_fill(struct tme_bus *bus,
		 struct tme_bus_connection_int *conn_int_asker,
		 struct tme_bus_tlb *tlb,
		 tme_bus_addr_t address, 
		 unsigned int cycles)
{
  int pivot;
  struct tme_bus_connection_int *conn_int;
  const struct tme_bus_subregion *subregion;
  struct tme_bus_connection *conn_bus_other;
  tme_bus_addr_t sourced_address_mask, conn_address;
  tme_bus_addr_t hole_first, hole_last;
  struct tme_bus_tlb tlb_bus;
  void *cycle_fault_private;
  tme_bus_cycle_handler cycle_fault;
  int rc;

  /* get the sourced address mask: */
  sourced_address_mask = conn_int_asker->tme_bus_connection_int_sourced;

  /* search for this address on the bus: */
  pivot = tme_bus_address_search(bus, sourced_address_mask | address);

  /* if this address doesn't exist: */
  if (pivot < 0) {

    /* save the bus' fault cycle handler: */
    cycle_fault_private = tlb->tme_bus_tlb_cycle_private;
    cycle_fault = tlb->tme_bus_tlb_cycle;

    /* initialize the TLB entry: */
    tme_bus_tlb_initialize(tlb);

    /* this TLB entry can cover the entire hole in the address space,
       limited by the sourced address mask of this device: */
    pivot = -1 - pivot;
    hole_first = (pivot == 0
		  ? 0
		  : ((bus->tme_bus_addressables[pivot - 1]
		      .tme_bus_addressable_connection->tme_bus_connection_int_address)
		     + (bus->tme_bus_addressables[pivot - 1]
			.tme_bus_addressable_subregion->tme_bus_subregion_address_last)
		     + 1));
    hole_first = TME_MAX(hole_first, sourced_address_mask);
    hole_last = (pivot == bus->tme_bus_addressables_count
		 ? bus->tme_bus_address_mask
		 : ((bus->tme_bus_addressables[pivot]
		     .tme_bus_addressable_connection->tme_bus_connection_int_address)
		    - 1));
    hole_last = TME_MIN(hole_last,
			sourced_address_mask
			+ conn_int_asker->tme_bus_connection_int_address_last);
    TME_ATOMIC_WRITE(tme_bus_addr_t, tlb->tme_bus_tlb_addr_first, hole_first);
    TME_ATOMIC_WRITE(tme_bus_addr_t, tlb->tme_bus_tlb_addr_last, hole_last);

    /* reads and writes are allowed: */
    tlb->tme_bus_tlb_cycles_ok = TME_BUS_CYCLE_READ | TME_BUS_CYCLE_WRITE;

    /* reads and writes in this region always fault: */
    tlb->tme_bus_tlb_cycle_private = cycle_fault_private;
    tlb->tme_bus_tlb_cycle = cycle_fault;
    rc = TME_OK;
  }

  /* otherwise, this address does exist: */
  else {
    conn_int = bus->tme_bus_addressables[pivot].tme_bus_addressable_connection;
    subregion = bus->tme_bus_addressables[pivot].tme_bus_addressable_subregion;
    conn_bus_other = 
      (struct tme_bus_connection *) conn_int->tme_bus_connection_int.tme_bus_connection.tme_connection_other;

    /* call the TLB fill function for the connection: */
    conn_address = (sourced_address_mask | address) - conn_int->tme_bus_connection_int_address;
    rc = (*conn_bus_other->tme_bus_tlb_fill)(conn_bus_other, tlb,
					     conn_address, cycles);

    /* if that succeeded: */
    if (rc == TME_OK) {
      
      /* create the mapping TLB entry: */
      TME_ATOMIC_WRITE(tme_bus_addr_t, tlb_bus.tme_bus_tlb_addr_first, 
		       (conn_int->tme_bus_connection_int_address
			+ subregion->tme_bus_subregion_address_first
			- sourced_address_mask));
      TME_ATOMIC_WRITE(tme_bus_addr_t, tlb_bus.tme_bus_tlb_addr_last, 
		       (conn_int->tme_bus_connection_int_address
			+ subregion->tme_bus_subregion_address_last
			- sourced_address_mask));
      tlb_bus.tme_bus_tlb_cycles_ok = TME_BUS_CYCLE_READ | TME_BUS_CYCLE_WRITE;
  
      /* map the filled TLB entry: */
      tme_bus_tlb_map(tlb, conn_address, &tlb_bus, address);
    }
  }

  /* done: */
  return (rc);
}

/* this allocates a new TLB set: */
int
tme_bus_tlb_set_allocate(struct tme_bus *bus,
			 struct tme_bus_connection_int *conn_int_asker,
			 unsigned int count, unsigned int sizeof_one, 
			 TME_ATOMIC_POINTER_TYPE(struct tme_bus_tlb **) _tlbs)
{
  struct tme_bus_connection *conn_bus_other, *conn_bus_dma;
  int conn_int_i;
  int rc;
  struct tme_bus_tlb *tlbs, *tlb;
  unsigned int tlb_i;

  /* at most one of our addressable connections may provide a TLB set
     allocator.  generally, this means that connection is
     DMA-controller-like connection to the bus, where it may need to
     invalidate at any later time the TLBs it fills out, due to sudden
     changes in how the DMA region on the bus is mapped: */
  conn_bus_dma = NULL;
  for (conn_int_i = 0;
       conn_int_i < bus->tme_bus_addressables_count;
       conn_int_i++) {
    conn_bus_other = 
      ((struct tme_bus_connection *)
       bus->tme_bus_addressables[conn_int_i].tme_bus_addressable_connection
       ->tme_bus_connection_int.tme_bus_connection.tme_connection_other);

    /* if this bus connection offers a TLB set allocator, it is
       a DMA-controller-like connection to the bus: */
    if (conn_bus_other->tme_bus_tlb_set_allocate != NULL) {

      /* if there is more than one of these, it is likely a
	 configuration error.  if we had some way of specifying which
	 of several DMA regions a given connection will always use, we
	 could avoid this: */
      if (conn_bus_dma != NULL) {
	abort();
      }

      conn_bus_dma = conn_bus_other;
    }
  }

  /* if there is a DMA-controller-like connection to the bus, 
     let it allocate the TLB set: */
  if (conn_bus_dma != NULL) {
    rc = (*conn_bus_dma->tme_bus_tlb_set_allocate)
      (conn_bus_dma, count, sizeof_one, _tlbs);
  }

  /* otherwise, allocate and initialize a singleton set ourselves: */
  else {
    tlbs = (struct tme_bus_tlb *) tme_malloc(count * sizeof_one);
    tlb = tlbs;
    for (tlb_i = 0; tlb_i < count; tlb_i++) {
      tme_bus_tlb_invalidate(tlb);
      tlb = (struct tme_bus_tlb *) (((tme_uint8_t *) tlb) + sizeof_one);
    }
    TME_ATOMIC_WRITE(struct tme_bus_tlb *, *_tlbs, tlbs);
    rc = TME_OK;
  }
      
  /* done: */
  return (rc);
}

/* this returns nonzero if the connection's address space is available: */
int
tme_bus_connection_ok(struct tme_bus *bus,
		      struct tme_bus_connection_int *conn_int)
{
  const struct tme_bus_subregion *subregion;
  const struct tme_bus_connection *conn_bus_other;
  int pivot_start, pivot_end;

  /* if this connection isn't addressable, it's always OK: */
  if (!conn_int->tme_bus_connection_int_addressable) {
    return (TRUE);
  }

  /* all subregions of this connection must fit on the bus,
     and they must not overlap with any other subregion on 
     any other existing connection: */
  /* XXX we should also check that the connection's subregions don't
     overlap with each other: */
  conn_bus_other
    = ((struct tme_bus_connection *)
       conn_int->tme_bus_connection_int.tme_bus_connection.tme_connection_other);
  for (subregion = &conn_bus_other->tme_bus_subregions;
       subregion != NULL;
       subregion = subregion->tme_bus_subregion_next) {

    /* the subregion's last address cannot be less than
       the first address: */
    if (subregion->tme_bus_subregion_address_last
	< subregion->tme_bus_subregion_address_first) {
      return (FALSE);
    }
    
    /* this subregion must fit on the bus: */
    if (subregion->tme_bus_subregion_address_last >
	(bus->tme_bus_address_mask
	 - conn_int->tme_bus_connection_int_address)) {
      return (FALSE);
    }

    /* search for anything covering the start or end of the new
       addressable subregion: */
    pivot_start = 
      tme_bus_address_search(bus, 
			     (conn_int->tme_bus_connection_int_address
			      + subregion->tme_bus_subregion_address_first));
    pivot_end =
      tme_bus_address_search(bus,
			     (conn_int->tme_bus_connection_int_address
			      + subregion->tme_bus_subregion_address_last));

    /* both searches must have failed, and they must have stopped at the
       same point in the sorted addressables, further indicating that no
       addressable exists anywhere *between* the start and end of the
       new addressable, either.  otherwise, this connection fails: */
    if (pivot_start >= 0
	|| pivot_end >= 0
	|| pivot_start != pivot_end) {
      return (FALSE);
    }
  }

  /* this connection's address space is available: */
  return (TRUE);
}

/* this makes a new connection: */
int
tme_bus_connection_make(struct tme_bus *bus,
			struct tme_bus_connection_int *conn_int,
			unsigned int state)
{
  const struct tme_bus_connection *conn_bus_other;
  const struct tme_bus_subregion *subregion;
  int pivot;

  /* if this connection is not full, return now: */
  if (state == TME_CONNECTION_HALF) {
    return (TME_OK);
  }

  /* add this connection to our list: */
  conn_int->tme_bus_connection_int.tme_bus_connection.tme_connection_next
    = (struct tme_connection *) bus->tme_bus_connections;
  bus->tme_bus_connections = conn_int;

  /* if this connection is addressable, and this is connection is now
     fully made, add it to our list of addressables: */
  if (conn_int->tme_bus_connection_int_addressable
      && state == TME_CONNECTION_FULL) {
    
    /* add all subregions of this connection as addressables: */
    conn_int->tme_bus_connection_int_address_last = 0;
    conn_bus_other
      = ((struct tme_bus_connection *)
	 conn_int->tme_bus_connection_int.tme_bus_connection.tme_connection_other);
    for (subregion = &conn_bus_other->tme_bus_subregions;
	 subregion != NULL;
	 subregion = subregion->tme_bus_subregion_next) {

      /* search for the place to insert this new addressable: */
      pivot = tme_bus_address_search(bus, 
				     (conn_int->tme_bus_connection_int_address
				      + subregion->tme_bus_subregion_address_first));
      assert(pivot < 0);
      pivot = -1 - pivot;
    
      /* if we have to, grow the addressable array: */
      if (bus->tme_bus_addressables_count
	  == bus->tme_bus_addressables_size) {
	bus->tme_bus_addressables_size += (bus->tme_bus_addressables_size >> 1) + 1;
	bus->tme_bus_addressables = tme_renew(struct tme_bus_addressable,
					      bus->tme_bus_addressables,
					      bus->tme_bus_addressables_size);
      }

      /* move all of the later addressables down: */
      memmove(&bus->tme_bus_addressables[pivot + 1],
	      &bus->tme_bus_addressables[pivot],
	      sizeof(bus->tme_bus_addressables[pivot])
	      * (bus->tme_bus_addressables_count
		 - pivot));

      /* insert this new addressable: */
      bus->tme_bus_addressables[pivot].tme_bus_addressable_connection = conn_int;
      bus->tme_bus_addressables[pivot].tme_bus_addressable_subregion = subregion;
      bus->tme_bus_addressables_count++;

      /* update the last address on this connection.  NB that the
	 subregion information should be used almost always.
	 currently this value is only used as the width of the
	 connection for the purposes of determining TLB entry limits
	 when the connection itself asks to fill a TLB entry: */
      conn_int->tme_bus_connection_int_address_last
	= TME_MAX(conn_int->tme_bus_connection_int_address_last,
		  subregion->tme_bus_subregion_address_last);
    }
  }

  return (TME_OK);
}

/* this breaks a connection: */
int
tme_bus_connection_break(struct tme_bus *bus,
			 struct tme_bus_connection_int *conn_int,
			 unsigned int state)
{
  abort();
}

/* this map the first bus TLB entry to be valid on another bus, according to
   the information in the second bus TLB entry: */
void
tme_bus_tlb_map(struct tme_bus_tlb *tlb0, tme_bus_addr_t addr0, 
		const struct tme_bus_tlb *tlb1, tme_bus_addr_t addr1)
{
  tme_bus_addr_t extra_before0, extra_after0;
  tme_bus_addr_t extra_before1, extra_after1;
  tme_bus_addr_t addr_offset;
  unsigned int cycles_ok;

  /* get the address offset: */
  addr_offset = addr1 - addr0;

  /* intersect the amount of bus address space covered: */
  extra_before0 = addr0 - TME_ATOMIC_READ(tme_bus_addr_t, tlb0->tme_bus_tlb_addr_first);
  extra_after0 = TME_ATOMIC_READ(tme_bus_addr_t, tlb0->tme_bus_tlb_addr_last) - addr0;
  extra_before1 = addr1 - TME_ATOMIC_READ(tme_bus_addr_t, tlb1->tme_bus_tlb_addr_first);
  extra_after1 = TME_ATOMIC_READ(tme_bus_addr_t, tlb1->tme_bus_tlb_addr_last) - addr1;
  TME_ATOMIC_WRITE(tme_bus_addr_t, tlb0->tme_bus_tlb_addr_first, 
		   addr1 - TME_MIN(extra_before0, extra_before1));
  TME_ATOMIC_WRITE(tme_bus_addr_t, tlb0->tme_bus_tlb_addr_last, 
		   addr1 + TME_MIN(extra_after0, extra_after1));

  /* intersect the kinds of bus cycles allowed: */
  cycles_ok = (tlb0->tme_bus_tlb_cycles_ok &= tlb1->tme_bus_tlb_cycles_ok);
  if (!(cycles_ok & TME_BUS_CYCLE_READ)) {
    tlb0->tme_bus_tlb_emulator_off_read = TME_EMULATOR_OFF_UNDEF;
  }
  else if (tlb0->tme_bus_tlb_emulator_off_read != TME_EMULATOR_OFF_UNDEF) {
    tlb0->tme_bus_tlb_emulator_off_read -= addr_offset;
  }
  if (!(cycles_ok & TME_BUS_CYCLE_WRITE)) {
    tlb0->tme_bus_tlb_emulator_off_write = TME_EMULATOR_OFF_UNDEF;
  }
  else if (tlb0->tme_bus_tlb_emulator_off_write != TME_EMULATOR_OFF_UNDEF) {
    tlb0->tme_bus_tlb_emulator_off_write -= addr_offset;
  }

  /* update the address shift for the cycle handler: */
  tlb0->tme_bus_tlb_addr_offset -= addr_offset;
}

/* this invalidates a bus TLB entry: */
void
tme_bus_tlb_invalidate(struct tme_bus_tlb *tlb)
{
  
  /* make the first address covered all-bits-one.  the only bus TLB
     entries this will not invalidate are those that have a last
     address covered of all-bits one: */
  TME_ATOMIC_WRITE(tme_bus_addr_t, tlb->tme_bus_tlb_addr_first, -1);
  
  /* make the last address covered all-bits-zero.  this will
     invalidate the TLB entries we didn't catch above: */
  TME_ATOMIC_WRITE(tme_bus_addr_t, tlb->tme_bus_tlb_addr_last, 0);
}

/* this initializes a bus TLB entry: */
void
tme_bus_tlb_initialize(struct tme_bus_tlb *tlb)
{
  
  /* make the first address covered all-bits-one: */
  TME_ATOMIC_WRITE(tme_bus_addr_t, tlb->tme_bus_tlb_addr_first, -1);
  
  /* make the last address covered all-bits-zero: */
  TME_ATOMIC_WRITE(tme_bus_addr_t, tlb->tme_bus_tlb_addr_last, 0);

  /* no fast (memory) transfers allowed: */
  tlb->tme_bus_tlb_emulator_off_read = TME_EMULATOR_OFF_UNDEF;
  tlb->tme_bus_tlb_emulator_off_write = TME_EMULATOR_OFF_UNDEF;
  tlb->tme_bus_tlb_rwlock = NULL;

  /* no bus cycles allowed: */
  tlb->tme_bus_tlb_cycles_ok = TME_BUS_CYCLE_UNDEF;

  /* no address offset or shift: */
  tlb->tme_bus_tlb_addr_offset = 0;
  tlb->tme_bus_tlb_addr_shift = 0;

  /* no bus cycle handler: */
  tlb->tme_bus_tlb_cycle_private = NULL;
  tlb->tme_bus_tlb_cycle = NULL;

  /* no bus fault handlers: */
  tlb->tme_bus_tlb_fault_handler_count = 0;
}

/* this calls a TLB entry's fault handlers: */
int
tme_bus_tlb_fault(struct tme_bus_tlb *tlb, struct tme_bus_cycle *cycle, int rc)
{
  unsigned int i;

  /* call all of the fault handlers: */
  for (i = 0; i < tlb->tme_bus_tlb_fault_handler_count; i++) {
    rc = ((*tlb->tme_bus_tlb_fault_handlers[i].tme_bus_tlb_fault_handler)
	  (tlb->tme_bus_tlb_fault_handlers[i].tme_bus_tlb_fault_handler_private,
	   tlb, cycle, rc));
  }

  return (rc);
}

/* this parses any bus address: */
tme_bus_addr_t
tme_bus_addr_parse_any(const char *address_string, int *_failed)
{
  unsigned long address;
  char *units;

  /* catch a NULL string: */
  if (address_string == NULL) {
    *_failed = TRUE;
    return (0);
  }

  /* assume we will succeed: */
  *_failed = FALSE;

  /* convert the string: */
  address = strtoul(address_string, &units, 0);
  if (units == address_string) {
    *_failed = TRUE;
    return (0);
  }

  /* handle any units: */
  if (!strcmp(units, "GB")
      || !strcasecmp(units, "G")) {
    return (((tme_bus_addr_t) address) * 1024 * 1024 * 1024);
  }
  else if (!strcmp(units, "MB")
      || !strcasecmp(units, "M")) {
    return (((tme_bus_addr_t) address) * 1024 * 1024);
  }
  else if (!strcmp(units, "KB")
	   || !strcasecmp(units, "k")) {
    return (((tme_bus_addr_t) address) * 1024);
  }
  else if (*units == '\0') {
    return ((tme_bus_addr_t) address);
  }
  *_failed = TRUE;
  return (0);
}

/* this parses a bus address that has a restricted range: */
tme_bus_addr_t
tme_bus_addr_parse(const char *address_string, tme_bus_addr_t failure_value)
{
  int failed;
  tme_bus_addr_t address;
  address = tme_bus_addr_parse_any(address_string, &failed);
  return (failed ? failure_value : address);
}

/* this transfers bytes between the two participants in a bus cycle: */
void
tme_bus_cycle_xfer(struct tme_bus_cycle *cycle_init, struct tme_bus_cycle *cycle_resp)
{
  struct tme_bus_cycle *cycle_reader, *cycle_writer;
  int buffer_increment_mask_reader, buffer_increment_mask_writer;
  int port_size_reader, port_size_writer;
  int port_overlap_lane_least, port_overlap_size, port_overlap_size_lg2;
  int lane, lane_end;
  int lane_reader, lane_writer;
  int lane_in_reader, lane_in_writer;
  int lane_routing_offset_reader, lane_routing_offset_writer;
  tme_bus_lane_t lane_routing_reader, lane_routing_writer;
  tme_uint8_t lane_value;
  int warn_on_lane;
  unsigned int cycle_size_reader, cycle_size_writer;

  /* sort the initiator and responder into bus reader and bus writer: */
  if (cycle_init->tme_bus_cycle_type == TME_BUS_CYCLE_READ) {
    assert(cycle_resp->tme_bus_cycle_type == TME_BUS_CYCLE_WRITE);
    cycle_reader = cycle_init;
    cycle_writer = cycle_resp;
  }
  else {
    assert(cycle_init->tme_bus_cycle_type == TME_BUS_CYCLE_WRITE);
    assert(cycle_resp->tme_bus_cycle_type == TME_BUS_CYCLE_READ);
    cycle_reader = cycle_resp;
    cycle_writer = cycle_init;
  }

  /* get the increment masks for the reader and writer.  since
     tme_bus_cycle_buffer_increment is always 1 or -1, this mask is
     used to negate values without multiplication: */
  if (cycle_reader->tme_bus_cycle_buffer_increment == -1) {
    buffer_increment_mask_reader = -1;
  }
  else {
    assert(cycle_reader->tme_bus_cycle_buffer_increment == 1);
    buffer_increment_mask_reader = 0;
  }
  if (cycle_writer->tme_bus_cycle_buffer_increment == -1) {
    buffer_increment_mask_writer = -1;
  }
  else {
    assert(cycle_writer->tme_bus_cycle_buffer_increment == 1);
    buffer_increment_mask_writer = 0;
  }
#define _TME_BUS_CYCLE_BUFFER_MULTIPLY(value, mask) \
  (((value) ^ (mask)) + ((mask) & 1))

  /* get the sizes, in bytes, of the reader and writer ports: */
  port_size_reader = (1 << TME_BUS_CYCLE_PORT_SIZE_LG2(cycle_reader->tme_bus_cycle_port));
  port_size_writer = (1 << TME_BUS_CYCLE_PORT_SIZE_LG2(cycle_writer->tme_bus_cycle_port));

  /* determine how the writer's port and the reader's port overlap: */
  port_overlap_size = port_size_writer;
  port_overlap_lane_least = TME_BUS_CYCLE_PORT_LANE_LEAST(cycle_writer->tme_bus_cycle_port);
  lane = TME_BUS_CYCLE_PORT_LANE_LEAST(cycle_reader->tme_bus_cycle_port);
  if (port_overlap_lane_least < lane) {
    port_overlap_size -= (lane - port_overlap_lane_least);
    port_overlap_lane_least = lane;
  }
  lane += port_size_reader;
  if ((port_overlap_lane_least + port_overlap_size) > lane) {
    port_overlap_size -= (lane - (port_overlap_lane_least + port_overlap_size));
  }
  assert(port_overlap_size > 0);
  for (port_overlap_size_lg2 = 0;
       (port_overlap_size >>= 1) != 0;
       port_overlap_size_lg2++);

  /* select the reader's lane routing: */
  lane_routing_offset_reader =
    TME_BUS_ROUTER_INDEX(TME_BUS_CYCLE_PORT_SIZE_LG2(cycle_reader->tme_bus_cycle_port),
			 port_overlap_size_lg2,
			 port_overlap_lane_least
			 - TME_BUS_CYCLE_PORT_LANE_LEAST(cycle_reader->tme_bus_cycle_port));

  /* select the writer's lane routing: */
  lane_routing_offset_writer =
    TME_BUS_ROUTER_INDEX(TME_BUS_CYCLE_PORT_SIZE_LG2(cycle_writer->tme_bus_cycle_port),
			 port_overlap_size_lg2,
			 port_overlap_lane_least
			 - TME_BUS_CYCLE_PORT_LANE_LEAST(cycle_writer->tme_bus_cycle_port));

  /* loop over all byte lanes in one or both ports: */
  lane = TME_MIN(TME_BUS_CYCLE_PORT_LANE_LEAST(cycle_reader->tme_bus_cycle_port),
		 TME_BUS_CYCLE_PORT_LANE_LEAST(cycle_writer->tme_bus_cycle_port));
  lane_end = TME_MAX(TME_BUS_CYCLE_PORT_LANE_LEAST(cycle_reader->tme_bus_cycle_port) + port_size_reader,
		     TME_BUS_CYCLE_PORT_LANE_LEAST(cycle_writer->tme_bus_cycle_port) + port_size_writer);
  cycle_size_reader = cycle_size_writer = 0;
  for (; lane < lane_end; lane++) {

    /* assume that we won't have to warn on this lane: */
    warn_on_lane = FALSE;

    /* see if this lane falls in the reader or writer's port: */
    lane_reader = lane - TME_BUS_CYCLE_PORT_LANE_LEAST(cycle_reader->tme_bus_cycle_port);
    lane_writer = lane - TME_BUS_CYCLE_PORT_LANE_LEAST(cycle_writer->tme_bus_cycle_port);
    lane_in_reader = (lane_reader >= 0 && lane_reader < port_size_reader);
    lane_in_writer = (lane_writer >= 0 && lane_writer < port_size_writer);

    /* get the value being written to this byte lane.  assume a
       garbage value: */
    lane_value = 0xd2;

    /* if this lane is in the writer's port, it may supply a real
       lane value: */
    if (lane_in_writer) {

      /* get the routing for the writer: */
      lane_routing_writer = 
	cycle_writer->tme_bus_cycle_lane_routing[lane_routing_offset_writer + lane_writer];

      /* if the writer doesn't expect this lane to be connected to the
	 reader, we will issue a warning on this lane: */
      if ((lane_routing_writer & TME_BUS_LANE_WARN)
	  && lane_in_reader) {
	warn_on_lane = TRUE;
      }
      lane_routing_writer &= ~TME_BUS_LANE_WARN;

      /* dispatch on the routing to get the lane value: */
      if (lane_routing_writer == TME_BUS_LANE_ABORT) {
	abort();
      }
      else if (lane_routing_writer != TME_BUS_LANE_UNDEF) {
	if (!(lane_routing_writer & TME_BUS_LANE_ROUTE_WRITE_IGNORE)
	    && lane_routing_writer >= cycle_size_writer) {
	  cycle_size_writer = lane_routing_writer + 1;
	}
	lane_routing_writer &= ~TME_BUS_LANE_ROUTE_WRITE_IGNORE;

	/* if the writer is the responder, make sure that only bytes
	   in the given register are ever referenced.  given the
	   writer's port size, we could warp the reference index as
	   needed, but hopefully we'll never have to: */
	assert(!(cycle_writer == cycle_resp
		 && (((cycle_writer->tme_bus_cycle_address + lane_routing_writer)
		      ^  cycle_writer->tme_bus_cycle_address)
		     & ~(port_size_writer - 1)) != 0));

	lane_value =
	  *(cycle_writer->tme_bus_cycle_buffer
	    + _TME_BUS_CYCLE_BUFFER_MULTIPLY(lane_routing_writer,
					     buffer_increment_mask_writer));
      }
    }

    /* if this lane is in the reader's port, it may take the lane
       value: */
    if (lane_in_reader) {

      /* get the routing for the reader: */
      lane_routing_reader =
	cycle_reader->tme_bus_cycle_lane_routing[lane_routing_offset_reader + lane_reader];

      /* if the reader doesn't expect this lane to be connected to the
	 writer, we will issue a warning on this lane: */
      if ((lane_routing_reader & TME_BUS_LANE_WARN)
	  && lane_in_writer) {
	warn_on_lane = TRUE;
      }
      lane_routing_reader &= ~TME_BUS_LANE_WARN;

      /* dispatch on the routing to take the lane value: */
      if (lane_routing_reader == TME_BUS_LANE_ABORT) {
	abort();
      }
      else if (lane_routing_reader != TME_BUS_LANE_UNDEF
	       && !(lane_routing_reader & TME_BUS_LANE_ROUTE_WRITE_IGNORE)) {
	if (lane_routing_reader >= cycle_size_reader) {
	  cycle_size_reader = lane_routing_reader + 1;
	}

	/* if the reader is the responder, make sure that only bytes
	   in the given register are ever referenced.  given the
	   reader's port size, we could warp the reference index as
	   needed, but hopefully we'll never have to: */
	assert(!(cycle_reader == cycle_resp
		 && (((cycle_reader->tme_bus_cycle_address + lane_routing_reader)
		      ^  cycle_reader->tme_bus_cycle_address)
		     & ~(port_size_reader - 1)) != 0));

	*(cycle_reader->tme_bus_cycle_buffer
	  + _TME_BUS_CYCLE_BUFFER_MULTIPLY(lane_routing_reader,
					   buffer_increment_mask_reader)) =
	  lane_value;
      }
    }

    /* if we need to issue a warning on this lane: */
    if (warn_on_lane) {
      /* XXX TBD: */
      abort();
    }
  }

  /* give the reader feedback: */
  cycle_reader->tme_bus_cycle_size = cycle_size_reader;
  cycle_reader->tme_bus_cycle_address += cycle_size_reader;
  cycle_reader->tme_bus_cycle_buffer += 
    _TME_BUS_CYCLE_BUFFER_MULTIPLY(cycle_size_reader,
				   buffer_increment_mask_reader);
  cycle_reader->tme_bus_cycle_lane_routing += lane_routing_offset_reader;
  cycle_reader->tme_bus_cycle_port = 
    TME_BUS_CYCLE_PORT(port_overlap_lane_least, port_overlap_size_lg2);
  
  /* give the writer feedback: */
  cycle_writer->tme_bus_cycle_size = cycle_size_writer;
  cycle_writer->tme_bus_cycle_address += cycle_size_writer;
  cycle_writer->tme_bus_cycle_buffer += 
    _TME_BUS_CYCLE_BUFFER_MULTIPLY(cycle_size_writer,
				   buffer_increment_mask_writer);
  cycle_writer->tme_bus_cycle_lane_routing += lane_routing_offset_writer;
  cycle_writer->tme_bus_cycle_port = 
    TME_BUS_CYCLE_PORT(port_overlap_lane_least, port_overlap_size_lg2);
}

/* this handles a bus cycle for a memory-like device: */
void
tme_bus_cycle_xfer_memory(struct tme_bus_cycle *cycle_init, tme_uint8_t *memory, tme_bus_addr_t address_last)
{
  tme_uint8_t memory_junk[sizeof(tme_bus_addr_t)];
  struct tme_bus_cycle cycle_resp;

  /* check the starting address: */
  assert(cycle_init->tme_bus_cycle_address <= address_last);

  /* get the start of the buffer for this starting address: */
  if (memory != NULL) {
    memory += cycle_init->tme_bus_cycle_address;
  }
  else {
    assert(sizeof(memory_junk)
	   >= ((unsigned int) 1 << TME_BUS_CYCLE_PORT_SIZE_LG2(cycle_init->tme_bus_cycle_port)));
    memory = memory_junk;
  }

  /* create the responder cycle: */
  cycle_resp.tme_bus_cycle_buffer = memory;
  cycle_resp.tme_bus_cycle_buffer_increment = 1;
  cycle_resp.tme_bus_cycle_lane_routing = cycle_init->tme_bus_cycle_lane_routing;
  cycle_resp.tme_bus_cycle_address = cycle_init->tme_bus_cycle_address;
  cycle_resp.tme_bus_cycle_type = (cycle_init->tme_bus_cycle_type
				   ^ (TME_BUS_CYCLE_WRITE
				      | TME_BUS_CYCLE_READ));
  cycle_resp.tme_bus_cycle_port = cycle_init->tme_bus_cycle_port;

  /* run the cycle: */
  tme_bus_cycle_xfer(cycle_init, &cycle_resp);

  /* check the finishing address: */
  assert((cycle_init->tme_bus_cycle_address - 1) <= address_last);
}
  
