/* $Id: bus-el.c,v 1.7 2003/05/16 21:48:08 fredette Exp $ */

/* generic/gen-bus-el.c - a real generic bus element: */

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
_TME_RCSID("$Id: bus-el.c,v 1.7 2003/05/16 21:48:08 fredette Exp $");

/* includes: */
#include <tme/generic/bus.h>
#include <stdlib.h>
#include <string.h>

/* macros: */

/* structures: */

/* this handles a bus connection signal edge: */
static int
_tme_bus_signal(struct tme_bus_connection *conn_bus_edger, unsigned int signal)
{
  struct tme_bus *bus;
  struct tme_bus_connection_int *conn_bus_int_edger;
  struct tme_bus_connection_int *conn_bus_int;
  unsigned int level_edge;
  struct tme_bus_connection *conn_bus;
  struct tme_bus_connection *conn_bus_other;
  int signal_asserted, need_propagate;
  unsigned int signal_index;
  tme_uint8_t signal_mask;
  int rc;
  int deadlocked;

  /* recover our bus: */
  bus = conn_bus_edger->tme_bus_connection.tme_connection_element->tme_element_private;
  conn_bus_int_edger = (struct tme_bus_connection_int *) conn_bus_edger;

  /* take out the level and edge: */
  level_edge = signal & (TME_BUS_SIGNAL_LEVEL_MASK
			 | TME_BUS_SIGNAL_EDGE);
  signal &= ~(TME_BUS_SIGNAL_LEVEL_MASK
	      | TME_BUS_SIGNAL_EDGE);

  /* lock the bus for writing: */
  rc = tme_rwlock_timedwrlock(&bus->tme_bus_rwlock, TME_THREAD_TIMEDLOCK);
  if (TME_THREADS_ERRNO(rc) != TME_OK) {
    return (TME_THREADS_ERRNO(rc));
  }
  
  /* if this device doesn't know its interrupt signal, fix it: */
  if (signal == TME_BUS_SIGNAL_INT_UNSPEC) {
    signal = conn_bus_int_edger->tme_bus_connection_int_signal_int;
    if (signal == TME_BUS_SIGNAL_INT_UNSPEC) {
      /* this bus connection is misconfigured: */
      if (!conn_bus_int_edger->tme_bus_connection_int_logged_int) {
	conn_bus_int_edger->tme_bus_connection_int_logged_int = TRUE;
	/* XXX diagnostic */
	abort();
      }
      tme_rwlock_unlock(&bus->tme_bus_rwlock);
      return (TME_OK);
    }
  }

  /* assume we don't need to propagate this signal across the bus: */
  need_propagate = FALSE;

  /* if this is a random bus-specific signal, just propagate it: */
  if (TME_BUS_SIGNAL_IS_X(signal)) {
    need_propagate = TRUE;
  }

  /* otherwise, this is a generic bus signal: */
  else {
    
    /* decide whether the device is asserting or negating this signal: */
    /* XXX we currently assume that for all bus signals handled by this
       function, low implies assertion, and signals are always ORed
       together.  this could be configurable, possibly even per-signal: */
    signal_asserted = TRUE;
    switch (level_edge & TME_BUS_SIGNAL_LEVEL_MASK) {
    case TME_BUS_SIGNAL_LEVEL_LOW: 
    case TME_BUS_SIGNAL_LEVEL_ASSERTED:
      break;
    case TME_BUS_SIGNAL_LEVEL_HIGH: 
    case TME_BUS_SIGNAL_LEVEL_NEGATED:
      signal_asserted = FALSE;
      break;
    }
    level_edge = ((level_edge & TME_BUS_SIGNAL_EDGE)
		  | (signal_asserted
		     ? TME_BUS_SIGNAL_LEVEL_ASSERTED
		     : TME_BUS_SIGNAL_LEVEL_NEGATED));

    /* get the index and mask of this signal in signal byte arrays: */
    signal_index = TME_BUS_SIGNAL_BIT_INDEX(signal);
    signal_mask = TME_BUS_SIGNAL_BIT_MASK(signal);

    /* if this signal is being asserted: */
    if (signal_asserted) {

      /* if this device wasn't already asserting this signal: */
      if (!(conn_bus_int_edger->tme_bus_connection_int_signals[signal_index]
	    & signal_mask)) {

	/* it is now asserting this signal: */
	conn_bus_int_edger->tme_bus_connection_int_signals[signal_index]
	  |= signal_mask;
	bus->tme_bus_signal_asserts[TME_BUS_SIGNAL_WHICH(signal)]++;

	/* if this is the only device asserting this signal,
	   propagate the change across the bus: */
	if (bus->tme_bus_signal_asserts[TME_BUS_SIGNAL_WHICH(signal)] == 1) {
	  need_propagate = TRUE;
	}
      }

      /* otherwise, this device was already asserting this signal: */
      else {
	assert(bus->tme_bus_signal_asserts[TME_BUS_SIGNAL_WHICH(signal)] > 0);
      }
    }

    /* otherwise, this signal is being negated: */
    else {

      /* if this device was asserting this signal: */
      if (conn_bus_int_edger->tme_bus_connection_int_signals[signal_index]
	  & signal_mask) {

	/* it is no longer asserting this signal: */
	conn_bus_int_edger->tme_bus_connection_int_signals[signal_index]
	  &= ~signal_mask;
	assert(bus->tme_bus_signal_asserts[TME_BUS_SIGNAL_WHICH(signal)] > 0);
	bus->tme_bus_signal_asserts[TME_BUS_SIGNAL_WHICH(signal)]--;
      }

      /* we always propagate across the bus a signal that some device
	 has negated, because often code will lazily only send
	 negating edges, or other devices might still be asserting it,
	 we assume these signals are always ORed together, and it
	 kicks (even though this signal isn't edging) emulated
	 circuitry that is meant to do things like interrupt
	 arbitration, etc.: */
      if (bus->tme_bus_signal_asserts[TME_BUS_SIGNAL_WHICH(signal)] > 0) {
	level_edge = TME_BUS_SIGNAL_LEVEL_ASSERTED;
      }
      need_propagate = TRUE;
    }
  }

  /* if we're propagating this signal across the bus: */
  rc = TME_OK;
  if (need_propagate) {

    /* put the level and edge back in: */
    signal |= level_edge;

    /* assume that we won't deadlock: */
    deadlocked = FALSE;

    /* propagate the signal to each connection to the bus: */
    for (conn_bus_int = bus->tme_bus_connections;
	 conn_bus_int != NULL;
	 conn_bus_int =
	   (struct tme_bus_connection_int *) 
	   conn_bus_int->tme_bus_connection_int
	   .tme_bus_connection
	   .tme_connection_next) {
      conn_bus = &conn_bus_int->tme_bus_connection_int;
      conn_bus_other = 
	(struct tme_bus_connection *) 
	conn_bus->tme_bus_connection.tme_connection_other;
      
      /* skip this device if it edged the line to begin with: */
      if (conn_bus == conn_bus_edger) {
	continue;
      }

      /* skip this device if it doesn't care about bus signals: */
      if (conn_bus_other->tme_bus_signal == NULL) {
	continue;
      }

      /* give the edge to this connection: */
      rc =  (*conn_bus_other->tme_bus_signal)(conn_bus_other, signal);

      /* if we deadlocked, remember to tell the caller: */
      if (rc == TME_EDEADLK) {
	deadlocked = TRUE;
      }
    }
    rc = (deadlocked ? TME_EDEADLK : TME_OK);
  }

  /* unlock the bus: */
  tme_rwlock_unlock(&bus->tme_bus_rwlock);

  /* done: */
  return (rc);
}
			      
/* this handles a bus interrupt acknowledge: */
static int
_tme_bus_intack(struct tme_bus_connection *conn_bus_acker, unsigned int signal, int *vector)
{
  struct tme_bus *bus;
  struct tme_bus_connection_int *conn_bus_int;
  struct tme_bus_connection *conn_bus;
  struct tme_bus_connection *conn_bus_other;
  unsigned int signal_index;
  tme_uint8_t signal_mask;
  int rc;

  /* recover our bus: */
  bus = conn_bus_acker->tme_bus_connection.tme_connection_element->tme_element_private;

  /* get rid of any level and edge: */
  signal &= ~(TME_BUS_SIGNAL_LEVEL_MASK
	      | TME_BUS_SIGNAL_EDGE);

  /* this must be an interrupt signal: */
  assert(TME_BUS_SIGNAL_IS_INT(signal));

  /* lock the bus for writing: */
  rc = tme_rwlock_timedwrlock(&bus->tme_bus_rwlock, TME_THREAD_TIMEDLOCK);
  if (TME_THREADS_ERRNO(rc) != TME_OK) {
    return (TME_THREADS_ERRNO(rc));
  }
  
  /* get the index and mask of this signal in signal byte arrays: */
  signal_index = TME_BUS_SIGNAL_BIT_INDEX(signal);
  signal_mask = TME_BUS_SIGNAL_BIT_MASK(signal);

  /* find the first connection to the bus that is asserting this
     interrupt signal.  if no connection is asserting the signal,
     return ENOENT: */
  rc = ENOENT;
  for (conn_bus_int = bus->tme_bus_connections;
       conn_bus_int != NULL;
       conn_bus_int =
	 (struct tme_bus_connection_int *) 
	 conn_bus_int->tme_bus_connection_int
	 .tme_bus_connection
	 .tme_connection_next) {
    conn_bus = &conn_bus_int->tme_bus_connection_int;
    conn_bus_other = 
      (struct tme_bus_connection *) 
      conn_bus->tme_bus_connection.tme_connection_other;
    
    /* if this device is asserting this interrupt signal: */
    if (conn_bus_int->tme_bus_connection_int_signals[signal_index]
	& signal_mask) {

      /* if this device doesn't acknowledge interrupts, 
	 return an undefined vector: */
      if (conn_bus_other->tme_bus_intack == NULL) {
	*vector = TME_BUS_INTERRUPT_VECTOR_UNDEF;
	rc = TME_OK;
      }

      /* otherwise, run the interrupt acknowledge with this connection: */
      else {
	rc = (*conn_bus_other->tme_bus_intack)(conn_bus_other, signal, vector);
      }

      /* stop: */
      break;
    }
  }

  /* unlock the bus: */
  tme_rwlock_unlock(&bus->tme_bus_rwlock);

  /* done: */
  return (rc);
}

static int
_tme_bus_fault(void *junk0, struct tme_bus_cycle *junk1)
{
  return (ENOENT);
}

/* this fills a TLB entry: */
static int
_tme_bus_tlb_fill(struct tme_bus_connection *conn_bus_asker, 
		  struct tme_bus_tlb *tlb,
		  tme_bus_addr_t address, 
		  unsigned int cycles)
{
  struct tme_bus *bus;
  struct tme_bus_connection_int *conn_int;
  int rc;

  /* recover our bus and our connection to the asker: */
  bus = conn_bus_asker->tme_bus_connection.tme_connection_element->tme_element_private;
  conn_int = (struct tme_bus_connection_int *) conn_bus_asker;

  /* put our fault handler in the TLB entry: */
  tlb->tme_bus_tlb_cycle_private = NULL;
  tlb->tme_bus_tlb_cycle = _tme_bus_fault;

  /* lock the bus for reading: */
  rc = tme_rwlock_timedrdlock(&bus->tme_bus_rwlock, TME_THREAD_TIMEDLOCK);
  if (TME_THREADS_ERRNO(rc) != TME_OK) {
    return (TME_THREADS_ERRNO(rc));
  }

  /* call the generic bus support function: */
  rc = tme_bus_tlb_fill(bus,
			conn_int,
			tlb, address, cycles);

  /* unlock the bus: */
  tme_rwlock_unlock(&bus->tme_bus_rwlock);

  /* done: */
  return (rc);
}

/* this allocates a new TLB set: */
static int
_tme_bus_tlb_set_allocate(struct tme_bus_connection *conn_bus_asker,
			  unsigned int count, unsigned int sizeof_one, 
			  TME_ATOMIC_POINTER_TYPE(struct tme_bus_tlb **) _tlbs)
{
  struct tme_bus *bus;
  struct tme_bus_connection_int *conn_int;
  int rc;

  /* recover our bus and our connection to the asker: */
  bus = conn_bus_asker->tme_bus_connection.tme_connection_element->tme_element_private;
  conn_int = (struct tme_bus_connection_int *) conn_bus_asker;

  /* lock the bus for reading: */
  rc = tme_rwlock_timedrdlock(&bus->tme_bus_rwlock, TME_THREAD_TIMEDLOCK);
  if (TME_THREADS_ERRNO(rc) != TME_OK) {
    return (TME_THREADS_ERRNO(rc));
  }

  /* call the generic bus support function: */
  rc = tme_bus_tlb_set_allocate(bus,
				conn_int,
				count, sizeof_one, 
				_tlbs);

  /* unlock the bus: */
  tme_rwlock_unlock(&bus->tme_bus_rwlock);

  /* done: */
  return (rc);
}

/* this scores a new connection: */
static int
_tme_bus_connection_score(struct tme_connection *conn, unsigned int *_score)
{
  struct tme_bus *bus;
  struct tme_bus_connection_int *conn_int;
  int rc, ok;

  /* both sides must be generic bus connections: */
  assert(conn->tme_connection_type == TME_CONNECTION_BUS_GENERIC);
  assert(conn->tme_connection_other->tme_connection_type == TME_CONNECTION_BUS_GENERIC);

  /* recover our bus and our internal connection side: */
  bus = conn->tme_connection_element->tme_element_private;
  conn_int = (struct tme_bus_connection_int *) conn;

  /* get the address mask from the other side: */
  conn_int->tme_bus_connection_int_address_last = 
    ((struct tme_bus_connection *) conn->tme_connection_other)->tme_bus_address_last;

  /* lock the bus for reading: */
  rc = tme_rwlock_timedrdlock(&bus->tme_bus_rwlock, TME_THREAD_TIMEDLOCK);
  if (TME_THREADS_ERRNO(rc) != TME_OK) {
    return (TME_THREADS_ERRNO(rc));
  }
  
  /* call the generic bus support function: */
  ok = tme_bus_connection_ok(bus,
			     conn_int);

  /* unlock the bus: */
  tme_rwlock_unlock(&bus->tme_bus_rwlock);

  /* return the score: */
  *_score = (ok ? 1 : 0);
  return (TME_OK);
}

/* this makes a new connection: */
static int
_tme_bus_connection_make(struct tme_connection *conn, unsigned int state)
{
  struct tme_bus *bus;
  struct tme_bus_connection_int *conn_int;
  int rc;

  /* both sides must be generic bus connections: */
  assert(conn->tme_connection_type == TME_CONNECTION_BUS_GENERIC);
  assert(conn->tme_connection_other->tme_connection_type == TME_CONNECTION_BUS_GENERIC);

  /* recover our bus and our internal connection side: */
  bus = conn->tme_connection_element->tme_element_private;
  conn_int = (struct tme_bus_connection_int *) conn;
  
  /* lock the bus for writing: */
  rc = tme_rwlock_timedwrlock(&bus->tme_bus_rwlock, TME_THREAD_TIMEDLOCK);
  if (TME_THREADS_ERRNO(rc) != TME_OK) {
    return (TME_THREADS_ERRNO(rc));
  }

  /* call the generic bus support function: */
  rc = tme_bus_connection_make(bus,
			       conn_int,
			       state);

  /* unlock the bus: */
  tme_rwlock_unlock(&bus->tme_bus_rwlock);

  return (rc);
}

/* this breaks a connection: */
static int 
_tme_bus_connection_break(struct tme_connection *conn, unsigned int state)
{
  abort();
}

/* this returns the new connections possible: */
static int
_tme_bus_connections_new(struct tme_element *element,
			 const char * const *args,
			 struct tme_connection **_conns,
			 char **_output)
{
  const struct tme_bus *bus;
  struct tme_bus_connection_int *conn_int;
  struct tme_bus_connection *conn_bus;
  struct tme_connection *conn;
  int ipl;
  int arg_i;
  int usage;

  /* recover our bus.  we only read the address mask, so we don't lock
     the rwlock: */
  bus = element->tme_element_private;

  /* allocate the new connection side: */
  conn_int = tme_new0(struct tme_bus_connection_int, 1);
  conn_bus = &conn_int->tme_bus_connection_int;
  conn = &conn_bus->tme_bus_connection;

  /* loop reading our arguments: */
  usage = FALSE;
  arg_i = 1;
  for (;;) {

    /* the address of this connection: */
    if (TME_ARG_IS(args[arg_i + 0], "addr")) {
      conn_int->tme_bus_connection_int_addressable = TRUE;
      conn_int->tme_bus_connection_int_address = tme_bus_addr_parse_any(args[arg_i + 1], &usage);
      if (usage
	  || (conn_int->tme_bus_connection_int_address
	      > bus->tme_bus_address_mask)) {
	usage = TRUE;
	break;
      }
      arg_i += 2;
    }

    /* the interrupt signal for this connection: */
    else if (TME_ARG_IS(args[arg_i + 0], "ipl")
	     && args[arg_i + 1] != NULL
	     && (ipl = atoi(args[arg_i + 1])) > 0) {
      conn_int->tme_bus_connection_int_signal_int = TME_BUS_SIGNAL_INT(ipl);
      arg_i += 2;
    }

    /* if we've run out of arguments: */
    else if (args[arg_i + 0] == NULL) {
      break;
    }

    /* this is a bad argument: */
    else {
      tme_output_append_error(_output,
			      "%s %s, ",
			      args[arg_i],
			      _("unexpected"));
      usage = TRUE;
      break;
    }
  }

  if (usage) {
    tme_output_append_error(_output, 
			    "%s %s [ addr %s ] [ ipl %s ]",
			    _("usage:"),
			    args[0],
			    _("BUS-ADDRESS"),
			    _("INTERRUPT-LEVEL"));
    tme_free(conn_int);
    return (EINVAL);
  }

  /* fill in the bus connection: */
  conn_bus->tme_bus_address_last = bus->tme_bus_address_mask;
  conn_bus->tme_bus_signal = _tme_bus_signal;
  conn_bus->tme_bus_intack = _tme_bus_intack;
  conn_bus->tme_bus_tlb_set_allocate = _tme_bus_tlb_set_allocate;
  conn_bus->tme_bus_tlb_fill = _tme_bus_tlb_fill;

  /* fill in the generic connection: */
  conn->tme_connection_next = *_conns;
  conn->tme_connection_type = TME_CONNECTION_BUS_GENERIC;
  conn->tme_connection_score = _tme_bus_connection_score;
  conn->tme_connection_make = _tme_bus_connection_make;
  conn->tme_connection_break = _tme_bus_connection_break;
  
  /* return the new connection side: */
  *_conns = conn;
  return (TME_OK);
}

/* this creates a new bus element: */
TME_ELEMENT_SUB_NEW_DECL(tme_generic,bus) {
  struct tme_bus *bus;
  tme_bus_addr_t bus_size;
  int failed;

  /* our arguments must include the bus size, and the
     bus size must be a power of two: */
  failed = TRUE;
  bus_size = 0;
  if (TME_ARG_IS(args[1], "size")) {
    bus_size = tme_bus_addr_parse_any(args[2], &failed);
    if (bus_size & (bus_size - 1)) {
      failed = TRUE;
    }
  }
  if (failed) {
    tme_output_append_error(_output,
			    "%s %s size %s",
			    _("usage:"),
			    args[0],
			    _("SIZE"));
    return (EINVAL);
  }

  /* allocate and initialize the new bus: */
  bus = tme_new0(struct tme_bus, 1);
  tme_rwlock_init(&bus->tme_bus_rwlock);
  bus->tme_bus_address_mask = bus_size - 1;
  bus->tme_bus_addressables_count = 0;
  bus->tme_bus_addressables_size = 1;
  bus->tme_bus_addressables = tme_new(struct tme_bus_connection_int *,
				      bus->tme_bus_addressables_size);

  /* fill the element: */
  element->tme_element_private = bus;
  element->tme_element_connections_new = _tme_bus_connections_new;

  return (TME_OK);
}
