/* $Id: bus-device.c,v 1.5 2003/05/16 21:48:08 fredette Exp $ */

/* bus/bus-device.c - implementation of a generic bus device support: */

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
_TME_RCSID("$Id: bus-device.c,v 1.5 2003/05/16 21:48:08 fredette Exp $");

/* includes: */
#include <tme/generic/bus-device.h>

/* this scores a connection: */
int
tme_bus_device_connection_score(struct tme_connection *conn, unsigned int *_score)
{
  struct tme_bus_device *device;
  struct tme_bus_connection *conn_bus_other_old;
  struct tme_bus_connection *conn_bus_other_new;

  /* recover our device: */
  device = conn->tme_connection_element->tme_element_private;

  /* both sides must be generic bus connections: */
  assert(conn->tme_connection_type == TME_CONNECTION_BUS_GENERIC);
  assert(conn->tme_connection_other->tme_connection_type == TME_CONNECTION_BUS_GENERIC);

  /* a simple bus device can have multiple connections, but they all
     must be from the same bus.  the only way we check that is by
     comparing elements: */
  conn_bus_other_old = TME_ATOMIC_READ(struct tme_bus_connection *,
				       device->tme_bus_device_connection);
  conn_bus_other_new = (struct tme_bus_connection *) conn->tme_connection_other;
  if (conn_bus_other_old != NULL
      && (conn_bus_other_old->tme_bus_connection.tme_connection_element
	  != conn_bus_other_new->tme_bus_connection.tme_connection_element)) {
    *_score = 0;
    return (TME_OK);
  }

  /* otherwise, this is our first connection or another connection to
     the same element.  note that there's no good way to differentiate
     a connection to a bus from a connection to just another chip, so
     we always return a nonzero score here: */
  *_score = 1;
  return (TME_OK);
}

/* this makes a new connection: */
int
tme_bus_device_connection_make(struct tme_connection *conn, unsigned int state)
{
  struct tme_bus_device *device;

  /* recover our device: */
  device = conn->tme_connection_element->tme_element_private;

  /* both sides must be generic bus connections: */
  assert(conn->tme_connection_type == TME_CONNECTION_BUS_GENERIC);
  assert(conn->tme_connection_other->tme_connection_type == TME_CONNECTION_BUS_GENERIC);

  /* simple bus devices are always set up to answer calls across the
     connection, so we only have to do work when the connection
     has gone full, namely taking the other side of the
     connection: */
  if (state == TME_CONNECTION_FULL) {

    /* save our connection: */
    TME_ATOMIC_WRITE(struct tme_bus_connection *,
		     device->tme_bus_device_connection,
		     (struct tme_bus_connection *) conn->tme_connection_other);
  }

  return (TME_OK);
}

/* this breaks a connection: */
int
tme_bus_device_connection_break(struct tme_connection *conn, unsigned int state)
{
  abort();
}

static int
_tme_bus_device_signal(struct tme_bus_connection *conn_bus, unsigned int signal)
{
  struct tme_bus_device *device;
  device = conn_bus->tme_bus_connection.tme_connection_element->tme_element_private;
  return ((*device->tme_bus_device_signal)(device, signal));
}

static int
_tme_bus_device_intack(struct tme_bus_connection *conn_bus, unsigned int signal, int *vector)
{
  struct tme_bus_device *device;
  device = conn_bus->tme_bus_connection.tme_connection_element->tme_element_private;
  return ((*device->tme_bus_device_intack)(device, signal, vector));
}

static int
_tme_bus_device_tlb_fill(struct tme_bus_connection *conn_bus, struct tme_bus_tlb *tlb,
			 tme_bus_addr_t address, unsigned int cycles)
{
  struct tme_bus_device *device;
  device = conn_bus->tme_bus_connection.tme_connection_element->tme_element_private;
  return ((*device->tme_bus_device_tlb_fill)(device, tlb, address, cycles));
}

/* this makes a new connection side for a simple bus device: */
int
tme_bus_device_connections_new(struct tme_element *element,
			       const char * const *args,
			       struct tme_connection **_conns,
			       char **_output)
{
  struct tme_bus_device *device;
  struct tme_bus_connection *conn_bus;
  struct tme_connection *conn;

  /* recover our device: */
  device = (struct tme_bus_device *) element->tme_element_private;

  /* create our side of a generic bus connection: */
  conn_bus = tme_new0(struct tme_bus_connection, 1);
  conn = &conn_bus->tme_bus_connection;

  /* fill in the generic connection: */
  conn->tme_connection_next = *_conns;
  conn->tme_connection_type = TME_CONNECTION_BUS_GENERIC;
  conn->tme_connection_score = tme_bus_device_connection_score;
  conn->tme_connection_make = tme_bus_device_connection_make;
  conn->tme_connection_break = tme_bus_device_connection_break;

  /* fill in the generic bus connection: */
  conn_bus->tme_bus_address_last = device->tme_bus_device_address_last;
  if (device->tme_bus_device_signal != NULL) {
    conn_bus->tme_bus_signal = _tme_bus_device_signal;
  }
  if (device->tme_bus_device_intack != NULL) {
    conn_bus->tme_bus_intack = _tme_bus_device_intack;
  }
  conn_bus->tme_bus_tlb_fill = _tme_bus_device_tlb_fill;

  /* return the connection side possibility: */
  *_conns = conn;
  return (TME_OK);
}
