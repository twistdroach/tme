/* $Id: bus-device.h,v 1.4 2003/05/16 21:48:15 fredette Exp $ */

/* tme/gen-bus-device.h - header file for generic bus device support: */

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

#ifndef _TME_GENERIC_BUS_DEVICE_H
#define _TME_GENERIC_BUS_DEVICE_H

#include <tme/common.h>
_TME_RCSID("$Id: bus-device.h,v 1.4 2003/05/16 21:48:15 fredette Exp $");

/* includes: */
#include <tme/element.h>
#include <tme/generic/bus.h>

/* macros: */

/* structures: */

/* a bus device: */
struct tme_bus_device {
  
  /* backpointer to the device's element: */
  struct tme_element *tme_bus_device_element;

  /* this device's bus connection: */
  TME_ATOMIC(struct tme_bus_connection *, tme_bus_device_connection);

  /* the last address, starting from zero, for this device's bus connection: */
  tme_bus_addr_t tme_bus_device_address_last;

  /* the bus signal handler: */
  int (*tme_bus_device_signal) _TME_P((void *, unsigned int));

  /* the bus interrupt acknowledge handler: */
  int (*tme_bus_device_intack) _TME_P((void *, unsigned int, int *));

  /* the bus TLB entry filler: */
  int (*tme_bus_device_tlb_fill) _TME_P((void *, struct tme_bus_tlb *, tme_bus_addr_t, unsigned int));
};

/* prototypes: */
int tme_bus_device_connection_score _TME_P((struct tme_connection *, unsigned int *));
int tme_bus_device_connection_make _TME_P((struct tme_connection *, unsigned int));
int tme_bus_device_connection_break _TME_P((struct tme_connection *, unsigned int));
int tme_bus_device_connections_new _TME_P((struct tme_element *, _tme_const char * _tme_const *, struct tme_connection **, char **));

#endif /* !_TME_GENERIC_BUS_DEVICE_H */
