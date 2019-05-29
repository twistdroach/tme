/* $Id: scsi-bus.c,v 1.4 2003/10/16 02:35:21 fredette Exp $ */

/* scsi/scsi-bus.c - a generic SCSI bus element: */

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
_TME_RCSID("$Id: scsi-bus.c,v 1.4 2003/10/16 02:35:21 fredette Exp $");

/* includes: */
#include <tme/generic/scsi.h>
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else  /* HAVE_STDARG_H */
#include <varargs.h>
#endif /* HAVE_STDARG_H */

/* macros: */

/* the count of IDs: */
#define TME_SCSI_BUS_ID_COUNT	(sizeof(tme_scsi_data_t) * 8)

/* the callout flags: */
#define TME_SCSI_BUS_CALLOUT_CHECK		(0)
#define TME_SCSI_BUS_CALLOUT_RUNNING		TME_BIT(0)
#define TME_SCSI_BUS_CALLOUTS_MASK		(-2)
#define  TME_SCSI_BUS_CALLOUT_CYCLE		TME_BIT(1)

/* structures: */

/* a scsi bus: */
struct tme_scsi_bus {

  /* backpointer to our element: */
  struct tme_element *tme_scsi_bus_element;

  /* our mutex: */
  tme_mutex_t tme_scsi_bus_mutex;

  /* our connections: */
  struct tme_connection *tme_scsi_bus_connections;

  /* the callout flags: */
  int tme_scsi_bus_callout_flags;

  /* the current bus state: */
  tme_scsi_control_t tme_scsi_bus_control;
  tme_scsi_control_t tme_scsi_bus_data;
};

/* internal information about a SCSI connection: */
struct tme_scsi_connection_int {

  /* the external SCSI connection: */
  struct tme_scsi_connection tme_scsi_connection_int;

  /* the control and data lines currently asserted by this connection: */
  tme_scsi_control_t tme_scsi_connection_int_control;
  tme_scsi_data_t tme_scsi_connection_int_data;

  /* the SCSI bus state last reported to the connection: */
  tme_scsi_control_t tme_scsi_connection_int_last_control;
  tme_scsi_control_t tme_scsi_connection_int_last_data;

  /* any sequence that this connection is running, and the step
     within that sequence: */
  const struct tme_scsi_sequence *tme_scsi_connection_int_sequence;
  unsigned int tme_scsi_connection_int_sequence_step;

  /* any DMA structure for this connection: */
  struct tme_scsi_dma *tme_scsi_connection_int_dma;
  
  /* specific callout flags for this connection: */
  int tme_scsi_connection_int_callout_flags;
};

/* the predefined sequence atom type: */
typedef unsigned long tme_scsi_sequence_atom_t;

/* globals: */

/* atoms representing the predefined sequences.  the tme SCSI
   interface allows a bus implementation to return predefined
   sequences that are actually completely opaque - despite getting a
   struct tme_scsi_sequence *, SCSI device implementations are not
   allowed to even *dereference* a pointer returned by a bus
   implementation's tme_scsi_connection_sequence_get method.  we take
   advantage of this here to make a partial, optimized implementation: */
const struct {
  tme_scsi_sequence_atom_t tme_scsi_sequence_info_dma_initiator;
  tme_scsi_sequence_atom_t tme_scsi_sequence_info_dma_target;
  tme_scsi_sequence_atom_t tme_scsi_sequence_wait_select_half[TME_SCSI_BUS_ID_COUNT];
  tme_scsi_sequence_atom_t tme_scsi_sequence_wait_select_full[TME_SCSI_BUS_ID_COUNT];
  tme_scsi_sequence_atom_t tme_scsi_sequence_wait_change;
} _tme_scsi_bus_sequences;

/* the SCSI bus callout function.  it must be called with the mutex locked: */
static void
_tme_scsi_bus_callout(struct tme_scsi_bus *scsi_bus, int new_callouts)
{
  struct tme_scsi_connection_int *conn_int;
  struct tme_scsi_connection *conn_scsi;
  int callouts, later_callouts;
  tme_scsi_control_t control;
  tme_scsi_data_t data;
  int rc;
  
  /* add in any new callouts: */
  scsi_bus->tme_scsi_bus_callout_flags |= new_callouts;

  /* if this function is already running in another thread, simply
     return now.  the other thread will do our work: */
  if (scsi_bus->tme_scsi_bus_callout_flags
      & TME_SCSI_BUS_CALLOUT_RUNNING) {
    return;
  }

  /* callouts are now running: */
  scsi_bus->tme_scsi_bus_callout_flags
    |= TME_SCSI_BUS_CALLOUT_RUNNING;

  /* assume that we won't need any later callouts: */
  later_callouts = 0;

  /* loop while callouts are needed: */
  for (; ((callouts
	   = scsi_bus->tme_scsi_bus_callout_flags)
	  & TME_SCSI_BUS_CALLOUTS_MASK); ) {

    /* clear the needed callouts: */
    scsi_bus->tme_scsi_bus_callout_flags
      = (callouts
	 & ~TME_SCSI_BUS_CALLOUTS_MASK);
    callouts &= TME_SCSI_BUS_CALLOUTS_MASK;

    /* if we need to call out SCSI bus cycles: */
    if (callouts & TME_SCSI_BUS_CALLOUT_CYCLE) {

      /* loop over all devices on the bus: */
      for (conn_int
	     = ((struct tme_scsi_connection_int *)
		scsi_bus->tme_scsi_bus_connections);
	   conn_int != NULL;
	   conn_int
	     = ((struct tme_scsi_connection_int *)
		conn_int->tme_scsi_connection_int.tme_scsi_connection.tme_connection_next)) {
	
	/* if this device doesn't need a callout, continue: */
	if (!(conn_int->tme_scsi_connection_int_callout_flags
	      & TME_SCSI_BUS_CALLOUT_CYCLE)) {
	  continue;
	}

	/* clear the callout flag on this device: */
	conn_int->tme_scsi_connection_int_callout_flags
	  &= ~TME_SCSI_BUS_CALLOUT_CYCLE;

	/* get the current state of the bus: */
	control = scsi_bus->tme_scsi_bus_control;
	data = scsi_bus->tme_scsi_bus_data;

	/* remember this last bus state called out to this connection: */
	conn_int->tme_scsi_connection_int_last_control
	  = control;
	conn_int->tme_scsi_connection_int_last_data
	  = data;

	/* unlock the mutex: */
	tme_mutex_unlock(&scsi_bus->tme_scsi_bus_mutex);
	
	/* do the callout: */
	conn_scsi
	  = ((struct tme_scsi_connection *)
	     conn_int->tme_scsi_connection_int.tme_scsi_connection.tme_connection_other);
	rc = ((*conn_scsi->tme_scsi_connection_cycle)
	      (conn_scsi,
	       control,
	       data,
	       conn_int->tme_scsi_connection_int_sequence,
	       NULL));
	
	/* lock the mutex: */
	tme_mutex_lock(&scsi_bus->tme_scsi_bus_mutex);
      
	/* if the callout was unsuccessful, remember that at some later
	   time this callout should be attempted again: */
	if (rc != TME_OK) {
	  conn_int->tme_scsi_connection_int_callout_flags
	    |= TME_SCSI_BUS_CALLOUT_CYCLE;
	  later_callouts
	    |= TME_SCSI_BUS_CALLOUT_CYCLE;
	}
      }
    }
  }
  
  /* put in any later callouts, and clear that callouts are running: */
  scsi_bus->tme_scsi_bus_callout_flags = later_callouts;
}

/* this handles a SCSI bus cycle: */
static int
_tme_scsi_bus_cycle(struct tme_scsi_connection *conn_scsi,
		    tme_scsi_control_t control,
		    tme_scsi_data_t data,
		    const struct tme_scsi_sequence *sequence_asker,
		    struct tme_scsi_dma *dma)
{
  struct tme_scsi_bus *scsi_bus;
  struct tme_scsi_connection_int *conn_int_asker, *conn_int;
  const struct tme_scsi_sequence *sequence;
  struct tme_scsi_connection_int *dma_initiator;
  struct tme_scsi_connection_int *dma_target;
  struct tme_scsi_dma *dma_in, *dma_out;
  unsigned long count;
  int bus_changed;
  int new_callouts;
  int again;
  tme_scsi_data_t id;

  /* recover our bus and internal connection: */
  scsi_bus = conn_scsi->tme_scsi_connection.tme_connection_element->tme_element_private;
  conn_int_asker = (struct tme_scsi_connection_int *) conn_scsi;

  /* assume we won't need any new callouts: */
  new_callouts = 0;

  /* lock the mutex: */
  tme_mutex_lock(&scsi_bus->tme_scsi_bus_mutex);

  /* update the signals that this device is asserting: */
  conn_int_asker->tme_scsi_connection_int_control = control;
  conn_int_asker->tme_scsi_connection_int_data = data;

  /* update the sequence for this device: */
  if (sequence_asker != NULL) {

    /* being a partial implementation, we don't support device-defined
       sequences - any sequence must be a predefined sequence that we
       returned: */
    if ((sequence_asker
	 < (const struct tme_scsi_sequence *) (char *) &_tme_scsi_bus_sequences)
	|| (sequence_asker
	    >= (const struct tme_scsi_sequence *) (char *) (&_tme_scsi_bus_sequences + 1))) {
      abort();
    }
  }
  conn_int_asker->tme_scsi_connection_int_sequence = sequence_asker;
  conn_int_asker->tme_scsi_connection_int_sequence_step = 0;

  /* update the DMA structure for this device.  being a partial
     implementation, we only support 8-bit asynchronous DMA: */
  if (dma != NULL) {
    if (((dma->tme_scsi_dma_flags
	  & TME_SCSI_DMA_WIDTH)
	 != TME_SCSI_DMA_8BIT)
	|| (dma->tme_scsi_dma_sync_offset
	    != 0)) {
      abort();
    }
    if (dma->tme_scsi_dma_resid == 0) {
      conn_int_asker->tme_scsi_connection_int_callout_flags
	|= TME_SCSI_BUS_CALLOUT_CYCLE;
      conn_int_asker->tme_scsi_connection_int_sequence
	= NULL;
      new_callouts
	|= TME_SCSI_BUS_CALLOUT_CYCLE;
      dma = NULL;
    }
  }
  conn_int_asker->tme_scsi_connection_int_dma = dma;

  /* if during any iteration of the below loop, we see or cause a
     change on the bus, we want to call out cycles to all devices
     waiting on a simple change: */
  bus_changed = FALSE;

  /* loop until things settle down: */
  for (again = TRUE; again; ) {
    again = FALSE;

    /* get the current state of the bus: */
    control = 0;
    data = 0;
    for (conn_int
	   = ((struct tme_scsi_connection_int *)
	      scsi_bus->tme_scsi_bus_connections);
	 conn_int != NULL;
	 conn_int
	   = ((struct tme_scsi_connection_int *)
	      conn_int->tme_scsi_connection_int.tme_scsi_connection.tme_connection_next)) {
      control |= conn_int->tme_scsi_connection_int_control;
      data |= conn_int->tme_scsi_connection_int_data;
    }
    if ((control != scsi_bus->tme_scsi_bus_control)
	|| (data != scsi_bus->tme_scsi_bus_data)) {
      bus_changed = TRUE;
    }
    scsi_bus->tme_scsi_bus_control = control;
    scsi_bus->tme_scsi_bus_data = data;

    /* loop over all devices on the bus: */
    dma_initiator = NULL;
    dma_target = NULL;
    for (conn_int
	   = ((struct tme_scsi_connection_int *)
	      scsi_bus->tme_scsi_bus_connections);
	 conn_int != NULL;
	 conn_int
	   = ((struct tme_scsi_connection_int *)
	      conn_int->tme_scsi_connection_int.tme_scsi_connection.tme_connection_next)) {

      /* dispatch on this device's sequence: */
      sequence = conn_int->tme_scsi_connection_int_sequence;
#define SEQUENCE_IS(s, f)				\
  (((s)							\
    >= ((const struct tme_scsi_sequence *)		\
	&_tme_scsi_bus_sequences.f))			\
   && ((s)						\
       < ((const struct tme_scsi_sequence *)		\
	  (&_tme_scsi_bus_sequences.f			\
	   + 1))))
#define SEQUENCE_INDEX(s, f)				\
  (((const tme_scsi_sequence_atom_t *) (s))		\
   - &_tme_scsi_bus_sequences.f[0])

      /* a device with no sequence is ignoring the bus completely: */
      if (sequence == NULL) {
	/* nothing to do: */
      }

      /* a device in TME_SCSI_SEQUENCE_WAIT_CHANGE is waiting on any
	 change to the bus state: */
      else if (SEQUENCE_IS(sequence, tme_scsi_sequence_wait_change)) {

	/* if the bus has changed, callout a cycle on this device: */
	if (bus_changed
	    || (control !=
		conn_int->tme_scsi_connection_int_last_control)
	    || (data
		!= conn_int->tme_scsi_connection_int_last_data)) {
	  conn_int->tme_scsi_connection_int_callout_flags
	    |= TME_SCSI_BUS_CALLOUT_CYCLE;
	  conn_int->tme_scsi_connection_int_sequence
	    = NULL;
	  new_callouts
	    |= TME_SCSI_BUS_CALLOUT_CYCLE;
	}
      }

      /* a connection in TME_SCSI_SEQUENCE_WAIT_SELECT_HALF or
	 TME_SCSI_SEQUENCE_WAIT_SELECT_FULL is waiting to be selected: */
      else if (SEQUENCE_IS(sequence,
			   tme_scsi_sequence_wait_select_half)
	       || SEQUENCE_IS(sequence,
			      tme_scsi_sequence_wait_select_full)) {

	/* get the SCSI ID for the connection: */
	id = (SEQUENCE_IS(sequence,
			  tme_scsi_sequence_wait_select_half)
	      ? SEQUENCE_INDEX(sequence,
			       tme_scsi_sequence_wait_select_half)
	      : SEQUENCE_INDEX(sequence,
			       tme_scsi_sequence_wait_select_full));
	
	/* dispatch on the sequence step: */
	switch (conn_int->tme_scsi_connection_int_sequence_step) {

	  /* "In all systems, the target shall determine that it is
	     selected when SEL and its SCSI ID bit are true and BSY and
	     I/O are false for at least a bus settle delay." */
	case 0:
	  if (((control
		& (TME_SCSI_SIGNAL_BSY
		   | TME_SCSI_SIGNAL_SEL
		   | TME_SCSI_SIGNAL_I_O))
	       == TME_SCSI_SIGNAL_SEL)
	      && (data
		  & TME_BIT(id))) {

	    /* this device is being selected: */

	    /* if this device is in
	       TME_SCSI_SEQUENCE_WAIT_SELECT_HALF, callout a cycle on
	       this device: */
	    if (SEQUENCE_IS(sequence,
			    tme_scsi_sequence_wait_select_half)) {
	      conn_int->tme_scsi_connection_int_callout_flags
		|= TME_SCSI_BUS_CALLOUT_CYCLE;
	      conn_int->tme_scsi_connection_int_sequence
		= NULL;
	      new_callouts
		|= TME_SCSI_BUS_CALLOUT_CYCLE;
	    }

	    /* otherwise, this device is in
	       TME_SCSI_SEQUENCE_WAIT_SELECT_FULL.  assert BSY on its
	       behalf and advance to the next state: */
	    else {
	      conn_int->tme_scsi_connection_int_control
		|= TME_SCSI_SIGNAL_BSY;
	      conn_int->tme_scsi_connection_int_sequence_step++;
	      again = TRUE;
	      bus_changed = TRUE;
	    }
	  }
	  break;

	  /* "At least two deskew delays after the initiator detects
	     BSY is true, it shall release SEL and may change the DATA
	     BUS."

	     as the target, we wait for SEL to be negated: */
	case 1:
	  if (!(control
		& TME_SCSI_SIGNAL_SEL)) {
	  
	    /* callout a cycle on this device: */
	    conn_int->tme_scsi_connection_int_callout_flags
	      |= TME_SCSI_BUS_CALLOUT_CYCLE;
	    conn_int->tme_scsi_connection_int_sequence
	      = NULL;
	    new_callouts
	      |= TME_SCSI_BUS_CALLOUT_CYCLE;
	  }
	  break;
	}
      }

      /* there can be at most one device in an initiator or target
	 information transfer phase DMA sequence: */
      else if (SEQUENCE_IS(sequence, tme_scsi_sequence_info_dma_initiator)) {
	assert (dma_initiator == NULL);
	dma_initiator = conn_int;
      }
      else if (SEQUENCE_IS(sequence, tme_scsi_sequence_info_dma_target)) {
	assert (dma_target == NULL);
	dma_target = conn_int;
      }
    }

    /* if we need to loop again, do so immediately: */
    if (again) {
      continue;
    }

    /* if a device is in the initiator information transfer phase DMA
       sequence, but the information transfer phase has changed,
       callout a cycle on this device: */
    if (dma_initiator != NULL
	&& (TME_SCSI_PHASE(control)
	    != TME_SCSI_PHASE(dma_initiator->tme_scsi_connection_int_last_control))) {
      dma_initiator->tme_scsi_connection_int_callout_flags
	|= TME_SCSI_BUS_CALLOUT_CYCLE;
      dma_initiator->tme_scsi_connection_int_sequence
	= NULL;
      new_callouts
	|= TME_SCSI_BUS_CALLOUT_CYCLE;
      dma_initiator = NULL;
    }

    /* if initiator and target are both in their respective DMA
       sequences, we can do a bulk copy between them: */
    if (dma_initiator != NULL
	&& dma_target != NULL) {

      /* sort the devices' DMA structures into input and output: */
      if (control & TME_SCSI_SIGNAL_I_O) {
	dma_in = dma_initiator->tme_scsi_connection_int_dma;
	dma_out = dma_target->tme_scsi_connection_int_dma;
      }
      else {
	dma_out = dma_initiator->tme_scsi_connection_int_dma;
	dma_in = dma_target->tme_scsi_connection_int_dma;
      }
      assert (dma_out != NULL && dma_in != NULL);

      /* get the size of the bulk copy: */
      count = TME_MIN(dma_out->tme_scsi_dma_resid,
		      dma_in->tme_scsi_dma_resid);
      assert (count > 0);

      /* do the bulk copy: */
      memcpy(dma_in->tme_scsi_dma_in,
	     dma_out->tme_scsi_dma_out,
	     count);

      /* advance the DMA pointers: */
      dma_in->tme_scsi_dma_in += count;
      dma_out->tme_scsi_dma_out += count;

      /* if the target's DMA has been exhausted, be sure to negate
	 REQ, and callout a cycle on the device: */
      dma = dma_target->tme_scsi_connection_int_dma;
      if ((dma->tme_scsi_dma_resid -= count) == 0) {

	/* negate REQ on the target's behalf: */
	dma_target->tme_scsi_connection_int_control
	  &= ~TME_SCSI_SIGNAL_REQ;
	again = TRUE;
	bus_changed = TRUE;

	/* request the callout: */
	dma_target->tme_scsi_connection_int_callout_flags
	  |= TME_SCSI_BUS_CALLOUT_CYCLE;
	dma_target->tme_scsi_connection_int_sequence
	  = NULL;
	new_callouts
	  |= TME_SCSI_BUS_CALLOUT_CYCLE;

	/* no device is currently in the target information phase
           transfer DMA sequence: */
	dma_target = NULL;
      }

      /* otherwise, the target's DMA has not been exhausted.  be sure
	 that we return to state zero in the DMA target sequence: */
      else {
	dma_target->tme_scsi_connection_int_sequence_step = 0;
      }
      
      /* if the initiator's DMA has been exhausted, callout a cycle on
         the device: */
      dma = dma_initiator->tme_scsi_connection_int_dma;
      if ((dma->tme_scsi_dma_resid -= count) == 0) {

	/* request the callout: */
	dma_initiator->tme_scsi_connection_int_callout_flags
	  |= TME_SCSI_BUS_CALLOUT_CYCLE;
	dma_initiator->tme_scsi_connection_int_sequence
	  = NULL;
	new_callouts
	  |= TME_SCSI_BUS_CALLOUT_CYCLE;

	/* no device is currently in the initiator information phase
           transfer DMA sequence: */
	dma_initiator = NULL;
      }

      /* otherwise, the initiator's DMA has not been exhausted.  be sure
	 that we return to state zero in the DMA initiator sequence: */
      else {
	dma_initiator->tme_scsi_connection_int_sequence_step = 0;
      }      
    }

    /* if we need to loop again, do so immediately: */
    if (again) {
      continue;
    }

    /* if a device is in the target information transfer phase DMA
       sequence: */
    if (dma_target != NULL) {

      /* get this device's DMA structure: */
      dma = dma_target->tme_scsi_connection_int_dma;
      assert (dma != NULL);

      /* dispatch on the sequence step: */
      switch (dma_target->tme_scsi_connection_int_sequence_step) {

	/* "If I/O is true (transfer to the initiator)... [after] ACK
	   is false the target may continue the transfer by driving
	   DB(7-0,P) and asserting REQ, as described above."

	   "If I/O is false (transfer to the target)... [after ACK is
	   false the target] may continue the transfer by asserting
	   REQ, as described above." */
      case 2:
	if (control & TME_SCSI_SIGNAL_ACK) {
	  break;
	}

	/* if the DMA has been exhausted, callout a cycle on this
	   device: */
	if (control & TME_SCSI_SIGNAL_I_O) {
	  dma->tme_scsi_dma_out++;
	}
	else {
	  dma->tme_scsi_dma_in++;
	}
	if (--dma->tme_scsi_dma_resid == 0) {
	  dma_target->tme_scsi_connection_int_callout_flags
	    |= TME_SCSI_BUS_CALLOUT_CYCLE;
	  dma_target->tme_scsi_connection_int_sequence
	    = NULL;
	  new_callouts
	    |= TME_SCSI_BUS_CALLOUT_CYCLE;
	  break;
	}

	/* FALLTHROUGH */

      /* "If I/O is true (transfer to the initiator), the target shall
	 first drive DB(7-0,P) to their desired values, delay at least
	 one deskew delay plus a cable skew delay, then assert REQ."

	 "If I/O is false (transfer to the target) the target shall request 
	 information by asserting REQ. " */
      case 0:

	/* assert REQ on the target's behalf: */
	dma_target->tme_scsi_connection_int_control
	  |= TME_SCSI_SIGNAL_REQ;
	again = TRUE;
	bus_changed = TRUE;

	/* if I/O is asserted, assert the output data: */
	if (control & TME_SCSI_SIGNAL_I_O) {
	  dma_target->tme_scsi_connection_int_data
	    = *(dma->tme_scsi_dma_out);
	}

	/* advance to step one: */
	dma_target->tme_scsi_connection_int_sequence_step = 1;
	break;

	/* "If I/O is true (transfer to the initiator)... [when] ACK
	   becomes true at the target, the target may change or
	   release DB(7-0,P) and shall negate REQ."

	   "If I/O is false (transfer to the target)... [when] ACK
	   becomes true at the target, the target shall read
	   DB(7-0,P), then negate REQ." */
      case 1:
	if (control & TME_SCSI_SIGNAL_ACK) {

	  /* if I/O is negated, read the input data: */
	  if (!(control & TME_SCSI_SIGNAL_I_O)) {
	    *(dma->tme_scsi_dma_in) = data;
	  }

	  /* negate REQ on the target's behalf: */
	  dma_target->tme_scsi_connection_int_control
	    &= ~TME_SCSI_SIGNAL_REQ;
	  again = TRUE;
	  bus_changed = TRUE;
	
	  /* advance to step two: */
	  dma_target->tme_scsi_connection_int_sequence_step = 2;
	}
	break;

      default: assert (FALSE);
      }

      /* if the bus is being reset: */
      if (control & TME_SCSI_SIGNAL_RST) {

	/* negate all signals on the target's behalf: */
	dma_target->tme_scsi_connection_int_control = 0;
	dma_target->tme_scsi_connection_int_data = 0;
	again = TRUE;
	bus_changed = TRUE;
	
	/* callout a cycle on this device: */
	dma_target->tme_scsi_connection_int_callout_flags
	  |= TME_SCSI_BUS_CALLOUT_CYCLE;
	dma_target->tme_scsi_connection_int_sequence
	  = NULL;
	new_callouts
	  |= TME_SCSI_BUS_CALLOUT_CYCLE;
      }
    }

    /* if we need to loop again, do so immediately: */
    if (again) {
      continue;
    }

    /* if a device is in the initiator information transfer phase DMA
       sequence: */
    if (dma_initiator != NULL) {

      /* get this device's DMA structure: */
      dma = dma_initiator->tme_scsi_connection_int_dma;
      assert (dma != NULL);
      
      /* dispatch on the sequence step: */
      switch (dma_initiator->tme_scsi_connection_int_sequence_step) {
	
	/* "If I/O is true (transfer to the initiator)... [the]
	   initiator shall read DB(7-0,P) after REQ is true, then
	   signal its acceptance of the data by asserting ACK."
	   
	   "If I/O is false (transfer to the target)... [the]
	   initiator shall drive DB(7-0,P) to their desired values
	   [after REQ is true], delay at least one deskew delay plus a
	   cable skew delay and assert ACK." */
      case 0:
	if (!(control & TME_SCSI_SIGNAL_REQ)) {
	  break;
	}
	  
	/* if I/O is true, read the data, else
	   write the data: */
	if (control & TME_SCSI_SIGNAL_I_O) {
	  *dma->tme_scsi_dma_in = data;
	}
	else {
	  dma_initiator->tme_scsi_connection_int_data
	    = *(dma->tme_scsi_dma_out);
	}
	
	/* assert ACK on the initiator's behalf: */
	dma_initiator->tme_scsi_connection_int_control
	  |= TME_SCSI_SIGNAL_ACK;
	again = TRUE;
	bus_changed = TRUE;
	
	/* advance to step one: */
	dma_initiator->tme_scsi_connection_int_sequence_step = 1;
	break;
	
	/* "If I/O is true (transfer to the initiator)... [after]
	   REQ is false the initiator shall then negate ACK."
	   
	   "If I/O is false (transfer to the target)... [when]
	   REQ becomes false at the initiator, the initiator may 
	   change or release DB(7-0,P) and shall negate ACK." */
      case 1:
	if (control & TME_SCSI_SIGNAL_REQ) {
	  break;
	}
	
	/* negate ACK on the initiator's behalf: */
	dma_initiator->tme_scsi_connection_int_control
	  &= ~TME_SCSI_SIGNAL_ACK;
	again = TRUE;
	bus_changed = TRUE;
	
	/* if the DMA has been exhausted, callout a cycle on this
	   device: */
	if (control & TME_SCSI_SIGNAL_I_O) {
	  dma->tme_scsi_dma_in++;
	}
	else {
	  dma->tme_scsi_dma_out++;
	}
	if (--dma->tme_scsi_dma_resid == 0) {
	  dma_initiator->tme_scsi_connection_int_callout_flags
	    |= TME_SCSI_BUS_CALLOUT_CYCLE;
	  dma_initiator->tme_scsi_connection_int_sequence
	    = NULL;
	  new_callouts |= TME_SCSI_BUS_CALLOUT_CYCLE;
	}
	
	/* otherwise, advance to step zero: */
	else {
	  dma_initiator->tme_scsi_connection_int_sequence_step = 0;
	}
	break;
	
      default:
	assert (FALSE);
      }

      /* if the bus is being reset: */
      if (control & TME_SCSI_SIGNAL_RST) {

	/* negate all signals on the initiator's behalf: */
	dma_target->tme_scsi_connection_int_control = 0;
	dma_target->tme_scsi_connection_int_data = 0;
	again = TRUE;
	bus_changed = TRUE;
	
	/* callout a cycle on this device: */
	dma_target->tme_scsi_connection_int_callout_flags
	  |= TME_SCSI_BUS_CALLOUT_CYCLE;
	dma_target->tme_scsi_connection_int_sequence
	  = NULL;
	new_callouts
	  |= TME_SCSI_BUS_CALLOUT_CYCLE;
      }
    }
  }

  /* make any needed callouts: */
  _tme_scsi_bus_callout(scsi_bus, new_callouts);

  /* unlock the mutex: */
  tme_mutex_unlock(&scsi_bus->tme_scsi_bus_mutex);

  return (TME_OK);
}

/* this returns a predefined SCSI sequence: */
#ifdef HAVE_STDARG_H
static const struct tme_scsi_sequence *
_tme_scsi_bus_sequence_get(struct tme_scsi_connection *conn_scsi,
			   unsigned int sequence_type,
			   ...)
#else  /* HAVE_STDARG_H */
static const struct tme_scsi_sequence *_tme_scsi_bus_sequence_get(conn_scsi, sequence_type, va_alist)
     struct tme_scsi_connection *conn_scsi;
     unsigned int sequence_type;
     va_dcl
#endif /* HAVE_STDARG_H */
{
  va_list sequence_args;
  const struct tme_scsi_sequence *sequence;
  tme_scsi_data_t id0;

  /* start the variable arguments: */
#ifdef HAVE_STDARG_H
  va_start(sequence_args, sequence_type);
#else  /* HAVE_STDARG_H */
  va_start(sequence_args);
#endif /* HAVE_STDARG_H */

  /* dispatch on the sequence type: */
#define SEQUENCE(f) ((const struct tme_scsi_sequence *) &_tme_scsi_bus_sequences.f)
  switch (sequence_type) {
  case TME_SCSI_SEQUENCE_INFO_DMA_INITIATOR:
    sequence = SEQUENCE(tme_scsi_sequence_info_dma_initiator);
    break;
  case TME_SCSI_SEQUENCE_INFO_DMA_TARGET:
    sequence = SEQUENCE(tme_scsi_sequence_info_dma_target);
    break;
  case TME_SCSI_SEQUENCE_WAIT_SELECT_HALF:
    id0 = va_arg(sequence_args, tme_scsi_data_t);
    sequence = SEQUENCE(tme_scsi_sequence_wait_select_half[id0]);
    break;
  case TME_SCSI_SEQUENCE_WAIT_SELECT_FULL:
    id0 = va_arg(sequence_args, tme_scsi_data_t);
    sequence = SEQUENCE(tme_scsi_sequence_wait_select_full[id0]);
    break;
  case TME_SCSI_SEQUENCE_WAIT_CHANGE:
    sequence = SEQUENCE(tme_scsi_sequence_wait_change);
    break;
  default:
    abort();
  }
#undef SEQUENCE

  /* end the variable arguments: */
  va_end(sequence_args);

  return (sequence);
}

/* this scores a new connection: */
static int
_tme_scsi_bus_connection_score(struct tme_connection *conn,
			       unsigned int *_score)
{
  struct tme_scsi_bus *scsi_bus;
  struct tme_scsi_connection_int *conn_int_other;

  /* both sides must be SCSI connections: */
  assert (conn->tme_connection_type == TME_CONNECTION_SCSI);
  assert (conn->tme_connection_other->tme_connection_type == TME_CONNECTION_SCSI);

  /* recover our bus and the other internal connection side: */
  scsi_bus = conn->tme_connection_element->tme_element_private;
  conn_int_other = (struct tme_scsi_connection_int *) conn->tme_connection_other;

  /* you cannot connect a bus to a bus: */
  *_score
    = (conn_int_other->tme_scsi_connection_int.tme_scsi_connection_sequence_get
       == NULL);
  return (TME_OK);
}

/* this makes a new connection: */
static int
_tme_scsi_bus_connection_make(struct tme_connection *conn,
			      unsigned int state)
{
  struct tme_scsi_bus *scsi_bus;
  struct tme_scsi_connection_int *conn_int;

  /* both sides must be SCSI connections: */
  assert (conn->tme_connection_type == TME_CONNECTION_SCSI);
  assert (conn->tme_connection_other->tme_connection_type == TME_CONNECTION_SCSI);

  /* recover our bus and our internal connection side: */
  scsi_bus = conn->tme_connection_element->tme_element_private;
  conn_int = (struct tme_scsi_connection_int *) conn;
  
  /* we're always set up to answer calls across the connection,
     so we only have to do work when the connection has gone full,
     namely taking the other side of the connection: */
  if (state == TME_CONNECTION_FULL) {

    /* lock the mutex: */
    tme_mutex_lock(&scsi_bus->tme_scsi_bus_mutex);

    /* add this connection to our list of connections: */
    conn->tme_connection_next = scsi_bus->tme_scsi_bus_connections;
    scsi_bus->tme_scsi_bus_connections = conn;

    /* unlock the mutex: */
    tme_mutex_unlock(&scsi_bus->tme_scsi_bus_mutex);
  }

  return (TME_OK);
}

/* this breaks a connection: */
static int 
_tme_scsi_bus_connection_break(struct tme_connection *conn,
			       unsigned int state)
{
  abort();
}

/* this returns the new connections possible: */
static int
_tme_scsi_bus_connections_new(struct tme_element *element,
			      const char * const *args,
			      struct tme_connection **_conns,
			      char **_output)
{
  struct tme_scsi_connection_int *conn_int;
  struct tme_scsi_connection *conn_scsi;
  struct tme_connection *conn;

  /* we never take any arguments: */
  if (args[1] != NULL) {
    tme_output_append_error(_output,
			    "%s %s, ",
			    args[1],
			    _("unexpected"));
    return (EINVAL);
  }

  /* create our side of a SCSI connection: */
  conn_int = tme_new0(struct tme_scsi_connection_int, 1);
  conn_scsi = &conn_int->tme_scsi_connection_int;
  conn = &conn_scsi->tme_scsi_connection;

  /* fill in the generic connection: */
  conn->tme_connection_next = *_conns;
  conn->tme_connection_type = TME_CONNECTION_SCSI;
  conn->tme_connection_score = _tme_scsi_bus_connection_score;
  conn->tme_connection_make = _tme_scsi_bus_connection_make;
  conn->tme_connection_break = _tme_scsi_bus_connection_break;

  /* fill in the SCSI connection: */
  conn_scsi->tme_scsi_connection_cycle = _tme_scsi_bus_cycle;
  conn_scsi->tme_scsi_connection_sequence_get = _tme_scsi_bus_sequence_get;

  /* return the connection side possibility: */
  *_conns = conn;
  return (TME_OK);
}

/* this creates a new SCSI bus element: */
TME_ELEMENT_SUB_NEW_DECL(tme_scsi,bus) {
  struct tme_scsi_bus *scsi_bus;
  int usage;
  int arg_i;

  /* check our arguments: */
  arg_i = 1;
  usage = FALSE;

  /* loop reading our arguments: */
  for (;;) {

    if (0) {
    }

    /* if we've run out of arguments: */
    else if (args[arg_i + 0] == NULL) {

      break;
    }

    /* this is a bad argument: */
    else {
      tme_output_append_error(_output,
			      "%s %s", 
			      args[arg_i],
			      _("unexpected"));
      usage = TRUE;
      break;
    }
  }

  if (usage) {
    tme_output_append_error(_output, 
			    "%s %s",
			    _("usage:"),
			    args[0]);
    return (EINVAL);
  }

  /* allocate and initialize the new SCSI bus: */
  scsi_bus = tme_new0(struct tme_scsi_bus, 1);
  tme_mutex_init(&scsi_bus->tme_scsi_bus_mutex);

  /* fill the element: */
  element->tme_element_private = scsi_bus;
  element->tme_element_connections_new = _tme_scsi_bus_connections_new;

  return (TME_OK);
}
