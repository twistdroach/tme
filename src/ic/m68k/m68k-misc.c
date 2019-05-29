/* $Id: m68k-misc.c,v 1.23 2005/03/23 12:42:56 fredette Exp $ */

/* ic/m68k/m68k-misc.c - miscellaneous things for the m68k emulator: */

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

/* includes: */
#include "m68k-impl.h"

_TME_RCSID("$Id: m68k-misc.c,v 1.23 2005/03/23 12:42:56 fredette Exp $");

/* the memory buffer read and write functions: */
#if TME_M68K_SIZE_8 != 1
#error "TME_M68K_SIZE_8 must be 1"
#endif
#if TME_M68K_SIZE_16 != 2
#error "TME_M68K_SIZE_16 must be 2"
#endif
#if TME_M68K_SIZE_32 != 4
#error "TME_M68K_SIZE_32 must be 4"
#endif
const _tme_m68k_xfer_memx _tme_m68k_read_memx[5] = {
  NULL,
  tme_m68k_read_memx8,
  tme_m68k_read_memx16,
  NULL,
  tme_m68k_read_memx32
};
const _tme_m68k_xfer_memx _tme_m68k_write_memx[5] = {
  NULL,
  tme_m68k_write_memx8,
  tme_m68k_write_memx16,
  NULL,
  tme_m68k_write_memx32
};
const _tme_m68k_xfer_mem _tme_m68k_read_mem[5] = {
  NULL,
  tme_m68k_read_mem8,
  tme_m68k_read_mem16,
  NULL,
  tme_m68k_read_mem32
};
const _tme_m68k_xfer_mem _tme_m68k_write_mem[5] = {
  NULL,
  tme_m68k_write_mem8,
  tme_m68k_write_mem16,
  NULL,
  tme_m68k_write_mem32
};

/* our bus signal handler: */
static int
_tme_m68k_bus_signal(struct tme_bus_connection *conn_bus, unsigned int signal)
{
  struct tme_m68k *ic;
  unsigned int level_edge;

  /* recover our IC: */
  ic = conn_bus->tme_bus_connection.tme_connection_element->tme_element_private;

  /* take out the level and edge: */
  level_edge = signal;
  signal = TME_BUS_SIGNAL_WHICH(signal);
  level_edge ^= signal;

  /* lock the external mutex: */
  tme_mutex_lock(&ic->tme_m68k_external_mutex);

  /* on the falling edge of HALT or RESET, halt the processor: */
  if (((level_edge & TME_BUS_SIGNAL_LEVEL_MASK)
       == TME_BUS_SIGNAL_LEVEL_ASSERTED)
      && (signal == TME_BUS_SIGNAL_HALT
	  || signal == TME_BUS_SIGNAL_RESET)) {
    ic->tme_m68k_external_halt = TRUE;
  }

  /* on the rising edge of RESET, reset the processor: */
  else if (signal == TME_BUS_SIGNAL_RESET
	   && ((level_edge & TME_BUS_SIGNAL_LEVEL_MASK)
	       == TME_BUS_SIGNAL_LEVEL_NEGATED)) {
    ic->tme_m68k_external_reset = TRUE;
  }

  /* on any other HALT or RESET, do nothing: */
  else if (signal == TME_BUS_SIGNAL_RESET
	   || signal == TME_BUS_SIGNAL_HALT) {
    /* nothing */
  }

  /* anything else: */
  else {
    abort();
  }

  /* unlock the external mutex: */
  tme_mutex_unlock(&ic->tme_m68k_external_mutex);

  /* notify any threads waiting on the external condition: */
  tme_cond_notify(&ic->tme_m68k_external_cond, TRUE);
  return (TME_OK);
}

/* this enables or disables an m6888x: */
static int
_tme_m6888x_enable(struct tme_m68k_bus_connection *conn_m68k, int enabled)
{
  struct tme_m68k *ic;

  /* recover our IC: */
  ic = conn_m68k->tme_m68k_bus_connection.tme_bus_connection.tme_connection_element->tme_element_private;

  /* NB: we're lazy here and don't bother locking the external mutex: */
  if (ic->tme_m68k_fpu_type == TME_M68K_FPU_NONE) {
    return (ENXIO);
  }
  ic->tme_m68k_fpu_enabled = enabled;
  return (TME_OK);
}

/* our interrupt handler: */
static int
_tme_m68k_bus_interrupt(struct tme_m68k_bus_connection *conn_m68k, unsigned int ipl)
{
  struct tme_m68k *ic;

  /* recover our IC: */
  ic = conn_m68k->tme_m68k_bus_connection.tme_bus_connection.tme_connection_element->tme_element_private;

  /* lock the external mutex: */
  tme_mutex_lock(&ic->tme_m68k_external_mutex);

  /* set the interrupt line: */
  ic->tme_m68k_external_ipl = ipl;

  /* if the IPL has dropped below the NMI level, the next transition
     to that level will cause an NMI: */
  if (ipl < TME_M68K_IPL_NMI) {
    ic->tme_m68k_external_ipl_previous_nmi = FALSE;
  }

  /* unlock the external mutex: */
  tme_mutex_unlock(&ic->tme_m68k_external_mutex);

  /* notify any threads waiting on the external condition: */
  tme_cond_notify(&ic->tme_m68k_external_cond, TRUE);
  return (TME_OK);
}

/* this checks for external signals.  this must be called with the
   external mutex held: */
void
tme_m68k_external_check(struct tme_m68k *ic, tme_uint32_t internal_exceptions)
{
  unsigned int ipl;
  int vector;
  int rc;

  /* if an external reset has been requested, start reset exception
     processing: */
  if (ic->tme_m68k_external_reset) {
    ic->tme_m68k_external_reset = FALSE;
    tme_mutex_unlock(&ic->tme_m68k_external_mutex);
    tme_m68k_exception(ic, TME_M68K_EXCEPTION_RESET);
  }

  /* if an external halt has been requested, halt: */
  if (ic->tme_m68k_external_halt) {
    ic->tme_m68k_external_halt = FALSE;
    tme_mutex_unlock(&ic->tme_m68k_external_mutex);
    ic->_tme_m68k_mode = TME_M68K_MODE_HALT;
    TME_M68K_SEQUENCE_START;
    tme_m68k_redispatch(ic);
  }

  /* if we are not halted, and an interrupt can be serviced, start
     interrupt exception processing: */
  ipl = ic->tme_m68k_external_ipl;
  if (ic->_tme_m68k_mode != TME_M68K_MODE_HALT
      && ipl >= TME_M68K_IPL_MIN
      && ipl <= TME_M68K_IPL_MAX
      && ((ipl == TME_M68K_IPL_NMI
	   && !ic->tme_m68k_external_ipl_previous_nmi)
	  || ipl > TME_M68K_FLAG_IPM(ic->tme_m68k_ireg_sr))) {
    
    /* if this is an NMI, prevent it from being repeatedly accepted: */
    if (ipl == TME_M68K_IPL_NMI) {
      ic->tme_m68k_external_ipl_previous_nmi = TRUE;
    }

    tme_mutex_unlock(&ic->tme_m68k_external_mutex);
    
    /* acknowledge the interrupt and get the vector: */
    rc = (*ic->_tme_m68k_bus_connection->tme_m68k_bus_connection.tme_bus_intack)
      (&ic->_tme_m68k_bus_connection->tme_m68k_bus_connection,
       ipl, &vector);
    if (rc == TME_EDEADLK) {
      abort();
    }

    /* if the interrupt acknowledge failed, this is a spurious interrupt: */
    if (rc == ENOENT) {
      vector = TME_M68K_VECTOR_SPURIOUS;
    }

    /* if no vector is given, use the autovector: */
    else if (vector == TME_BUS_INTERRUPT_VECTOR_UNDEF) {
      vector = TME_M68K_VECTOR_SPURIOUS + ipl;
    }

    /* dispatch the exceptions: */
    tme_m68k_exception(ic, internal_exceptions | TME_M68K_EXCEPTION_INT(ipl, vector));
  }

  /* if there are internal exceptions to process, do so: */
  if (internal_exceptions != 0) {
    tme_mutex_unlock(&ic->tme_m68k_external_mutex);
    tme_m68k_exception(ic, internal_exceptions);
  }

  /* there are no exceptions to process: */
}

/* the idle function, used when the processor is halted or stopped: */
static void
tme_m68k_idle(struct tme_m68k *ic)
{  
  /* lock the external mutex: */
  tme_mutex_lock(&ic->tme_m68k_external_mutex);

  /* loop forever: */
  for (;;) {

    /* check for any external signal: */
    tme_m68k_external_check(ic, 0);

    /* await an external condition: */
    tme_cond_wait_yield(&ic->tme_m68k_external_cond, &ic->tme_m68k_external_mutex);
  }
}

/* the m68k thread: */
static void
tme_m68k_thread(struct tme_m68k *ic)
{

  /* we use longjmp to redispatch: */
  do { } while (setjmp(ic->_tme_m68k_dispatcher));

  /* dispatch on the current mode: */
  switch (ic->_tme_m68k_mode) {

  case TME_M68K_MODE_EXECUTION:
    (*ic->_tme_m68k_mode_execute)(ic);
    /* NOTREACHED */

  case TME_M68K_MODE_EXCEPTION:
    (*ic->_tme_m68k_mode_exception)(ic);
    /* NOTREACHED */

  case TME_M68K_MODE_RTE:
    (*ic->_tme_m68k_mode_rte)(ic);
    /* NOTREACHED */

  case TME_M68K_MODE_STOP:
  case TME_M68K_MODE_HALT:
    tme_m68k_idle(ic);
    /* NOTREACHED */

  default:
    abort();
  }
  /* NOTREACHED */
}

/* the TLB filler for when we are on a generic bus: */
static int
_tme_m68k_generic_tlb_fill(struct tme_m68k_bus_connection *conn_m68k, 
			   struct tme_m68k_tlb *tlb,
			   unsigned int function_code, 
			   tme_uint32_t external_address, 
			   unsigned int cycles)
{
  struct tme_m68k *ic;

  /* recover our IC: */
  ic = conn_m68k->tme_m68k_bus_connection.tme_bus_connection.tme_connection_element->tme_element_private;

  /* this m68k implementation never fills TLB entries on the stack, so
     a TLB entry reserves itself.  this also means that we don't have
     to call tme_bus_tlb_back() after the fill: */
  tme_bus_tlb_reserve(&tlb->tme_m68k_tlb_bus_tlb, &tlb->tme_m68k_tlb_bus_tlb);

  /* call the generic bus TLB filler: */
  (ic->_tme_m68k_bus_generic->tme_bus_tlb_fill)
    (ic->_tme_m68k_bus_generic,
     &tlb->tme_m68k_tlb_bus_tlb,
     external_address,
     cycles);
  
  /* when we're on a generic bus a TLB entry is valid for all function codes: */
  tlb->tme_m68k_tlb_function_codes_mask = -1;

  return (TME_OK);
}

/* the connection scorer: */
static int
_tme_m68k_connection_score(struct tme_connection *conn, unsigned int *_score)
{
  struct tme_m68k_bus_connection *conn_m68k;
  struct tme_bus_connection *conn_bus;
  unsigned int score;

  /* assume that this connection is useless: */
  score = 0;

  /* dispatch on the connection type: */
  conn_m68k = (struct tme_m68k_bus_connection *) conn->tme_connection_other;
  conn_bus = (struct tme_bus_connection *) conn->tme_connection_other;
  switch (conn->tme_connection_type) {

    /* this must be a bus, and not another m68k chip: */
  case TME_CONNECTION_BUS_M68K:
    if (conn_bus->tme_bus_tlb_set_allocate != NULL
	&& conn_m68k->tme_m68k_bus_tlb_fill != NULL
	&& conn_m68k->tme_m68k_bus_m6888x_enable == NULL) {
      score = 10;
    }
    break;

    /* this must be a bus, and not another chip: */
  case TME_CONNECTION_BUS_GENERIC:
    if (conn_bus->tme_bus_tlb_set_allocate != NULL
	&& conn_bus->tme_bus_tlb_fill != NULL) {
      score = 1;
    }
    break;

  default: abort();
  }

  *_score = score;
  return (TME_OK);
}

/* this makes a new connection: */
static int
_tme_m68k_connection_make(struct tme_connection *conn, unsigned int state)
{
  struct tme_m68k *ic;
  struct tme_m68k_bus_connection *conn_m68k;
  struct tme_bus_connection *conn_bus;
  struct tme_connection *conn_other;

  /* since the CPU is halted, it won't be making any connection calls,
     so we only have to do work when the connection is fully made: */
  if (state == TME_CONNECTION_FULL) {

    /* recover our IC: */
    ic = conn->tme_connection_element->tme_element_private;
    
    /* dispatch on the connection type: */
    conn_other = conn->tme_connection_other;
    conn_m68k = (struct tme_m68k_bus_connection *) conn_other;
    conn_bus = (struct tme_bus_connection *) conn_other;
    switch (conn->tme_connection_type) {
      
    case TME_CONNECTION_BUS_M68K:
      ic->_tme_m68k_bus_connection = conn_m68k;
      break;
      
      /* we need an adaptation layer: */
    case TME_CONNECTION_BUS_GENERIC:
      conn_m68k = tme_new0(struct tme_m68k_bus_connection, 1);
      conn_m68k->tme_m68k_bus_connection.tme_bus_connection.tme_connection_element = conn->tme_connection_element;
      conn_m68k->tme_m68k_bus_tlb_fill = _tme_m68k_generic_tlb_fill;
      ic->_tme_m68k_bus_connection = conn_m68k;
      ic->_tme_m68k_bus_generic = conn_bus;
      break;
      
    default: abort();
    }

    /* allocate the TLB hash set: */
    (*ic->_tme_m68k_bus_connection->tme_m68k_bus_connection.tme_bus_tlb_set_allocate)
      (&ic->_tme_m68k_bus_connection->tme_m68k_bus_connection, 
       _TME_M68K_TLB_HASH_SIZE, 
       sizeof(struct tme_m68k_tlb),
       TME_ATOMIC_POINTER((struct tme_bus_tlb **) &ic->_tme_m68k_tlb_array));

    /* allocate the ITLB set: */
    (*ic->_tme_m68k_bus_connection->tme_m68k_bus_connection.tme_bus_tlb_set_allocate)
      (&ic->_tme_m68k_bus_connection->tme_m68k_bus_connection, 
       1, 
       sizeof(struct tme_m68k_tlb),
       TME_ATOMIC_POINTER((struct tme_bus_tlb **) &ic->_tme_m68k_itlb));
  }

  /* NB: the machine needs to issue a reset to bring the CPU out of halt. */
  return (TME_OK);
}

/* this breaks a connection: */
static int 
_tme_m68k_connection_break(struct tme_connection *conn, unsigned int state)
{
  abort();
  return (0);
}

/* this makes new connection sides: */
static int
_tme_m68k_connections_new(struct tme_element *element, const char * const *args, struct tme_connection **_conns, char **_output)
{
  struct tme_m68k_bus_connection *conn_m68k;
  struct tme_bus_connection *conn_bus;
  struct tme_connection *conn;

  /* if we already have a bus connection, we can take no more connections: */
  if (((struct tme_m68k *) element->tme_element_private)->_tme_m68k_bus_connection != NULL) {
    return (TME_OK);
  }

  /* create our side of an m68k bus connection: */
  conn_m68k = tme_new0(struct tme_m68k_bus_connection, 1);
  conn_bus = &conn_m68k->tme_m68k_bus_connection;
  conn = &conn_bus->tme_bus_connection;

  /* fill in the generic connection: */
  conn->tme_connection_next = *_conns;
  conn->tme_connection_type = TME_CONNECTION_BUS_M68K;
  conn->tme_connection_score = _tme_m68k_connection_score;
  conn->tme_connection_make = _tme_m68k_connection_make;
  conn->tme_connection_break = _tme_m68k_connection_break;

  /* fill in the generic bus connection: */
  conn_bus->tme_bus_signal = _tme_m68k_bus_signal;
  conn_bus->tme_bus_tlb_set_allocate = NULL;

  /* full in the m68k bus connection: */
  conn_m68k->tme_m68k_bus_interrupt = _tme_m68k_bus_interrupt;
  conn_m68k->tme_m68k_bus_tlb_fill = NULL;
  conn_m68k->tme_m68k_bus_m6888x_enable = _tme_m6888x_enable;

  /* add this connection to the set of possibilities: */
  *_conns = conn;

  /* create our side of a generic bus connection: */
  conn_bus = tme_new0(struct tme_bus_connection, 1);
  conn = &conn_bus->tme_bus_connection;

  /* fill in the generic connection: */
  conn->tme_connection_next = *_conns;
  conn->tme_connection_type = TME_CONNECTION_BUS_GENERIC;
  conn->tme_connection_score = _tme_m68k_connection_score;
  conn->tme_connection_make = _tme_m68k_connection_make;
  conn->tme_connection_break = _tme_m68k_connection_break;

  /* fill in the generic bus connection: */
  conn_bus->tme_bus_signal = _tme_m68k_bus_signal;
  conn_bus->tme_bus_tlb_set_allocate = NULL;
  conn_bus->tme_bus_tlb_fill = NULL;

  /* add this connection to the set of possibilities: */
  *_conns = conn;

  /* done: */
  return (TME_OK);
}

/* the common m68k new function: */
int
tme_m68k_new(struct tme_m68k *ic, const char * const *args, const void *extra, char **_output)
{
  struct tme_element *element;
  int arg_i;
  int usage;

  /* check our arguments: */
  arg_i = 1;
  usage = FALSE;
  for (;;) {
    
    if (0) {

    }

    /* if we've run out of arguments: */
    else if (args[arg_i + 0] == NULL) {
      break;
    }

    /* this is either a bad argument or an FPU argument: */
    else {

      /* if this is not an FPU argument: */
      if (!tme_m68k_fpu_new(ic, args, &arg_i, &usage, _output)) {
	tme_output_append_error(_output,
				"%s %s, ",
				args[arg_i],
				_("unexpected"));
	usage = TRUE;
      }
      
      if (usage) {
	break;
      }
    }
  }

  if (usage) {
    tme_output_append_error(_output, 
			    "%s %s",
			    _("usage:"),
			    args[0]);
    tme_m68k_fpu_usage(_output);
    tme_free(ic);
    return (EINVAL);
  }

  /* initialize the verifier: */
  tme_m68k_verify_init();

  /* dispatch on the type: */
  switch (ic->tme_m68k_type) {
  case TME_M68K_M68000:
    ic->_tme_m68k_bus_16bit = TRUE;
    break;
  case TME_M68K_M68010:
    ic->_tme_m68k_bus_16bit = TRUE;
    break;
  case TME_M68K_M68020:
    ic->_tme_m68k_bus_16bit = FALSE;
    break;
  default:
    abort();
  }

  /* we have no bus connection yet: */
  ic->_tme_m68k_bus_connection = NULL;

  /* fill the element: */
  element = ic->tme_m68k_element;
  element->tme_element_private = ic;
  element->tme_element_connections_new = _tme_m68k_connections_new;

  /* calculate the instruction burst size: */
  /* XXX TBD: */
  ic->_tme_m68k_instruction_burst = 200;
  ic->_tme_m68k_instruction_burst_remaining
    = ic->_tme_m68k_instruction_burst;

  /* set the status register T bits mask: */
  ic->_tme_m68k_sr_mask_t
    = (TME_M68K_FLAG_T1
       | ((ic->tme_m68k_type >= TME_M68K_M68020)
	  * TME_M68K_FLAG_T0));

  /* initialize the small immediates: */
  ic->tme_m68k_ireg_uint32(TME_M68K_IREG_ZERO) = 0;
  ic->tme_m68k_ireg_uint32(TME_M68K_IREG_ONE) = 1;
  ic->tme_m68k_ireg_uint32(TME_M68K_IREG_TWO) = 2;
  ic->tme_m68k_ireg_uint32(TME_M68K_IREG_THREE) = 3;
  ic->tme_m68k_ireg_uint32(TME_M68K_IREG_FOUR) = 4;
  ic->tme_m68k_ireg_uint32(TME_M68K_IREG_FIVE) = 5;
  ic->tme_m68k_ireg_uint32(TME_M68K_IREG_SIX) = 6;
  ic->tme_m68k_ireg_uint32(TME_M68K_IREG_SEVEN) = 7;
  ic->tme_m68k_ireg_uint32(TME_M68K_IREG_EIGHT) = 8;

  /* force the processor to be halted: */
  ic->_tme_m68k_mode = TME_M68K_MODE_HALT;
  TME_M68K_SEQUENCE_START;

  /* start the m68k thread: */
  tme_thread_create((tme_thread_t) tme_m68k_thread, ic);

  return (TME_OK);
}  

/* the common m68k reset function: */
void
tme_m68k_do_reset(struct tme_m68k *ic)
{
  
  /* force the VBR to zero: */
  ic->tme_m68k_ireg_vbr = 0;

  /* clear the E and F bits in the CACR: */
  ic->tme_m68k_ireg_cacr = 0;

  /* force supervisor mode, interrupts disabled: */
  tme_m68k_change_sr(ic, TME_M68K_FLAG_S | (7 << 8));

  /* load the initial SSP and PC: */
  ic->_tme_m68k_ea_function_code = TME_M68K_FC_SP;
  ic->_tme_m68k_ea_address = 0;
  tme_m68k_read_mem32(ic, TME_M68K_IREG_A7);
  ic->_tme_m68k_ea_address += sizeof(ic->tme_m68k_ireg_a7);
  tme_m68k_read_mem32(ic, TME_M68K_IREG_PC);

  /* clear all exceptions: */
  ic->_tme_m68k_exceptions = 0;

  /* reset the FPU: */
  tme_m68k_fpu_reset(ic);

  /* start execution: */
  ic->_tme_m68k_mode = TME_M68K_MODE_EXECUTION;
  TME_M68K_SEQUENCE_START;
  tme_m68k_redispatch(ic);
}

/* this returns nonzero iff the slow instruction executor must be
   used: */
int
tme_m68k_go_slow(const struct tme_m68k *ic)
{
  struct tme_m68k_tlb *tlb;
  tme_uint32_t linear_pc;

  tlb = TME_ATOMIC_READ(struct tme_m68k_tlb *, ic->_tme_m68k_itlb);
  linear_pc = ic->tme_m68k_ireg_pc;
  return (
	  
	  /* the ITLB entry must support reads from emulator memory: */
	  !TME_M68K_TLB_OK_FAST_READ(tlb, 
				     TME_M68K_FUNCTION_CODE_PROGRAM(ic),
				     linear_pc,
				     linear_pc)

	  /* the ITLB emulator memory must be 32-bit aligned for the
	     benefit of the fast instruction word fetch macros, so
	     that emulator address alignment goes with linear address
	     alignment: */
	  || (((unsigned long) tlb->tme_m68k_tlb_emulator_off_read)
	      & (sizeof(tme_uint32_t) - 1))

	  /* the linear PC must be 16-bit aligned: */
	  || (linear_pc & 1)

	  /* there must be no tracing: */
	  || (ic->tme_m68k_ireg_sr & ic->_tme_m68k_sr_mask_t) != 0);
}

/* this redispatches: */
void
tme_m68k_redispatch(struct tme_m68k *ic)
{
#ifdef _TME_M68K_STATS
  ic->tme_m68k_stats.tme_m68k_stats_redispatches++;
#endif /* _TME_M68K_STATS */
  longjmp(ic->_tme_m68k_dispatcher, 1);
}

/* this fills a TLB entry: */
void
tme_m68k_tlb_fill(struct tme_m68k *ic, struct tme_m68k_tlb *tlb, 
		  unsigned int function_code, 
		  tme_uint32_t linear_address, 
		  unsigned int cycles)
{
  tme_uint32_t external_address;
  struct tme_bus_tlb tlb_internal;
  
#ifdef _TME_M68K_STATS
  if (function_code == TME_M68K_FC_UP
      || function_code == TME_M68K_FC_SP) {
    ic->tme_m68k_stats.tme_m68k_stats_itlb_fill++;
  }
  else {
    ic->tme_m68k_stats.tme_m68k_stats_dtlb_fill++;
  }
#endif /* _TME_M68K_STATS */

  /* when emulating a CPU with a 16-bit bus, only 24 bits of address
     are external: */
  external_address = linear_address;
  if (ic->_tme_m68k_bus_16bit) {
    external_address &= 0x00ffffff;
  }

  /* this m68k implementation never fills TLB entries on the stack, so
     a TLB entry reserves itself.  this also means that we don't have
     to call tme_bus_tlb_back() after the fill: */
  tme_bus_tlb_reserve(&tlb->tme_m68k_tlb_bus_tlb, &tlb->tme_m68k_tlb_bus_tlb);

  /* fill the TLB entry: */
  (*ic->_tme_m68k_bus_connection->tme_m68k_bus_tlb_fill)
    (ic->_tme_m68k_bus_connection, tlb,
     function_code,
     external_address,
     cycles);

  /* if this code isn't 32-bit clean, we have to deal: */
  if (external_address != linear_address) {
    TME_ATOMIC_WRITE(tme_bus_addr_t, tlb_internal.tme_bus_tlb_addr_first,
		     TME_ATOMIC_READ(tme_bus_addr_t, tlb->tme_m68k_tlb_linear_first)
		     | (linear_address ^ external_address));
    TME_ATOMIC_WRITE(tme_bus_addr_t, tlb_internal.tme_bus_tlb_addr_last,
		     TME_ATOMIC_READ(tme_bus_addr_t, tlb->tme_m68k_tlb_linear_last)
		     | (linear_address ^ external_address));
    tlb_internal.tme_bus_tlb_cycles_ok = tlb->tme_m68k_tlb_bus_tlb.tme_bus_tlb_cycles_ok;
    tme_bus_tlb_map(&tlb->tme_m68k_tlb_bus_tlb, external_address,
		    &tlb_internal, linear_address);
  }
}

/* this triggers exception processing: */
void
tme_m68k_exception(struct tme_m68k *ic, tme_uint32_t new_exceptions)
{
  assert(new_exceptions != 0);

  /* if the set of new exceptions includes a group zero exception: */
  if (new_exceptions & 
      (TME_M68K_EXCEPTION_RESET
       | TME_M68K_EXCEPTION_AERR
       | TME_M68K_EXCEPTION_BERR)) {
    
    /* there must be only one exception - you cannot trigger a group 0
       exception simultaneously with any other group 0, 1, or 2
       exception: */
    assert((new_exceptions & (new_exceptions - 1)) == 0);
    
    /* if this is a reset exception, it clears all other exceptions: */
    if (new_exceptions == TME_M68K_EXCEPTION_RESET) {
      ic->_tme_m68k_exceptions = 0;
    }

    /* otherwise, this is an address error or a bus error.  if we were
       already processing a group 0 exception, this is a
       double fault, and the processor enters the halted state: */
    else if (ic->_tme_m68k_exceptions &
	     (TME_M68K_EXCEPTION_RESET
	      | TME_M68K_EXCEPTION_AERR
	      | TME_M68K_EXCEPTION_BERR)) {
      tme_log(TME_M68K_LOG_HANDLE(ic), 0, TME_OK,
	      (TME_M68K_LOG_HANDLE(ic),
	       _("double fault, processor halted")));
      ic->_tme_m68k_mode = TME_M68K_MODE_HALT;
      TME_M68K_SEQUENCE_START;
      tme_m68k_redispatch(ic);
    }
  }

  /* otherwise, exception processing must not already be happening: */
  else {
    assert(ic->_tme_m68k_exceptions == 0);
  }

  /* begin exception processing: */
  ic->_tme_m68k_exceptions |= new_exceptions;
  ic->_tme_m68k_mode = TME_M68K_MODE_EXCEPTION;
  TME_M68K_SEQUENCE_START;
  tme_m68k_redispatch(ic);
}

/* this changes SR, and swaps %a7 as needed: */
void
tme_m68k_change_sr(struct tme_m68k *ic, tme_uint16_t sr)
{
  tme_uint16_t flags_mode;

  /* only recognize the M bit on a 68020 or better: */
  flags_mode = (TME_M68K_FLAG_S
		| ((ic->tme_m68k_type >= TME_M68K_M68020)
		   * TME_M68K_FLAG_M));
  
  /* save %a7 in the proper stack pointer control register: */
  switch (ic->tme_m68k_ireg_sr & flags_mode) {
  case 0:
  case TME_M68K_FLAG_M:
    ic->tme_m68k_ireg_usp = ic->tme_m68k_ireg_a7;
    break;
  case TME_M68K_FLAG_S:
    ic->tme_m68k_ireg_isp = ic->tme_m68k_ireg_a7;
    break;
  case (TME_M68K_FLAG_S | TME_M68K_FLAG_M):
    ic->tme_m68k_ireg_msp = ic->tme_m68k_ireg_a7;
    break;
  }

  /* load %a7 from the proper stack pointer control register: */
  ic->tme_m68k_ireg_sr = sr;
  switch (ic->tme_m68k_ireg_sr & flags_mode) {
  case 0:
  case TME_M68K_FLAG_M:
    ic->tme_m68k_ireg_a7 = ic->tme_m68k_ireg_usp;
    break;
  case TME_M68K_FLAG_S:
    ic->tme_m68k_ireg_a7 = ic->tme_m68k_ireg_isp;
    break;
  case (TME_M68K_FLAG_S | TME_M68K_FLAG_M):
    ic->tme_m68k_ireg_a7 = ic->tme_m68k_ireg_msp;
    break;
  }
}

/* this starts processing an m68k exception: */
void
tme_m68k_exception_process_start(struct tme_m68k *ic, unsigned int ipl)
{
  tme_uint16_t sr;

  /* make an internal copy of the status register, then set S, clear
     T, and update I: */
  if (!TME_M68K_SEQUENCE_RESTARTING) {
    ic->tme_m68k_ireg_shadow_sr = ic->tme_m68k_ireg_sr;
    sr = (ic->tme_m68k_ireg_sr | TME_M68K_FLAG_S) & ~ic->_tme_m68k_sr_mask_t;
    if (ipl > TME_M68K_IPL_NONE) {
      assert(ipl == TME_M68K_IPL_NMI
	     || ipl > TME_M68K_FLAG_IPM(sr));
      sr = (sr & ~(TME_M68K_IPL_MAX << 8)) | (ipl << 8);
    }
    tme_m68k_change_sr(ic, sr);
  }
}

/* this finishes processing an m68k exception: */
void
tme_m68k_exception_process_finish(struct tme_m68k *ic, tme_uint8_t format, tme_uint8_t vector)
{
  tme_uint16_t vector_offset;

  /* stack the frame format and vector offset, unless this is a 68000: */
  vector_offset = ((tme_uint16_t) vector) << 2;
  if (ic->tme_m68k_type != TME_M68K_M68000) {
    tme_m68k_push16(ic, (((tme_uint16_t) format) << 12) | vector_offset);
  }

  /* stack the program counter: */
  tme_m68k_push32(ic, ic->tme_m68k_ireg_pc);
  
  /* stack the internal copy of the status register: */
  tme_m68k_push16(ic, ic->tme_m68k_ireg_shadow_sr);

  /* do a bus cycle to read the vector into the program counter: */
  if (!TME_M68K_SEQUENCE_RESTARTING) {
    ic->_tme_m68k_ea_function_code = TME_M68K_FC_SD;
    ic->_tme_m68k_ea_address = ic->tme_m68k_ireg_vbr + vector_offset;
  }
  tme_m68k_read_mem32(ic, TME_M68K_IREG_PC);
}

/* common m68000 and m68010 exception processing: */
void
tme_m68000_exception_process(struct tme_m68k *ic)
{
  tme_uint32_t exceptions;
  tme_uint8_t vector;

  /* get the set of exceptions.  we must have no group 0 exceptions: */
  exceptions = ic->_tme_m68k_exceptions;
  assert((exceptions & (TME_M68K_EXCEPTION_RESET
			| TME_M68K_EXCEPTION_AERR
			| TME_M68K_EXCEPTION_BERR)) == 0);

  /* these if statements are ordered to implement the priority
     relationship between the different exceptions as outlined in 
     the 68000 user's manual (pp 93 in my copy): */
  
  if (TME_M68K_EXCEPTION_IS_INST(exceptions)) {
    tme_m68k_exception_process_start(ic, 0);
    tme_m68k_exception_process_finish(ic, TME_M68K_FORMAT_0, TME_M68K_EXCEPTION_IS_INST(exceptions));
  }
  
  if (exceptions & TME_M68K_EXCEPTION_TRACE) {
    tme_m68k_exception_process_start(ic, 0);
    tme_m68k_exception_process_finish(ic, TME_M68K_FORMAT_0, TME_M68K_VECTOR_TRACE);
  }
  
  if (TME_M68K_EXCEPTION_IS_INT(exceptions)) {
    tme_m68k_exception_process_start(ic, TME_M68K_EXCEPTION_IS_INT(exceptions));
    tme_m68k_exception_process_finish(ic, TME_M68K_FORMAT_0, TME_M68K_EXCEPTION_INT_VEC(exceptions));
  }
  
  if (exceptions & TME_M68K_EXCEPTION_ILL) {
    if (TME_FIELD_EXTRACTU(ic->_tme_m68k_insn_opcode, 12, 4) == 0xa) {
      vector = TME_M68K_VECTOR_LINE_A;
    }
    else if (TME_FIELD_EXTRACTU(ic->_tme_m68k_insn_opcode, 12, 4) == 0xf) {
      vector = TME_M68K_VECTOR_LINE_F;
    }
    else {
      vector = TME_M68K_VECTOR_ILL;
    }
    tme_m68k_exception_process_start(ic, 0);
    tme_m68k_exception_process_finish(ic, TME_M68K_FORMAT_0, vector);
  }
  
  if (exceptions & TME_M68K_EXCEPTION_PRIV) {
    tme_m68k_exception_process_start(ic, 0);
    tme_m68k_exception_process_finish(ic, TME_M68K_FORMAT_0, TME_M68K_VECTOR_TRACE);
  }
  
  /* we have processed all exceptions - resume execution: */
  ic->_tme_m68k_exceptions = 0;
  ic->_tme_m68k_mode = TME_M68K_MODE_EXECUTION;
  TME_M68K_SEQUENCE_START;
  tme_m68k_redispatch(ic);
}

/* common m68020 and later exception processing: */
void
tme_m68020_exception_process(struct tme_m68k *ic)
{
  tme_uint32_t exceptions;
  tme_uint8_t vector;
  struct {
    tme_uint16_t tme_m68k_fmt1_sr;
    tme_uint16_t tme_m68k_fmt1_pc_hi;
    tme_uint16_t tme_m68k_fmt1_pc_lo;    
    tme_uint16_t tme_m68k_fmt1_vector_offset;
  } fmt1;

  /* get the set of exceptions.  we must have no group 0 or 1
     exceptions: */
  exceptions = ic->_tme_m68k_exceptions;
  assert((exceptions & (TME_M68K_EXCEPTION_RESET
			| TME_M68K_EXCEPTION_AERR
			| TME_M68K_EXCEPTION_BERR)) == 0);

  /* these if statements are ordered to implement the priority
     relationship between the different exceptions as outlined in 
     the 68020 user's manual (pp 144 in my copy): */
  
  /* group 2 exceptions: */
  if (TME_M68K_EXCEPTION_IS_INST(exceptions)) {
    tme_m68k_exception_process_start(ic, 0);

    /* get the vector number: */
    vector = TME_M68K_EXCEPTION_IS_INST(exceptions);

    /* of the group 2 exceptions, only the Format Error and TRAP #N
       exceptions generate a format 0 stack frame.  the RTE mode code
       and the TRAP instruction code are expected to have left
       ic->tme_m68k_ireg_pc as the PC they want stacked: */
    if (vector == TME_M68K_VECTOR_FORMAT
	|| (TME_M68K_VECTOR_TRAP_0 <= vector
	    && vector < (TME_M68K_VECTOR_TRAP_0 + 16))) {
      tme_m68k_exception_process_finish(ic, TME_M68K_FORMAT_0, vector);
    }

    /* all other group 2 exceptions generate a format 2 stack frame.
       all code that can signal this exception is expected to have
       left ic->tme_m68k_ireg_pc *and* ic->tme_m68k_ireg_pc_last as
       the PCs they want stacked: */
    else {
      
      /* stack the program counter of the instruction that caused the exception: */
      tme_m68k_push32(ic, ic->tme_m68k_ireg_pc_last);

      /* finish with a format 2 stack frame: */
      tme_m68k_exception_process_finish(ic, TME_M68K_FORMAT_2, vector);
    }
  }

  /* group 3 exceptions: */
  if (exceptions & TME_M68K_EXCEPTION_ILL) {
    if (TME_FIELD_EXTRACTU(ic->_tme_m68k_insn_opcode, 12, 4) == 0xa) {
      vector = TME_M68K_VECTOR_LINE_A;
    }
    else if (TME_FIELD_EXTRACTU(ic->_tme_m68k_insn_opcode, 12, 4) == 0xf) {
      vector = TME_M68K_VECTOR_LINE_F;
    }
    else {
      vector = TME_M68K_VECTOR_ILL;
    }
    tme_m68k_exception_process_start(ic, 0);
    tme_m68k_exception_process_finish(ic, TME_M68K_FORMAT_0, vector);
  }
  if (exceptions & TME_M68K_EXCEPTION_PRIV) {
    tme_m68k_exception_process_start(ic, 0);
    tme_m68k_exception_process_finish(ic, TME_M68K_FORMAT_0, TME_M68K_VECTOR_PRIV);
  }

  /* group 4.1 exceptions: */
  if (exceptions & TME_M68K_EXCEPTION_TRACE) {
    tme_m68k_exception_process_start(ic, 0);
    tme_m68k_push32(ic, ic->tme_m68k_ireg_pc_last);
    tme_m68k_exception_process_finish(ic, TME_M68K_FORMAT_2, TME_M68K_VECTOR_TRACE);
  }
  
  /* group 4.2 exceptions: */
  if (TME_M68K_EXCEPTION_IS_INT(exceptions)) {
    tme_m68k_exception_process_start(ic, TME_M68K_EXCEPTION_IS_INT(exceptions));
    tme_m68k_exception_process_finish(ic, TME_M68K_FORMAT_0, TME_M68K_EXCEPTION_INT_VEC(exceptions));

    /* if the M-bit is set: */
    if (ic->tme_m68k_ireg_sr & TME_M68K_FLAG_M) {

      /* make the throwaway four-word stack frame (format 1): */
      fmt1.tme_m68k_fmt1_vector_offset = tme_htobe_u16((TME_M68K_FORMAT_1 << 12) | (TME_M68K_EXCEPTION_INT_VEC(exceptions) << 2));
      fmt1.tme_m68k_fmt1_pc_lo = tme_htobe_u16((ic->tme_m68k_ireg_pc >>  0) & 0xffff);
      fmt1.tme_m68k_fmt1_pc_hi = tme_htobe_u16((ic->tme_m68k_ireg_pc >> 16) & 0xffff);
      fmt1.tme_m68k_fmt1_sr = tme_htobe_u16(ic->tme_m68k_ireg_sr);

      /* store the throwaway four-word stack frame on the interrupt stack: */
      if (!TME_M68K_SEQUENCE_RESTARTING) {
	ic->_tme_m68k_ea_function_code = TME_M68K_FC_SD;
	ic->_tme_m68k_ea_address = ic->tme_m68k_ireg_isp - sizeof(fmt1);
      }
      tme_m68k_write_mem(ic, (tme_uint8_t *) &fmt1, sizeof(fmt1));

      /* move to the interrupt stack: */
      ic->tme_m68k_ireg_isp -= sizeof(fmt1);
      tme_m68k_change_sr(ic, ic->tme_m68k_ireg_sr & ~TME_M68K_FLAG_M);
    }
  }
  
  /* we have processed all exceptions - resume execution: */
  ic->_tme_m68k_exceptions = 0;
  ic->_tme_m68k_mode = TME_M68K_MODE_EXECUTION;
  TME_M68K_SEQUENCE_START;
  tme_m68k_redispatch(ic);
}

/* this starts an m68k RTE: */
tme_uint16_t
tme_m68k_rte_start(struct tme_m68k *ic)
{

  /* set up to read from the stack frame: */
  if (!TME_M68K_SEQUENCE_RESTARTING) {
    ic->_tme_m68k_ea_function_code = TME_M68K_FC_SD;
    ic->_tme_m68k_ea_address = ic->tme_m68k_ireg_a7;
  }

  /* read the stacked status register: */
  tme_m68k_read_mem16(ic, TME_M68K_IREG_SHADOW_SR);
  if (!TME_M68K_SEQUENCE_RESTARTING) {
    ic->_tme_m68k_ea_address += sizeof(ic->tme_m68k_ireg_shadow_sr);
  }

  /* read the stacked PC: */
  tme_m68k_read_mem32(ic, TME_M68K_IREG_PC_NEXT);
  if (!TME_M68K_SEQUENCE_RESTARTING) {
    ic->_tme_m68k_ea_address += sizeof(ic->tme_m68k_ireg_pc_next);
  }

  /* read the stacked format/offset word, unless this is a 68000: */
  if (ic->tme_m68k_type != TME_M68K_M68000) {
    tme_m68k_read_mem16(ic, TME_M68K_IREG_FORMAT_OFFSET);
    if (!TME_M68K_SEQUENCE_RESTARTING) {
      ic->_tme_m68k_ea_address += sizeof(ic->tme_m68k_ireg_format_offset);
    }
  }
  else {
    ic->tme_m68k_ireg_format_offset = 0;
  }

  /* return the frame format: */
  return (ic->tme_m68k_ireg_format_offset >> 12);
}

/* this finishes an m68k RTE: */
void
tme_m68k_rte_finish(struct tme_m68k *ic, tme_uint32_t format_extra)
{
  tme_uint32_t frame_size;

  /* calculate the total frame size.  the 68000 doesn't have a
     format/status word: */
  frame_size = (sizeof(ic->tme_m68k_ireg_shadow_sr)
		+ sizeof(ic->tme_m68k_ireg_pc_next)
		+ (ic->tme_m68k_type != TME_M68K_M68000
		   ? sizeof(ic->tme_m68k_ireg_format_offset)
		   : 0)
		+ format_extra);
  assert((frame_size & 1) == 0);

  /* adjust the stack: */
  ic->tme_m68k_ireg_a7 += frame_size;
  
  /* set the status register: */
  tme_m68k_change_sr(ic, ic->tme_m68k_ireg_shadow_sr);
		     
  /* set the PC: */
  ic->tme_m68k_ireg_pc = ic->tme_m68k_ireg_pc_next;
  
  /* redispatch: */
  tme_m68k_redispatch(ic);
}

/* this stores the group 0 sequence into a region of host memory.
   this is used when preparing the state information to be stored
   on the stack for a bus or address error: */
int
tme_m68k_sequence_empty(const struct tme_m68k *ic, tme_uint8_t *raw, unsigned int raw_avail)
{
  const struct _tme_m68k_sequence *sequence;
  unsigned int raw_used;
  
  /* get the group 0 sequence: */
  sequence = &ic->_tme_m68k_group0_sequence;
  raw_used = 0;

  /* we use 8 bits for the mode (2 bits) and flags (6 bits): */
  raw_used += sizeof(tme_uint8_t);
  assert(raw_avail >= raw_used);
  assert(sequence->_tme_m68k_sequence_mode < TME_BIT(2));
  assert(sequence->_tme_m68k_sequence_mode_flags < TME_BIT(6));
  *(raw++) = ((sequence->_tme_m68k_sequence_mode << 6)
	      | sequence->_tme_m68k_sequence_mode_flags);
  

  /* we use 16 bits for the faulted memory transfer ordinal
     (12 bits) and already-transferred byte count (4 bits): */
  raw_used += sizeof(tme_uint16_t);
  assert(raw_avail >= raw_used);
  assert(sequence->_tme_m68k_sequence_transfer_faulted < TME_BIT(12));
  assert(sequence->_tme_m68k_sequence_transfer_faulted_after < TME_BIT(4));
  *(raw++) = sequence->_tme_m68k_sequence_transfer_faulted >> 4;
  *(raw++) = ((sequence->_tme_m68k_sequence_transfer_faulted << 4)
	      | sequence->_tme_m68k_sequence_transfer_faulted_after);

#ifdef _TME_M68K_VERIFY
  /* we use sizeof(_tme_m68k_sequence_uid) bytes for the sequence UID: */
  raw_used += sizeof(sequence->_tme_m68k_sequence_uid);
  assert(raw_avail >= raw_used);
  memcpy(raw,
	 &sequence->_tme_m68k_sequence_uid,
	 sizeof(sequence->_tme_m68k_sequence_uid));
  raw += sizeof(sequence->_tme_m68k_sequence_uid);
#endif /* _TME_M68K_VERIFY */

  /* done: */
  return (raw_used);
}

/* this restores the group 0 sequence from a region of host memory.
   this is used when reading the state information stored on the
   stack for a bus or address error: */
int
tme_m68k_sequence_fill(struct tme_m68k *ic, const tme_uint8_t *raw, unsigned int raw_avail)
{
  struct _tme_m68k_sequence *sequence;
  unsigned int raw_used;
  
  /* get the group 0 sequence: */
  sequence = &ic->_tme_m68k_group0_sequence;
  raw_used = 0;

  /* we used 8 bits for the mode (2 bits) and flags (6 bits): */
  raw_used += sizeof(tme_uint8_t);
  if (raw_avail < raw_used) {
    return (-1);
  }
  sequence->_tme_m68k_sequence_mode = *raw >> 6;
  sequence->_tme_m68k_sequence_mode_flags = (*(raw++) & (TME_BIT(6) - 1));

  /* we used 16 bits for the faulted memory transfer ordinal
     (12 bits) and already-transferred byte count (4 bits): */
  raw_used += sizeof(tme_uint16_t);
  if (raw_avail < raw_used) {
    return (-1);
  }
  sequence->_tme_m68k_sequence_transfer_faulted = 
    (((tme_uint16_t) raw[0]) << 4)
    | (raw[1] >> 4);
  sequence->_tme_m68k_sequence_transfer_faulted_after = raw[1] & (TME_BIT(4) - 1);
  raw += sizeof(tme_uint16_t);

#ifdef _TME_M68K_VERIFY
  /* we used sizeof(_tme_m68k_sequence_uid) bytes for the sequence UID: */
  raw_used += sizeof(sequence->_tme_m68k_sequence_uid);
  if (raw_avail < raw_used) {
    return (-1);
  }
  memcpy(&sequence->_tme_m68k_sequence_uid,
	 raw,
	 sizeof(sequence->_tme_m68k_sequence_uid));
  raw += sizeof(sequence->_tme_m68k_sequence_uid);
#endif /* _TME_M68K_VERIFY */

  /* initialize this to one: */
  sequence->_tme_m68k_sequence_transfer_next = 1;

  /* done: */
  return (raw_used);
}

/* this transfers the instruction buffer to or from a region of host
   memory.  unlike the raw region in host memory, where the 16- and
   32-bit parts of the instruction are contiguous and therefore
   potentially misaligned, in the instruction buffer these instruction
   parts are all properly aligned.  given the instruction buffer, a
   contiguous host memory buffer, a count of instruction bytes and the
   sizes of the instruction fetches, this transfers from one buffer to
   the other.

   this is used to fill the instruction buffer when we're restoring
   our state from an exception stack or when we fault anywhere inside
   the fast executor, and it's used to empty the instruction buffer
   into an exception stack when we fault: */
int
tme_m68k_insn_buffer_xfer(struct tme_m68k *ic, tme_uint8_t *raw, unsigned int raw_avail, int what)
{
  int fill, sanity_assert;
  tme_uint16_t fetch_total;
  tme_uint16_t fetch_sizes;
  unsigned int fetch_sizes_bits;
  unsigned int insn_buffer_off, fetch_off, fetch_size, resid;
#define _FETCH_SIZES_BITS (8 * sizeof(ic->_tme_m68k_insn_buffer_fetch_sizes))
#define _FETCH_SIZE_BIT (1 << (_FETCH_SIZES_BITS - 1))
#define _FETCH_SANITY(e) \
do { \
  if (sanity_assert) \
    assert(e); \
  else if (!(e)) \
    return (-1); \
} while (/* CONSTCOND */ 0)
  
  /* if what is zero, we faulted somewhere inside the fast executor
     and we need to fill the instruction buffer from raw host memory: */
  if (what == 0) {
    fill = TRUE;
    sanity_assert = TRUE;

    /* the fetch total and sizes are in the state: */
    fetch_total = ic->_tme_m68k_insn_buffer_fetch_total;
    fetch_sizes = ic->_tme_m68k_insn_buffer_fetch_sizes;
    raw_avail = fetch_total;
  }

  /* else, if what is one, we're emptying the instruction buffer into
     an exception frame: */
  else if (what == 1) {
    fill = FALSE;
    sanity_assert = TRUE;

    /* the fetch total and sizes are in the state: */
    fetch_total = ic->_tme_m68k_insn_buffer_fetch_total;
    fetch_sizes = ic->_tme_m68k_insn_buffer_fetch_sizes;

    /* the first word we place into the exception frame is
       (fetch_sizes << 4) | (fetch_total >> 1): */
    _FETCH_SANITY(raw_avail >= sizeof(tme_uint16_t));
    raw[1] = (fetch_sizes << 4) | (fetch_total >> 1);
    raw[0] = (fetch_sizes >> 4);
    raw += sizeof(tme_uint16_t);
    raw_avail -= sizeof(tme_uint16_t);
  }

  /* otherwise, we're filling the instruction buffer from an exception
     frame: */
  else {
    fill = TRUE;
    sanity_assert = FALSE;

    /* this function previously emptied the instruction buffer into
       this exception frame, and the first big-endian word placed
       there is (fetch_sizes << 4) | (fetch_total >> 1): */
    _FETCH_SANITY(raw_avail >= sizeof(tme_uint16_t));
    fetch_total = (raw[1] & 0x0f) << 1;
    fetch_sizes = (raw[0] << 4) | (raw[1] >> 4);
    raw += sizeof(tme_uint16_t);
    raw_avail -= sizeof(tme_uint16_t);
  }

  /* fetch_total must be even, because we only fetch some multiple of
     16-bit words: */
  _FETCH_SANITY((fetch_total & (sizeof(tme_uint16_t) - 1)) == 0);

  /* fetch_sizes is a bitmask, with a one bit representing a 32-bit
     fetch and a zero bit representing a 16-bit fetch, and the least
     significant bit is the *last* fetch performed.  count the number
     of significant bits in fetch_sizes and confirm that it makes
     sense with fetch_total: */
  fetch_sizes_bits = 0;
  for (fetch_off = 0; fetch_off < fetch_total; ) {
    _FETCH_SANITY(fetch_sizes_bits < _FETCH_SIZES_BITS);
    fetch_off += ((fetch_sizes & (1 << fetch_sizes_bits))
		  /* a 32-bit fetch: */
		  ? sizeof(tme_uint32_t)
		  /* a 16-bit fetch: */
		  : sizeof(tme_uint16_t));
    fetch_sizes_bits++;
  }
  _FETCH_SANITY(fetch_off == fetch_total);

  /* we must have enough raw space available: */
  _FETCH_SANITY(raw_avail >= fetch_total);

  /* shift fetch_sizes up so the bit for the first transfer
     is the most significant bit: */
  fetch_sizes <<= (_FETCH_SIZES_BITS - fetch_sizes_bits);

  /* now fill or empty the instruction buffer: */
  insn_buffer_off = 0;
  for (fetch_off = 0; fetch_off < fetch_total; ) {

    /* get the size of this fetch: */
    fetch_size = ((fetch_sizes & _FETCH_SIZE_BIT)
		  /* a 32-bit fetch: */
		  ? sizeof(tme_uint32_t)
		  /* a 16-bit fetch: */
		  : sizeof(tme_uint16_t));
    fetch_sizes <<= 1;

    /* do one transfer.  the insn buffer is kept in host byte
       order, like the internal registers are: */
    insn_buffer_off = TME_ALIGN(insn_buffer_off, fetch_size);
    if (fill) {
      for (resid = fetch_size; resid-- > 0; ) {
	ic->_tme_m68k_insn_buffer[(insn_buffer_off
				   + (resid
#ifndef WORDS_BIGENDIAN
				      ^ (fetch_size - 1)
#endif /* !WORDS_BIGENDIAN */
				      ))] 
	  = raw[fetch_off + resid];
      }
    }
    else {
      for (resid = fetch_size; resid-- > 0; ) {
	raw[fetch_off + resid] = 
	  ic->_tme_m68k_insn_buffer[(insn_buffer_off
				     + (resid
#ifndef WORDS_BIGENDIAN
					^ (fetch_size - 1)
#endif /* !WORDS_BIGENDIAN */
					))];
      }
    }
    insn_buffer_off += fetch_size;
    fetch_off += fetch_size;

    /* if we faulted somewhere in the fast executor, we need to
       account for the instruction fetches in the group0 sequence: */
    if (what == 0) {
      ic->_tme_m68k_group0_sequence._tme_m68k_sequence_transfer_next++;
    }
  }

  /* NB: for total consistency we might want to store the fetch total
     and sizes into the state when we're filling the instruction
     buffer from an execution frame.  however this isn't really needed
     because the fetch pattern from the restarting slow executor
     should be exactly the same and it doesn't care anyways. 
     if the user bashes the exception frame all bets are off. */

  /* return the number of bytes we put in an exception frame: */
  return (sizeof(tme_uint16_t) + fetch_total);
}

/* this is the group 0 fault hook for the fast executor: */
void
tme_m68k_group0_hook_fast(struct tme_m68k *ic)
{
  struct tme_m68k_tlb *tlb;
  tme_uint8_t *raw;

  /* fill the instruction buffer and increase the transfer count
     as if the slow executor had been doing the fetching: */
  /* NB that this breaks const: */
  tlb = TME_ATOMIC_READ(struct tme_m68k_tlb *, ic->_tme_m68k_itlb);
  raw = (tme_uint8_t *) (tlb->tme_m68k_tlb_emulator_off_read + ic->tme_m68k_ireg_pc);
  tme_m68k_insn_buffer_xfer(ic, raw, 0, 0);
}

/* this starts a read/modify/write cycle.  this works in conjunction
   with the tme_m68k_readSIZE() and tme_m68k_writeSIZE() functions to
   aggregate many bus transactions into a larger transaction: */
struct tme_m68k_tlb *
tme_m68k_rmw_start(struct tme_m68k *ic)
{
  struct tme_m68k_tlb *tlb;

  /* if the user reran the cycle, do nothing: */
  if (TME_M68K_SEQUENCE_RESTARTING
      && (ic->_tme_m68k_group0_buffer_read_softrr > 0
	  || ic->_tme_m68k_group0_buffer_write_softrr > 0)) {
    return (NULL);
  }

  /* we always rerun read/modify/write cycles in their entirety: */
  ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_faulted
    = ic->_tme_m68k_sequence._tme_m68k_sequence_transfer_next - 1;

  /* get an applicable TLB entry: */
  tlb = TME_M68K_TLB_ENTRY(ic, ic->_tme_m68k_ea_function_code, ic->_tme_m68k_ea_address);

  /* we *must* guarantee that a read/modify/write cycle be atomic.
     unfortunately, the only way we can really do that is to acquire a
     single lock, now, that somehow protects all of the things we want
     to do, and hold that lock for the duration.

     the only way we can do this is to require that read/modify/write
     cycles always involve TLB entries that allow fast reads and
     writes.  this gives us a single rwlock that we can lock for
     writing now and hold until we're done: */

  /* we invalidate the TLB entry so we can set the TLB rwlock to NULL,
     which is seen by the first tme_m68k_readSIZE (or
     tme_m68k_writeSIZE, in the bizarre case that that's called first)
     as a signal that, after it reloads the TLB entry, it has to lock
     the rwlock: */
  tme_bus_tlb_invalidate(&tlb->tme_m68k_tlb_bus_tlb);
  tlb->tme_m68k_tlb_bus_rwlock = NULL;

  return (tlb);
}

/* this finishes a read/modify/write cycle.  this works in conjunction
   with the tme_m68k_readSIZE() and tme_m68k_writeSIZE() functions to
   aggregate many bus transactions into a larger transaction: */
void
tme_m68k_rmw_finish(struct tme_m68k *ic, struct tme_m68k_tlb *tlb)
{
  
  /* if we didn't acquire the rwlock, something is wrong: */
  assert(tlb->tme_m68k_tlb_bus_rwlock != NULL);
  
  /* unlock the lock: */
  tme_rwlock_unlock(tlb->tme_m68k_tlb_bus_rwlock);
}

/* this handles a bitfield offset.  if the bitfield is in memory,
   and it hasn't already been done, this adjusts the effective
   address to point to the beginning of the bitfield.  this always
   returns a nonnegative bitfield offset: */
unsigned int
tme_m68k_bitfield_offset(struct tme_m68k *ic, int adjust)
{
  tme_int16_t specop;
  tme_int32_t bf_offset;
  tme_int32_t bf_ea_offset;
    
  /* get the bitfield offset from a data register or as an immediate: */
  specop = ic->_tme_m68k_insn_specop;
  bf_offset = ((specop & TME_BIT(11))
	       ? ic->tme_m68k_ireg_int32(TME_M68K_IREG_D0 + TME_FIELD_EXTRACTU(specop, 6, 3))
	       : (tme_int32_t) TME_FIELD_EXTRACTU(specop, 6, 5));

  /* if this bitfield is in a register (EA mode field is zero): */
  if (TME_FIELD_EXTRACTU(ic->_tme_m68k_insn_opcode, 3, 3) == 0) {

    /* adjust the bitfield offset to be nonnegative: */
    bf_offset &= 31;
  }

  /* otherwise, this bitfield is in memory: */
  else {

    /* calculate the effective address offset and adjust the bitfield
       offset to be nonnegative: */
    bf_ea_offset = ((bf_offset < 0
		     ? (bf_offset - 7)
		     : bf_offset)
		    / 8);
    bf_offset &= 7;

    /* if this is our first call to this function for this instruction
       and we're not restarting, adjust the effective address: */
    if (adjust
	&& !TME_M68K_SEQUENCE_RESTARTING) {
      ic->_tme_m68k_ea_address += bf_ea_offset;
    }
  }

  /* return the nonnegative bitfield offset: */
  return ((unsigned int) bf_offset);
}

/* this returns a bitfield width: */
unsigned int
tme_m68k_bitfield_width(struct tme_m68k *ic)
{
  unsigned int bf_width;
  tme_int16_t specop;

  /* get the bitfield width from a register or as an immediate: */
  specop = ic->_tme_m68k_insn_specop;
  if (specop & TME_BIT(5)) {
    bf_width = ic->tme_m68k_ireg_uint32(TME_M68K_IREG_D0 + TME_FIELD_EXTRACTU(specop, 0, 3));
    bf_width &= 31;
  }
  else {
    bf_width = TME_FIELD_EXTRACTU(specop, 0, 5);
  }
  if (bf_width == 0) bf_width = 32;
  return (bf_width);
}

/* this reads a bitfield: */
tme_uint32_t
_tme_m68k_bitfield_read(struct tme_m68k *ic, int is_signed)
{
  unsigned int bf_offset, bf_width;
  unsigned int shift;
  tme_uint8_t *bf_bytes;
  tme_uint32_t bf_value;
  int ireg;

  /* get the bitfield offset and width: */
  bf_offset = tme_m68k_bitfield_offset(ic, TRUE);
  bf_width = tme_m68k_bitfield_width(ic);

  /* if this expression is > 32, in a register this means the bitfield
     wraps, and in memory this means the bitfield covers 5 bytes: */
  shift = (bf_offset + bf_width);

  /* if this bitfield is in a register (EA mode field is zero): */
  if (TME_FIELD_EXTRACTU(ic->_tme_m68k_insn_opcode, 3, 3) == 0) {
    ireg = (TME_M68K_IREG_D0
	    + TME_FIELD_EXTRACTU(ic->_tme_m68k_insn_opcode, 0, 3));

    /* get the raw 32-bit word containing the bitfield: */
    bf_value = ic->tme_m68k_ireg_uint32(ireg);

    /* if this bitfield wraps the register, shift in the wrapped part
       on the right: */
    if (shift > 32) {
      shift -= 32;
      bf_value = (bf_value << shift) | (bf_value >> (32 - shift));
      bf_offset -= shift;
    }
  }

  /* otherwise, this bitfield is in memory: */
  else {

    /* this instruction can fault: */
    ic->_tme_m68k_mode_flags |= TME_M68K_EXECUTION_INST_CANFAULT;

    /* read in the bytes covering the bitfield: */
    bf_bytes = (tme_uint8_t *) &ic->tme_m68k_ireg_memx32;
    tme_m68k_read_mem(ic, bf_bytes, (bf_offset + bf_width + 7) / 8);

    /* get the raw 32-bit word containing the bitfield: */
    bf_value = tme_betoh_u32(ic->tme_m68k_ireg_memx32);

    /* if this bitfield covers 5 bytes, shift in the part from the fifth byte
       (actually in memy32!) on the right: */
    if (shift > 32) {
      shift -= 32;
      bf_value = (bf_value << shift) | (bf_bytes[4] >> (8 - shift));
      bf_offset -= shift;
    }
  }
  
  /* shift the value: */
  shift = (32 - (bf_offset + bf_width));
  bf_value >>= shift;

  /* mask the value: */
  bf_value &= (0xffffffffUL >> (32 - bf_width));

  /* if this is a signed value, sign-extend it: */
  if (is_signed
      && (bf_value & TME_BIT(bf_width - 1))) {
    bf_value |= (0xffffffffUL << (bf_width - 1));
  }

  /* all bitfield instructions that read the bitfield set the flags: */
  if (!TME_M68K_SEQUENCE_RESTARTING) {
    ic->tme_m68k_ireg_ccr = ((ic->tme_m68k_ireg_ccr & TME_M68K_FLAG_X)
			     | ((bf_value & TME_BIT(bf_width - 1))
				? TME_M68K_FLAG_N
				: 0)
			     | (bf_value
				? 0
				: TME_M68K_FLAG_Z));
  }

  /* return the bitfield value: */
  return (bf_value);
}

/* this writes a bitfield to memory: */
void
tme_m68k_bitfield_write_unsigned(struct tme_m68k *ic, tme_uint32_t bf_value, int set_flags)
{
  unsigned int bf_offset, bf_width;
  unsigned int shift;
  tme_uint8_t *bf_bytes;
  unsigned int count;
  int ireg;

  /* for bitfields in memory, we want to know if the memory covering
     the bitfield is already in our memory buffer, so we can avoid
     reading that memory again.  all bitfield instructions set flags
     based on a bitfield value; if set_flags is FALSE our caller
     must have tested the old bitfield value, and so the bitfield
     memory must be in our buffer, otherwise assume that this is our
     first access to the bitfield memory: */
#define first_memory set_flags
  
  /* get the bitfield offset and width: */
  bf_offset = tme_m68k_bitfield_offset(ic, first_memory);
  bf_width = tme_m68k_bitfield_width(ic);

  /* if this expression is > 32, in a register this means the bitfield
     wraps, and in memory this means the bitfield covers 5 bytes: */
  shift = (bf_offset + bf_width);

  /* mask the value: */
  bf_value &= (0xffffffffUL >> (32 - bf_width));

  /* if we're supposed to, set the flags: */
  if (set_flags
      && !TME_M68K_SEQUENCE_RESTARTING) {
    ic->tme_m68k_ireg_ccr = ((ic->tme_m68k_ireg_ccr & TME_M68K_FLAG_X)
			     | ((bf_value & TME_BIT(bf_width - 1))
				? TME_M68K_FLAG_N
				: 0)
			     | (bf_value
				? 0
				: TME_M68K_FLAG_Z));
  }

  /* if this bitfield is in a register (EA mode field is zero): */
  if (TME_FIELD_EXTRACTU(ic->_tme_m68k_insn_opcode, 3, 3) == 0) {
    ireg = (TME_M68K_IREG_D0
	    + TME_FIELD_EXTRACTU(ic->_tme_m68k_insn_opcode, 0, 3));
    
    /* if this bitfield wraps the register, put the wrapped
       part in the left: */
    if (shift > 32) {
      shift -= 32;
      ic->tme_m68k_ireg_uint32(ireg) = ((ic->tme_m68k_ireg_uint32(ireg)
					 & (0xffffffffUL >> shift))
					| (bf_value << (32 - shift)));
      bf_value >>= shift;
      bf_width -= shift;
    }
    
    /* update the register: */
    shift = (32 - (bf_offset + bf_width));
    ic->tme_m68k_ireg_uint32(ireg) = ((ic->tme_m68k_ireg_uint32(ireg)
				       & ~((0xffffffffUL >> (32 - bf_width)) << shift))
				      | (bf_value << shift));
  }

  /* otherwise, this bitfield is in memory: */
  else {

    /* this instruction can fault: */
    ic->_tme_m68k_mode_flags |= TME_M68K_EXECUTION_INST_CANFAULT;

    /* read in the bytes covering the bitfield if we haven't yet: */
    bf_bytes = (tme_uint8_t *) &ic->tme_m68k_ireg_memx32;
    count = (bf_offset + bf_width + 7) / 8;
    if (first_memory) {
      tme_m68k_read_mem(ic, bf_bytes, count);
    }

    /* if this bitfield covers 5 bytes, put the part for the fifth
       byte (actually in memy32!) in on the left: */
    if (shift > 32) {
      shift -= 32;
      if (!TME_M68K_SEQUENCE_RESTARTING) {
	bf_bytes[4] = ((bf_bytes[4]
			& (0xff >> shift))
		       | ((bf_value & 0xff) << (8 - shift)));
      }
      bf_value >>= shift;
      bf_width -= shift;
    }

    /* update the memory buffer: */
    if (!TME_M68K_SEQUENCE_RESTARTING) {
      shift = (32 - (bf_offset + bf_width));
      ic->tme_m68k_ireg_memx32 =
	tme_htobe_u32((tme_betoh_u32(ic->tme_m68k_ireg_memx32)
		       & ~((0xffffffffUL >> (32 - bf_width)) << shift))
		      | (bf_value << shift));
    }

    /* write out the bytes covering bitfield to memory: */
    tme_m68k_write_mem(ic, bf_bytes, count);
  }
#undef first_memory
}

/* our global verify hook function: */
#undef tme_m68k_verify_hook
void
tme_m68k_verify_hook(void)
{
}

#if 1
#include <stdio.h>

/* this dumps out the m68k state: */
void
tme_m68k_dump(struct tme_m68k *ic)
{
  int ireg;
  int count;

  /* dump out the integer registers: */
  count = 0;
  for (ireg = TME_M68K_IREG_D0;
       ireg <= TME_M68K_IREG_A7;
       ireg++) {
    fprintf(stderr,
	    "%%%c%d[%p] = 0x%08x",
	    (ireg < TME_M68K_IREG_A0
	     ? 'd'
	     : 'a'),
	    ireg - (ireg < TME_M68K_IREG_A0
		    ? TME_M68K_IREG_D0
		    : TME_M68K_IREG_A0),
	    &ic->tme_m68k_ireg_uint32(ireg),
	    ic->tme_m68k_ireg_uint32(ireg));
    if (++count == 2) {
      fprintf(stderr, "\n");
      count = 0;
    }
    else {
      fprintf(stderr, "  ");
    }
  }

  /* dump out the PC and next PC: */
  fprintf(stderr, "%%pc = 0x%08x  %%pc_next = 0x%08x\n",
	  ic->tme_m68k_ireg_pc,
	  ic->tme_m68k_ireg_pc_next);

  /* dump out the status register: */
  fprintf(stderr, "%%sr = 0x%04x", ic->tme_m68k_ireg_sr);
  fprintf(stderr, "  flags:");
  if (ic->tme_m68k_ireg_ccr & TME_M68K_FLAG_X) {
    fprintf(stderr, " X");
  }
  if (ic->tme_m68k_ireg_ccr & TME_M68K_FLAG_N) {
    fprintf(stderr, " N");
  }
  if (ic->tme_m68k_ireg_ccr & TME_M68K_FLAG_Z) {
    fprintf(stderr, " Z");
  }
  if (ic->tme_m68k_ireg_ccr & TME_M68K_FLAG_V) {
    fprintf(stderr, " V");
  }
  if (ic->tme_m68k_ireg_ccr & TME_M68K_FLAG_C) {
    fprintf(stderr, " C");
  }
  fprintf(stderr, "\n");

  /* dump out the effective address and memory buffers: */
  fprintf(stderr, "\n");
  fprintf(stderr, "EA = %d:0x%08x\n",
	  ic->_tme_m68k_ea_function_code,
	  ic->_tme_m68k_ea_address);
  fprintf(stderr, "%%memx[%p] = 0x%08x  %%memy[%p] = 0x%08x\n",
	  &ic->tme_m68k_ireg_memx32,
	  ic->tme_m68k_ireg_memx32,
	  &ic->tme_m68k_ireg_memy32,
	  ic->tme_m68k_ireg_memy32);

  /* dump out the control registers: */
  fprintf(stderr, "\n");
  fprintf(stderr, "%%usp = 0x%08x\n", ic->tme_m68k_ireg_usp);
  fprintf(stderr, "%%isp = 0x%08x\n", ic->tme_m68k_ireg_isp);
  fprintf(stderr, "%%msp = 0x%08x\n", ic->tme_m68k_ireg_msp);
  fprintf(stderr, "%%sfc = 0x%08x\n", ic->tme_m68k_ireg_sfc);
  fprintf(stderr, "%%dfc = 0x%08x\n", ic->tme_m68k_ireg_dfc);
  fprintf(stderr, "%%vbr = 0x%08x\n", ic->tme_m68k_ireg_vbr);
  
  /* dump out instruction decoding information: */
  fprintf(stderr, "\n");
  fprintf(stderr, "opcode = 0x%04x  specop = 0x%04x\n",
	  ic->_tme_m68k_insn_opcode,
	  ic->_tme_m68k_insn_specop);
}
#endif /* 1 */
