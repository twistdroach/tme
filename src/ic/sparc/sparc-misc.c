/* $Id: sparc-misc.c,v 1.7 2007/08/24 01:21:50 fredette Exp $ */

/* ic/sparc/sparc-misc.c - miscellaneous things for the SPARC emulator: */

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

/* includes: */
#include "sparc-impl.h"

_TME_RCSID("$Id: sparc-misc.c,v 1.7 2007/08/24 01:21:50 fredette Exp $");

/* our bus signal handler: */
static int
_tme_sparc_bus_signal(struct tme_bus_connection *conn_bus, unsigned int signal)
{
  struct tme_sparc *ic;
  unsigned int level_edge;

  /* recover our IC: */
  ic = conn_bus->tme_bus_connection.tme_connection_element->tme_element_private;

  /* take out the level and edge: */
  level_edge = signal;
  signal = TME_BUS_SIGNAL_WHICH(signal);
  level_edge ^= signal;

  /* lock the external mutex: */
  tme_mutex_lock(&ic->tme_sparc_external_mutex);

  /* on the falling edge of HALT or RESET, halt the processor: */
  if (((level_edge & TME_BUS_SIGNAL_LEVEL_MASK)
       == TME_BUS_SIGNAL_LEVEL_ASSERTED)
      && (signal == TME_BUS_SIGNAL_HALT
	  || signal == TME_BUS_SIGNAL_RESET)) {
    ic->tme_sparc_external_halt = TRUE;
  }

  /* on the rising edge of RESET, reset the processor: */
  else if (signal == TME_BUS_SIGNAL_RESET
	   && ((level_edge & TME_BUS_SIGNAL_LEVEL_MASK)
	       == TME_BUS_SIGNAL_LEVEL_NEGATED)) {
    ic->tme_sparc_external_reset = TRUE;
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
  tme_mutex_unlock(&ic->tme_sparc_external_mutex);

  /* notify any threads waiting on the external condition: */
  tme_cond_notify(&ic->tme_sparc_external_cond, TRUE);
  return (TME_OK);
}

/* our interrupt handler: */
static int
_tme_sparc_bus_interrupt(struct tme_sparc_bus_connection *conn_sparc, unsigned int ipl)
{
  struct tme_sparc *ic;

  /* recover our IC: */
  ic = conn_sparc->tme_sparc_bus_connection.tme_bus_connection.tme_connection_element->tme_element_private;

  /* lock the external mutex: */
  tme_mutex_lock(&ic->tme_sparc_external_mutex);

  /* set the interrupt line: */
  ic->tme_sparc_external_ipl = ipl;

  /* unlock the external mutex: */
  tme_mutex_unlock(&ic->tme_sparc_external_mutex);

  /* notify any threads waiting on the external condition: */
  tme_cond_notify(&ic->tme_sparc_external_cond, TRUE);
  return (TME_OK);
}

/* the idle function, used when the processor is halted or stopped: */
static void
tme_sparc_idle(struct tme_sparc *ic)
{  
  /* lock the external mutex: */
  tme_mutex_lock(&ic->tme_sparc_external_mutex);

  /* loop forever: */
  for (;;) {

    /* check for any external signal: */
    tme_sparc32_external_check(ic);

    /* await an external condition: */
    tme_cond_wait_yield(&ic->tme_sparc_external_cond, &ic->tme_sparc_external_mutex);
  }
}

/* the sparc thread: */
static void
tme_sparc_thread(struct tme_sparc *ic)
{

  /* we use longjmp to redispatch: */
  do { } while (setjmp(ic->_tme_sparc_dispatcher));

  /* we must not have a busy instruction TLB entry: */
  assert (ic->_tme_sparc_itlb_busy == NULL);

  /* dispatch on the current mode: */
  switch (ic->_tme_sparc_mode) {

  case TME_SPARC_MODE_EXECUTION:
    (*ic->_tme_sparc_execute)(ic);
    /* NOTREACHED */

  case TME_SPARC_MODE_STOP:
  case TME_SPARC_MODE_HALT:
    tme_sparc_idle(ic);
    /* NOTREACHED */

  default:
    abort();
  }
  /* NOTREACHED */
}

/* the TLB filler for when we are on a generic bus: */
static int
_tme_sparc_generic_tlb_fill(struct tme_sparc_bus_connection *conn_sparc, 
			    struct tme_sparc_tlb *tlb,
			    unsigned int address_space, 
			    tme_bus_addr_t external_address, 
			    unsigned int cycles)
{
  struct tme_sparc *ic;

  /* recover our IC: */
  ic = conn_sparc->tme_sparc_bus_connection.tme_bus_connection.tme_connection_element->tme_element_private;

  /* call the generic bus TLB filler: */
  (ic->_tme_sparc_bus_generic->tme_bus_tlb_fill)
    (ic->_tme_sparc_bus_generic,
     &tlb->tme_sparc_tlb_bus_tlb,
     external_address,
     cycles);
  
  /* when we're on a generic bus a TLB entry is valid for all address spaces: */
  tlb->tme_sparc_tlb_asi_mask = ((tme_uint32_t) 0) - 1;

  return (TME_OK);
}

/* the sparc command function: */
static int
_tme_sparc_command(struct tme_element *element, const char * const * args, char **_output)
{
  struct tme_sparc *ic;
  unsigned int idle_type_saved;

  /* recover our IC: */
  ic = (struct tme_sparc *) element->tme_element_private;

  /* the "idle-type" command: */
  if (TME_ARG_IS(args[1], "idle-type")) {

    /* save the current idle type and set it to none: */
    idle_type_saved = ic->tme_sparc_idle_type;
    ic->tme_sparc_idle_type = TME_SPARC_IDLE_TYPE_NULL;

    /* if we're not setting the idle type to none: */
    if (!TME_ARG_IS(args[2], "none")) {

      /* check for a supported idle type: */
#define _TME_SPARC_IDLE_TYPE(x, s)		\
  do {						\
    if ((TME_SPARC_IDLE_TYPES_SUPPORTED		\
         & (x))					\
      && TME_ARG_IS(args[2], s)) {		\
      ic->tme_sparc_idle_type = (x);		\
    }						\
  } while (/* CONSTCOND */ 0)
      _TME_SPARC_IDLE_TYPE(TME_SPARC_IDLE_TYPE_NETBSD32_TYPE_0, "netbsd32-type-0");
      _TME_SPARC_IDLE_TYPE(TME_SPARC_IDLE_TYPE_SUNOS32_TYPE_0, "sunos32-type-0");
#undef _TME_SPARC_IDLE_TYPE

      /* if the idle type isn't supported: */
      if (ic->tme_sparc_idle_type == TME_SPARC_IDLE_TYPE_NULL) {

	/* restore the idle type and return a usage: */
	ic->tme_sparc_idle_type = idle_type_saved;
    
	tme_output_append_error(_output,
				"%s %s idle-type { none",
				_("usage:"),
				args[0]);

      /* add in the supported idle types: */
#define _TME_SPARC_IDLE_TYPE(x, s)		\
  do {						\
    if (TME_SPARC_IDLE_TYPES_SUPPORTED		\
	& (x)) {				\
      tme_output_append_error(_output, " | %s",	\
			      s);		\
    }						\
  } while (/* CONSTCOND */ 0)
	_TME_SPARC_IDLE_TYPE(TME_SPARC_IDLE_TYPE_NETBSD32_TYPE_0, "netbsd32-type-0");
	_TME_SPARC_IDLE_TYPE(TME_SPARC_IDLE_TYPE_SUNOS32_TYPE_0, "sunos32-type-0");
#undef _TME_SPARC_IDLE_TYPE

	tme_output_append_error(_output, " }");
	return (EINVAL);
      }
    }

    /* poison all idle type state: */
    ic->tme_sparc_idle_type_pc32 = TME_SPARC_IDLE_TYPE_PC_STATE(1);
  }

  /* any other command: */
  else {
    if (args[1] != NULL) {
      tme_output_append_error(_output,
			      "%s '%s', ",
			      _("unknown command"),
			      args[1]);
    }
    tme_output_append_error(_output,
			    _("available %s commands:%s"),
			    args[0],
			    (TME_SPARC_IDLE_TYPES_SUPPORTED != 0
			     ? " idle-type"
			     : ""));
    return (EINVAL);
  }

  return (TME_OK);
}

/* the connection scorer: */
static int
_tme_sparc_connection_score(struct tme_connection *conn, unsigned int *_score)
{
  struct tme_sparc_bus_connection *conn_sparc;
  struct tme_bus_connection *conn_bus;
  unsigned int score;

  /* assume that this connection is useless: */
  score = 0;

  /* dispatch on the connection type: */
  conn_sparc = (struct tme_sparc_bus_connection *) conn->tme_connection_other;
  conn_bus = (struct tme_bus_connection *) conn->tme_connection_other;
  switch (conn->tme_connection_type) {

    /* this must be a bus, and not another sparc chip: */
  case TME_CONNECTION_BUS_SPARC:
    if (conn_bus->tme_bus_tlb_set_allocate != NULL
	&& conn_sparc->tme_sparc_bus_tlb_fill != NULL
	&& conn_sparc->tme_sparc_bus_fpu_strict == NULL) {
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
_tme_sparc_connection_make(struct tme_connection *conn, unsigned int state)
{
  struct tme_sparc *ic;
  struct tme_sparc_bus_connection *conn_sparc;
  struct tme_bus_connection *conn_bus;
  struct tme_connection *conn_other;

  /* since the CPU is halted, it won't be making any connection calls,
     so we only have to do work when the connection is fully made: */
  if (state == TME_CONNECTION_FULL) {

    /* recover our IC: */
    ic = conn->tme_connection_element->tme_element_private;
    
    /* dispatch on the connection type: */
    conn_other = conn->tme_connection_other;
    conn_sparc = (struct tme_sparc_bus_connection *) conn_other;
    conn_bus = (struct tme_bus_connection *) conn_other;
    switch (conn->tme_connection_type) {
      
    case TME_CONNECTION_BUS_SPARC:
      ic->_tme_sparc_bus_connection = conn_sparc;
      break;
      
      /* we need an adaptation layer: */
    case TME_CONNECTION_BUS_GENERIC:
      conn_sparc = tme_new0(struct tme_sparc_bus_connection, 1);
      conn_sparc->tme_sparc_bus_connection.tme_bus_connection.tme_connection_element = conn->tme_connection_element;
      conn_sparc->tme_sparc_bus_tlb_fill = _tme_sparc_generic_tlb_fill;
      ic->_tme_sparc_bus_connection = conn_sparc;
      ic->_tme_sparc_bus_generic = conn_bus;
      break;
      
    default: abort();
    }

    /* allocate the DTLB hash set: */
    (*ic->_tme_sparc_bus_connection->tme_sparc_bus_connection.tme_bus_tlb_set_allocate)
      (&ic->_tme_sparc_bus_connection->tme_sparc_bus_connection, 
       _TME_SPARC_DTLB_HASH_SIZE, 
       sizeof(struct tme_sparc_tlb),
       &ic->_tme_sparc_dtlb_array_bus,
       &ic->_tme_sparc_tlb_rwlock);

    /* allocate the ITLB hash set: */
    (*ic->_tme_sparc_bus_connection->tme_sparc_bus_connection.tme_bus_tlb_set_allocate)
      (&ic->_tme_sparc_bus_connection->tme_sparc_bus_connection, 
       _TME_SPARC_ITLB_HASH_SIZE, 
       sizeof(struct tme_sparc_tlb),
       &ic->_tme_sparc_itlb_array_bus,
       &ic->_tme_sparc_tlb_rwlock);
  }

  /* NB: the machine needs to issue a reset to bring the CPU out of halt. */
  return (TME_OK);
}

/* this breaks a connection: */
static int 
_tme_sparc_connection_break(struct tme_connection *conn, unsigned int state)
{
  abort();
  return (0);
}

/* this makes new connection sides: */
static int
_tme_sparc_connections_new(struct tme_element *element, const char * const *args, struct tme_connection **_conns, char **_output)
{
  struct tme_sparc_bus_connection *conn_sparc;
  struct tme_bus_connection *conn_bus;
  struct tme_connection *conn;

  /* if we already have a bus connection, we can take no more connections: */
  if (((struct tme_sparc *) element->tme_element_private)->_tme_sparc_bus_connection != NULL) {
    return (TME_OK);
  }

  /* create our side of an sparc bus connection: */
  conn_sparc = tme_new0(struct tme_sparc_bus_connection, 1);
  conn_bus = &conn_sparc->tme_sparc_bus_connection;
  conn = &conn_bus->tme_bus_connection;

  /* fill in the generic connection: */
  conn->tme_connection_next = *_conns;
  conn->tme_connection_type = TME_CONNECTION_BUS_SPARC;
  conn->tme_connection_score = _tme_sparc_connection_score;
  conn->tme_connection_make = _tme_sparc_connection_make;
  conn->tme_connection_break = _tme_sparc_connection_break;

  /* fill in the generic bus connection: */
  conn_bus->tme_bus_signal = _tme_sparc_bus_signal;
  conn_bus->tme_bus_tlb_set_allocate = NULL;

  /* full in the sparc bus connection: */
  conn_sparc->tme_sparc_bus_interrupt = _tme_sparc_bus_interrupt;
  conn_sparc->tme_sparc_bus_tlb_fill = NULL;
  conn_sparc->tme_sparc_bus_fpu_strict = tme_sparc_fpu_strict;

  /* add this connection to the set of possibilities: */
  *_conns = conn;

  /* create our side of a generic bus connection: */
  conn_bus = tme_new0(struct tme_bus_connection, 1);
  conn = &conn_bus->tme_bus_connection;

  /* fill in the generic connection: */
  conn->tme_connection_next = *_conns;
  conn->tme_connection_type = TME_CONNECTION_BUS_GENERIC;
  conn->tme_connection_score = _tme_sparc_connection_score;
  conn->tme_connection_make = _tme_sparc_connection_make;
  conn->tme_connection_break = _tme_sparc_connection_break;

  /* fill in the generic bus connection: */
  conn_bus->tme_bus_signal = _tme_sparc_bus_signal;
  conn_bus->tme_bus_tlb_set_allocate = NULL;
  conn_bus->tme_bus_tlb_fill = NULL;

  /* add this connection to the set of possibilities: */
  *_conns = conn;

  /* done: */
  return (TME_OK);
}

/* the common sparc new function: */
int
tme_sparc_new(struct tme_sparc *ic, const char * const *args, const void *extra, char **_output)
{
  struct tme_element *element;
  int arg_i;
  int usage;

  /* assume that we have no FPU: */
  ic->tme_sparc_fpu_fsr = TME_SPARC_FSR_VER_missing;

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
      if (!tme_sparc_fpu_new(ic, args, &arg_i, &usage, _output)) {
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
    tme_sparc_fpu_usage(ic, _output);
    tme_free(ic);
    return (EINVAL);
  }

  /* initialize the verifier: */
  tme_sparc_verify_init();

  /* we have no bus connection yet: */
  ic->_tme_sparc_bus_connection = NULL;

  /* fill the element: */
  element = ic->tme_sparc_element;
  element->tme_element_private = ic;
  element->tme_element_connections_new = _tme_sparc_connections_new;
  element->tme_element_command = _tme_sparc_command;

  /* calculate the instruction burst size: */
  /* XXX TBD: */
  ic->_tme_sparc_instruction_burst = 800;
  ic->_tme_sparc_instruction_burst_remaining
    = ic->_tme_sparc_instruction_burst;

  /* force the processor to be halted: */
  ic->_tme_sparc_mode = TME_SPARC_MODE_HALT;

  /* poison all idle type state: */
  ic->tme_sparc_idle_type_pc32 = TME_SPARC_IDLE_TYPE_PC_STATE(1);

  /* start the sparc thread: */
  tme_thread_create((tme_thread_t) tme_sparc_thread, ic);

  return (TME_OK);
}  

/* this redispatches: */
void
tme_sparc_redispatch(struct tme_sparc *ic)
{
  struct tme_sparc_tlb *tlb;

  /* if we have a busy instruction TLB entry: */
  tlb = ic->_tme_sparc_itlb_busy;
  if (__tme_predict_true(tlb != NULL)) {

    /* unbusy and forget the instruction TLB entry: */
    tme_sparc_tlb_unbusy(tlb);
    ic->_tme_sparc_itlb_busy = NULL;
  }

  /* do the redispatch: */
#ifdef _TME_SPARC_STATS
  ic->tme_sparc_stats.tme_sparc_stats_redispatches++;
#endif /* _TME_SPARC_STATS */
  longjmp(ic->_tme_sparc_dispatcher, 1);
}

/* our global verify hook function: */
#undef tme_sparc_verify_hook
void
tme_sparc_verify_hook(void)
{
}

/* the common sparc reset function: */
void
tme_sparc_do_reset(struct tme_sparc *ic)
{

  /* if this is a v7 or v8 CPU: */
  if (ic->tme_sparc_version < 9) {

    /* set the initial PCs: */
    ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC_NEXT) = 0;
    ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC_NEXT_NEXT) = sizeof(tme_uint32_t);
    
    /* force supervisor mode, traps disabled: */
    ic->tme_sparc32_ireg_psr
      = ((ic->tme_sparc32_ireg_psr
	  & ~TME_SPARC32_PSR_ET)
	 | TME_SPARC32_PSR_S);
  }

  /* otherwise, this is a v9 CPU: */
  else {

    /* XXX WRITEME */
    abort();
  }

  /* reset the FPU: */
  tme_sparc_fpu_reset(ic);

  /* poison all idle type state, to force the idle type to retrain: */
  ic->tme_sparc_idle_type_pc32 = TME_SPARC_IDLE_TYPE_PC_STATE(1);

  /* start execution: */
  ic->_tme_sparc_mode = TME_SPARC_MODE_EXECUTION;
  tme_sparc_redispatch(ic);
}

/* the common sparc idle function: */
void
tme_sparc_do_idle(struct tme_sparc *ic)
{

  /* NB: since the interrupt that causes us to leave stop mode will
     call tme_sparc32_trap_preinstruction(), this function can only be
     called on a preinstruction boundary (i.e., while PC still points
     to the (completed!) instruction that triggered the idle
     condition): */

  /* redispatch into stop mode: */
  ic->_tme_sparc_mode = TME_SPARC_MODE_STOP;
  tme_sparc_redispatch(ic);
}

/* this checks for external signals.  this must be called with the
   external mutex held: */
void
tme_sparc32_external_check(struct tme_sparc *ic)
{
  unsigned int ipl;
  int vector;

  /* if an external reset has been requested, start reset trap
     processing: */
  if (ic->tme_sparc_external_reset) {
    ic->tme_sparc_external_reset = FALSE;
    tme_mutex_unlock(&ic->tme_sparc_external_mutex);
    tme_sparc32_trap_preinstruction(ic, TME_SPARC_TRAP_reset);
  }

  /* if an external halt has been requested, halt: */
  if (ic->tme_sparc_external_halt) {
    ic->tme_sparc_external_halt = FALSE;
    tme_mutex_unlock(&ic->tme_sparc_external_mutex);
    ic->_tme_sparc_mode = TME_SPARC_MODE_HALT;
    tme_sparc_redispatch(ic);
  }

  /* if we are not halted and an interrupt can be serviced, start
     interrupt trap processing: */
  ipl = ic->tme_sparc_external_ipl;
  if (ic->_tme_sparc_mode != TME_SPARC_MODE_HALT
      && (ic->tme_sparc32_ireg_psr & TME_SPARC32_PSR_ET)
      && ipl >= TME_SPARC_IPL_MIN
      && ipl <= TME_SPARC_IPL_MAX
      && (ipl == TME_SPARC_IPL_NMI
	  || ipl > TME_FIELD_MASK_EXTRACTU(ic->tme_sparc32_ireg_psr, TME_SPARC32_PSR_PIL))) {
    
    tme_mutex_unlock(&ic->tme_sparc_external_mutex);

    /* acknowledge the interrupt: */
    (*ic->_tme_sparc_bus_connection->tme_sparc_bus_connection.tme_bus_intack)
      (&ic->_tme_sparc_bus_connection->tme_sparc_bus_connection,
       ipl, &vector);

    /* dispatch the trap: */
    tme_sparc32_trap_preinstruction(ic, TME_SPARC_TRAP_interrupt_level(ipl));
  }

  /* there are no traps to process: */
}

/* this triggers sparc32 trap processing on a preinstruction boundary: */
void
tme_sparc32_trap_preinstruction(struct tme_sparc *ic, tme_uint32_t trap)
{

  /* shift the next instruction's PC and next-next PC up: */
  ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC) = ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC_NEXT);
  ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC_NEXT) = ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC_NEXT_NEXT);

  /* do the rest of the sparc32 trap processing: */
  tme_sparc32_trap(ic, trap);
}

/* this triggers sparc32 trap processing by an instruction: */
void
tme_sparc32_trap(struct tme_sparc *ic, tme_uint32_t trap)
{
  unsigned int cwp;
  unsigned int cwp_offset;

  /* reset traps are handled specially: */
  if (__tme_predict_false(trap == TME_SPARC_TRAP_reset)) {
    tme_sparc_do_reset(ic);
    /* NOTREACHED */
  }  

  /* "The processor enters error_mode state when a trap occurs while
     ET = 0. An implementation should preserve as much processor state
     as possible when this happens. Standard trap actions (such as
     decrementing CWP and saving state information in locals) should
     not occur when entering error_mode. In particular, the tt field
     of the TBR is only written during a transition into error_mode
     state in the singular case of a RETT instruction that traps while
     ET = 0. In this case, tt is written to indicate the type of
     exception that was induced by the RETT instruction.

     What occurs after error_mode is entered is
     implementation-dependent; typically the processor triggers an
     external reset, causing a reset trap (see below). */
  if (__tme_predict_false((ic->tme_sparc32_ireg_psr & TME_SPARC32_PSR_ET) == 0)) {

    /* if we were executing a RETT instruction: */
    assert (ic->_tme_sparc_mode == TME_SPARC_MODE_EXECUTION);
    if ((ic->_tme_sparc_insn
	 & ((3 << 30) | (0x3f << 19)))
	== ((tme_uint32_t) (2 << 30) | (0x39 << 19))) {
      
      /* update the TBR register: */
      TME_FIELD_MASK_DEPOSITU(ic->tme_sparc32_ireg_tbr, 0xff, trap);
    }

    /* reset the processor: */
    tme_log(TME_SPARC_LOG_HANDLE(ic), 0, EPERM,
	    (TME_SPARC_LOG_HANDLE(ic),
	     _("took a trap while traps disabled, processor reset")));
    tme_sparc32_trap(ic, TME_SPARC_TRAP_reset);
  }

  /* "Traps are disabled: ET <- 0.
     The existing user/supervisor mode is preserved: PS <- S.
     The user/supervisor mode is changed to supervisor: S <- 1." */
  ic->tme_sparc32_ireg_psr
    = ((ic->tme_sparc32_ireg_psr
	& ~(TME_SPARC32_PSR_ET
	    | TME_SPARC32_PSR_PS))
       | ((ic->tme_sparc32_ireg_psr
	   & TME_SPARC32_PSR_S)
	  / (TME_SPARC32_PSR_S
	     / TME_SPARC32_PSR_PS))
       | TME_SPARC32_PSR_S);

  /* "The register window is advanced to a new window: 
     CWP <- ((CWP - 1) modulo NWINDOWS) 
     [note: without test for window overflow]." */
  cwp = TME_FIELD_MASK_EXTRACTU(ic->tme_sparc32_ireg_psr, TME_SPARC32_PSR_CWP);
  cwp -= 1;
  cwp %= ic->tme_sparc_nwindows;
  TME_FIELD_MASK_DEPOSITU(ic->tme_sparc32_ireg_psr, TME_SPARC32_PSR_CWP, cwp);
  cwp_offset = TME_SPARC_CWP_OFFSET(cwp);
  ic->tme_sparc_cwp_offset = cwp_offset;

  /* "The trapped program counters are saved in local registers 1 and
     2 of the new window: r[17] <- PC, r[18] <- nPC." */
  ic->tme_sparc_ireg_uint32(cwp_offset + 17) = ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC);
  ic->tme_sparc_ireg_uint32(cwp_offset + 18) = ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC_NEXT);

  /* "The tt field is written to the particular value that identifies
     the exception or interrupt request, except as defined for `Reset
     Trap' and `Error Mode' above." */
  TME_FIELD_MASK_DEPOSITU(ic->tme_sparc32_ireg_tbr, 0x00000ff0, trap);

  /* "If the trap is not a reset trap, control is transferred into the
     trap table: PC <- TBR, nPC <- TBR + 4." */
  ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC_NEXT) = ic->tme_sparc32_ireg_tbr;
  ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC_NEXT_NEXT) = ic->tme_sparc32_ireg_tbr + sizeof(tme_uint32_t);

  /* redispatch: */
  ic->_tme_sparc_mode = TME_SPARC_MODE_EXECUTION;
  tme_sparc_redispatch(ic);
}

/* the default slow instruction fetcher: */
tme_uint32_t
tme_sparc32_fetch_slow(struct tme_sparc *ic, int annulled)
{
  tme_uint32_t pc;
  const tme_shared tme_uint8_t *memory;
  struct tme_sparc_tlb *dtlb;
  tme_uint32_t insn;

  /* get the PC of the instruction: */
  pc = ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC);

  /* XXX FIXME - unfortunately, using tme_sparc32_load() means that we
     we have to pollute the DTLB here: */
  /* get and busy the DTLB entry: */
  dtlb = TME_SPARC_DTLB_ENTRY(ic, pc);
  tme_sparc_tlb_busy(dtlb);

  /* do a load for an instruction: */
  memory
    = tme_sparc32_load(ic,
		       pc,
		       (sizeof(tme_uint32_t)
			| TME_SPARC_SLOW_FLAG_INSN
			| (annulled
			   ? TME_SPARC_SLOW_FLAG_NO_FAULTS
			   : 0)));

  /* if this instruction is annulled: */
  if (annulled) {

    /* we can return anything: */
    insn = 0;
  }

  /* otherwise, this instruction is not annulled: */
  else {

    /* XXX FIXME - we don't currently finish a real slow load: */
    abort();
  }

  /* unbusy the DTLB entry: */
  tme_sparc_tlb_unbusy(dtlb);

  /* return the instruction: */
  return (insn);
}

/* the default bus fault to trap mapping function: */
tme_uint32_t
tme_sparc32_bus_fault(struct tme_sparc *ic,
		      const struct tme_bus_cycle *cycle,
		      unsigned int flags,
		      int err)
{
  switch (err) {
  case EFAULT:
  case ENOENT:
  case EIO: 
    if (flags & TME_SPARC_SLOW_FLAG_INSN) {
      return ((flags & TME_SPARC_SLOW_FLAG_NO_FAULTS)
	      ? TME_SPARC_TRAP_none
	      : TME_SPARC_TRAP_instruction_access_exception);
    }
    assert (!(flags & TME_SPARC_SLOW_FLAG_NO_FAULTS));
    return (TME_SPARC_TRAP_data_access_exception);
  default: abort();
  }
}

/* this triggers sparc64 trap processing by an instruction: */
void
tme_sparc64_trap(struct tme_sparc *ic, tme_uint32_t trap)
{
  abort();
}

/* this fetches an instruction close enough to the current instruction
   that it should be within the current instruction TLB entry.  it
   returns all-bits-one if the instruction isn't within the current
   instruction TLB entry: */
tme_uint32_t
tme_sparc_fetch_nearby(struct tme_sparc *ic, long offset_in_insns)
{
  struct tme_sparc_tlb *itlb_current;
  tme_bus_addr_t pc;
  tme_uint32_t insn;

  /* get the address of the current instruction: */
  pc = (
#ifdef TME_HAVE_INT64_T
	(TME_SPARC_VERSION(ic) >= 9)
	? ic->tme_sparc_ireg_uint64(TME_SPARC_IREG_PC)
	:
#endif /* TME_HAVE_INT64_T */
	ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC));

  /* assume that we can't fetch the nearby instruction: */
  insn = 0xffffffff;

  /* there must be a current instruction TLB entry, and it must be
     busy, cover this ASI and address, and allow fast reading: */
  itlb_current = ic->_tme_sparc_itlb_busy;
  assert (itlb_current != NULL);
  /* XXX FIXME - there should be a tme_bus_tlb_assert_busy(): */
  assert (TME_SPARC_TLB_ASI_MASK_OK(itlb_current, ic->tme_sparc_asi_mask_insn));
  assert (itlb_current->tme_sparc_tlb_addr_first <= pc
	  && pc <= itlb_current->tme_sparc_tlb_addr_last);
  assert (itlb_current->tme_sparc_tlb_emulator_off_read != TME_EMULATOR_OFF_UNDEF);

  /* adjust the address to point to the nearby instruction: */
  pc += offset_in_insns * sizeof(tme_uint32_t);

  /* if the instruction TLB entry is valid and also covers this address: */
  if (tme_bus_tlb_is_valid(&itlb_current->tme_sparc_tlb_bus_tlb)
      && itlb_current->tme_sparc_tlb_addr_first <= pc
      && pc <= itlb_current->tme_sparc_tlb_addr_last) {
    
    /* fetch the nearby instruction: */
    insn = tme_memory_bus_read32((const tme_shared tme_uint32_t *) (itlb_current->tme_sparc_tlb_emulator_off_read + pc),
				 itlb_current->tme_sparc_tlb_bus_rwlock,
				 sizeof(tme_uint32_t),
				 (TME_SPARC_VERSION(ic) < 9
				  ? sizeof(tme_uint32_t)
				  : sizeof(tme_uint32_t) * 2));
    insn = tme_betoh_u32(insn);
  }

  return (insn);
}

/* this unlocks data structures before a callout: */
void
tme_sparc_callout_unlock(struct tme_sparc *ic)
{
  struct tme_sparc_tlb *tlb;

  assert ((ic->_tme_sparc_mode == TME_SPARC_MODE_EXECUTION)
	  || (ic->_tme_sparc_itlb_busy == NULL));

  /* if we have a busy instruction TLB entry: */
  tlb = ic->_tme_sparc_itlb_busy;
  if (__tme_predict_true(tlb != NULL)) {

    /* unbusy the instruction TLB entry: */
    tme_sparc_tlb_unbusy(tlb);
  }
}

/* this relocks data structures after a callout: */
void
tme_sparc_callout_relock(struct tme_sparc *ic)
{
  struct tme_sparc_tlb *tlb;

  assert ((ic->_tme_sparc_mode == TME_SPARC_MODE_EXECUTION)
	  || (ic->_tme_sparc_itlb_busy == NULL));

  /* if we have a busy instruction TLB entry: */
  tlb = ic->_tme_sparc_itlb_busy;
  if (__tme_predict_true(tlb != NULL)) {

    /* rebusy the instruction TLB entry: */
    tme_sparc_tlb_busy(tlb);
      
    /* if this instruction TLB entry is invalid: */
    if (tme_bus_tlb_is_invalid(&tlb->tme_sparc_tlb_bus_tlb)) {

      /* poison this instruction TLB entry, so we won't try to do any
	 fast fetches with it: */
      tlb->tme_sparc_tlb_addr_first = 1;
      tlb->tme_sparc_tlb_addr_last = 0;
    }
  }
}

#if 0
#include <stdio.h>

/* this dumps out the sparc state: */
void
tme_sparc32_dump(const struct tme_sparc *ic)
{
  unsigned int cwp_first;
  unsigned int cwp;
  unsigned int reg_i;
  unsigned int reg_base;
  unsigned int ireg;

  /* dump out the windowed integer registers, finishing with the
     current window: */
  cwp_first = TME_FIELD_MASK_EXTRACTU(ic->tme_sparc32_ireg_psr, TME_SPARC32_PSR_CWP);
  cwp_first += TME_SPARC_NWINDOWS(ic) - 1;
  cwp_first %= TME_SPARC_NWINDOWS(ic);
  cwp = cwp_first;
  do {
    for (reg_i = 0; reg_i < 8; reg_i++) {
      for (reg_base = 24; reg_base > 8; reg_base -= 8) {
	
	ireg = reg_base + reg_i + TME_SPARC_CWP_OFFSET(cwp);
	if (ireg > (TME_SPARC_CWP_OFFSET(TME_SPARC_NWINDOWS(ic)) + 7)) {
	  ireg -= TME_SPARC_CWP_OFFSET(TME_SPARC_NWINDOWS(ic));
	}

	fprintf(stderr,
		"w%u.%%%c%u[%p] = 0x%08x ",
		cwp,
		(reg_base == 24
		 ? 'i'
		 : 'l'),
		reg_i,
		&ic->tme_sparc_ireg_uint32(ireg),
		ic->tme_sparc_ireg_uint32(ireg));
      }
      fprintf(stderr, "\n");
    }
    cwp--;
    cwp %= TME_SPARC_NWINDOWS(ic);
  } while (cwp != cwp_first);

  /* dump out the global registers and the current window's output
     registers: */
  cwp = TME_FIELD_MASK_EXTRACTU(ic->tme_sparc32_ireg_psr, TME_SPARC32_PSR_CWP);
  for (reg_i = 0; reg_i < 8; reg_i++) {

    ireg = reg_i;
    fprintf(stderr,
	    "   %%g%u[%p] = 0x%08x ",
	    ireg,
	    &ic->tme_sparc_ireg_uint32(ireg),
	    ic->tme_sparc_ireg_uint32(ireg));

    ireg = 8 + reg_i + TME_SPARC_CWP_OFFSET(cwp);
    if (ireg > (TME_SPARC_CWP_OFFSET(TME_SPARC_NWINDOWS(ic)) + 7)) {
      ireg -= TME_SPARC_CWP_OFFSET(TME_SPARC_NWINDOWS(ic));
    }

    fprintf(stderr,
	    "w%u.%%o%u[%p] = 0x%08x ",
	    cwp,
	    reg_i,
	    &ic->tme_sparc_ireg_uint32(ireg),
	    ic->tme_sparc_ireg_uint32(ireg));
    fprintf(stderr, "\n");
  }

  /* dump out the PCs: */
  fprintf(stderr, "%%pc = 0x%08x  %%pc_next = 0x%08x  %%pc_next_next = 0x%08x\n",
	  ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC),
	  ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC_NEXT),
	  ic->tme_sparc_ireg_uint32(TME_SPARC_IREG_PC_NEXT_NEXT));

  /* dump out the PSR: */
  fprintf(stderr, "%%psr = 0x%08x", ic->tme_sparc32_ireg_psr);
  fprintf(stderr, "  cwp = %u",
	  TME_FIELD_MASK_EXTRACTU(ic->tme_sparc32_ireg_psr, TME_SPARC32_PSR_CWP));
  fprintf(stderr, "  pil = 0x%x",
	  TME_FIELD_MASK_EXTRACTU(ic->tme_sparc32_ireg_psr, TME_SPARC32_PSR_PIL));
  if (ic->tme_sparc32_ireg_psr & TME_SPARC32_PSR_ET) {
    fprintf(stderr, " ET");
  }
  fprintf(stderr, "  %c",
	  (ic->tme_sparc32_ireg_psr & TME_SPARC32_PSR_S
	   ? 'S'
	   : 'U'));
  fprintf(stderr, "  flags:");
  if (ic->tme_sparc32_ireg_psr & TME_SPARC32_PSR_ICC_N) {
    fprintf(stderr, " N");
  }
  if (ic->tme_sparc32_ireg_psr & TME_SPARC32_PSR_ICC_Z) {
    fprintf(stderr, " Z");
  }
  if (ic->tme_sparc32_ireg_psr & TME_SPARC32_PSR_ICC_V) {
    fprintf(stderr, " V");
  }
  if (ic->tme_sparc32_ireg_psr & TME_SPARC32_PSR_ICC_C) {
    fprintf(stderr, " C");
  }
  fprintf(stderr, "\n");

  /* dump out the instruction and the WIM: */
  fprintf(stderr, "insn = 0x%08x %%wim = 0x%08x\n",
	  ic->_tme_sparc_insn,
	  ic->tme_sparc32_ireg_wim);
}

void
tme_sparc32_dump_memory(struct tme_sparc *ic, tme_uint32_t address, tme_uint32_t resid)
{
  tme_uint32_t address_display;
  struct tme_sparc_tlb *dtlb;
  tme_memory_atomic_flag_t tlb_busy_old;
  const tme_shared tme_uint8_t *memory;
  tme_uint32_t count;
  tme_uint32_t byte_i;

  /* we always display aligned rows: */
  address_display = address & (((tme_uint32_t) 0) - (sizeof(tme_uint32_t) * 2));
  resid += (address - address_display);

  /* while we have memory to dump: */
  for (; resid > 0; ) {

    /* get the DTLB entry, and busy it if it isn't already: */
    dtlb = TME_SPARC_DTLB_ENTRY(ic, address_display);
    tlb_busy_old = dtlb->tme_sparc_tlb_bus_tlb.tme_bus_tlb_busy;
    dtlb->tme_sparc_tlb_bus_tlb.tme_bus_tlb_busy = TRUE;

    /* read more data: */
    count = TME_MIN(resid, sizeof(tme_uint32_t) * 2);
    memory
      = tme_sparc32_load(ic,
			 address_display,
			 (sizeof(tme_uint32_t) * 2));

    /* restore the DTLB busy flag: */
    dtlb->tme_sparc_tlb_bus_tlb.tme_bus_tlb_busy = tlb_busy_old;

    /* display the row: */
    fprintf(stderr, "0x%08x ", address_display);
    for (byte_i = 0;
	 byte_i < count;
	 byte_i++, address_display++) {
      if (address_display < address) {
	fprintf(stderr, "   ");
      }
      else {
	fprintf(stderr, " %02x",
		memory[address_display]);
	address++;
      }
      resid--;
    }
    fputc('\n', stderr);
  }
}
#endif /* 1 */
