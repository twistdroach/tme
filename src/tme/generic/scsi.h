/* $Id: scsi.h,v 1.1 2003/07/29 18:12:29 fredette Exp $ */

/* tme/generic/scsi.h - header file for generic SCSI support: */

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

#ifndef _TME_GENERIC_SCSI_H
#define _TME_GENERIC_SCSI_H

#include <tme/common.h>
_TME_RCSID("$Id: scsi.h,v 1.1 2003/07/29 18:12:29 fredette Exp $");

/* includes: */
#include <tme/element.h>

/* macros: */

/* the SCSI control signals.  the parity signals are really data
   signals, but it's easiest to keep them here: */
#define TME_SCSI_SIGNAL_BSY		TME_BIT(0)
#define TME_SCSI_SIGNAL_SEL		TME_BIT(1)
#define TME_SCSI_SIGNAL_C_D		TME_BIT(2)
#define TME_SCSI_SIGNAL_I_O		TME_BIT(3)
#define TME_SCSI_SIGNAL_MSG		TME_BIT(4)
#define TME_SCSI_SIGNAL_REQ		TME_BIT(5)
#define TME_SCSI_SIGNAL_ACK		TME_BIT(6)
#define TME_SCSI_SIGNAL_ATN		TME_BIT(7)
#define TME_SCSI_SIGNAL_RST		TME_BIT(8)
#define TME_SCSI_SIGNAL_DBP		TME_BIT(9)
#define TME_SCSI_SIGNAL_DBP1		TME_BIT(10)

/* this gets the current SCSI bus information transfer phase from a
   set of control signals: */
#define TME_SCSI_PHASE(c)		\
  ((c) & (TME_SCSI_SIGNAL_MSG		\
	  | TME_SCSI_SIGNAL_C_D		\
	  | TME_SCSI_SIGNAL_I_O))

/* the SCSI bus information transfer phases.  these are always tied to
   definite configurations of SCSI control signals after selection: */
#define TME_SCSI_PHASE_DATA_OUT		(0)
#define TME_SCSI_PHASE_DATA_IN		(TME_SCSI_SIGNAL_I_O)
#define TME_SCSI_PHASE_COMMAND		(TME_SCSI_SIGNAL_C_D)
#define TME_SCSI_PHASE_STATUS		(TME_SCSI_SIGNAL_C_D	\
					 | TME_SCSI_SIGNAL_I_O)
#define TME_SCSI_PHASE_MESSAGE_OUT	(TME_SCSI_SIGNAL_MSG	\
					 | TME_SCSI_SIGNAL_C_D)
#define TME_SCSI_PHASE_MESSAGE_IN	(TME_SCSI_SIGNAL_MSG	\
					 | TME_SCSI_SIGNAL_C_D	\
					 | TME_SCSI_SIGNAL_I_O)

/* the SCSI DMA flags: */
#define TME_SCSI_DMA_WIDTH		(0x03)
#define  TME_SCSI_DMA_8BIT		(0x00)
#define  TME_SCSI_DMA_16BIT		(0x01)
#define TME_SCSI_DMA_PARITY		(0x04)

/* the SCSI data signal routing: */
#define TME_SCSI_DATA_OUT_IMM		(0)
#define TME_SCSI_DATA_OUT_DMA		(1)
#define TME_SCSI_DATA_OUT_DMA_ADVANCE	(2)
#define TME_SCSI_DATA_IN_DMA		(3)
#define TME_SCSI_DATA_IN_DMA_ADVANCE	(4)

/* the predefined SCSI sequence types: */
#define TME_SCSI_SEQUENCE_ARBITRATE_HALF		(1)
#define TME_SCSI_SEQUENCE_ARBITRATE_FULL		(2)
#define TME_SCSI_SEQUENCE_SELECT			(3)
#define TME_SCSI_SEQUENCE_RESELECT			(4)
#define TME_SCSI_SEQUENCE_ARBITRATE_SELECT		(5)
#define TME_SCSI_SEQUENCE_ARBITRATE_RESELECT		(6)
#define TME_SCSI_SEQUENCE_INFO_DMA_INITIATOR		(7)
#define TME_SCSI_SEQUENCE_INFO_DMA_TARGET		(8)
#define TME_SCSI_SEQUENCE_WAIT_SELECT_HALF		(9)
#define TME_SCSI_SEQUENCE_WAIT_SELECT_FULL		(10)
#define TME_SCSI_SEQUENCE_WAIT_SELECT_OR_RESELECT_HALF	(11)
#define TME_SCSI_SEQUENCE_WAIT_SELECT_OR_RESELECT_FULL	(12)
#define TME_SCSI_SEQUENCE_WAIT_CHANGE			(13)

/* types: */

/* the SCSI control signals: */
typedef tme_uint32_t tme_scsi_control_t;

/* the SCSI data signals: */
typedef tme_uint32_t tme_scsi_data_t;

/* a SCSI sequence step: */
struct tme_scsi_sequence {

  /* the amount of time, in microseconds, to delay before asserting
     signals in this sequence step.  TME_SCSI_SEQUENCE_TIME_ARBITRATE
     is a special value indicating that this is the first step in
     arbitration: */
  unsigned long tme_scsi_sequence_delay_pre;

  /* the SCSI control signals to assert in this sequence step: */
  tme_scsi_control_t tme_scsi_sequence_controls;

  /* the SCSI data signals to assert in this sequence step.  the
     tme_scsi_sequence_data member is one of the TME_SCSI_DATA_
     identifiers: */
  unsigned int tme_scsi_sequence_data;
  tme_scsi_data_t tme_scsi_sequence_data_imm;

  /* the amount of time, in microseconds, to delay after signals have
     been asserted before sampling the bus: */
  unsigned long tme_scsi_sequence_delay_post;

  /* the amount of time, in microseconds, to sample the bus.
     TME_SCSI_SEQUENCE_TIME_INFINITE is a special value indicating
     that this should wait forever: */
  unsigned long tme_scsi_sequence_sample_time;
     
  /* the SCSI control and data signals masks to sample and match: */
  tme_scsi_control_t tme_scsi_sequence_controls_mask;
  tme_scsi_control_t tme_scsi_sequence_controls_match;
  tme_scsi_data_t tme_scsi_sequence_data_mask;
  tme_scsi_data_t tme_scsi_sequence_data_match;

  /* if the match succeeds, this is the next sequence step: */
  _tme_const struct tme_scsi_sequence *tme_scsi_sequence_next;
};

/* a SCSI DMA buffer: */
struct tme_scsi_dma {

  /* the flags: */
  unsigned char tme_scsi_dma_flags;

  /* how many bytes remain in the buffer: */
  unsigned long tme_scsi_dma_resid;

  /* the input buffer.  this is used for an initiator when I/O is
     true, and for a target when I/O is false: */
  tme_uint8_t *tme_scsi_dma_in;

  /* the output buffer.  this is used for an initiator when I/O is
     false, and for a target when I/O is true: */
  _tme_const tme_uint8_t *tme_scsi_dma_out;

  /* any synchronous transfer REQ/ACK offset.  zero implies 
     asynchronous transfer: */
  unsigned short tme_scsi_dma_sync_offset;

  /* any synchronous transfer period, in nanoseconds: */
  unsigned short tme_scsi_dma_sync_period;
};

/* a SCSI connection: */
struct tme_scsi_connection {

  /* the generic connection side: */
  struct tme_connection tme_scsi_connection;

  /* this is called for a SCSI bus cycle: */
  int (*tme_scsi_connection_cycle) _TME_P((struct tme_scsi_connection *,
					   tme_scsi_control_t,
					   tme_scsi_data_t,
					   _tme_const struct tme_scsi_sequence *,
					   struct tme_scsi_dma *));

  /* this is called to get a predefined sequence: */
  _tme_const struct tme_scsi_sequence *(*tme_scsi_connection_sequence_get)
       _TME_P((struct tme_scsi_connection *,
	       unsigned int, ...));
};

/* prototypes: */
int tme_scsi_connection_score _TME_P((struct tme_connection *, unsigned int *));
int tme_scsi_id_parse _TME_P((const char *));
#define tme_scsi_lun_parse tme_scsi_id_parse

#endif /* !_TME_GENERIC_SCSI_H */
