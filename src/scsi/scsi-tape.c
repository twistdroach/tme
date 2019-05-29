/* $Id: scsi-tape.c,v 1.3 2003/10/16 02:48:25 fredette Exp $ */

/* scsi/scsi-tape.c - implementation of SCSI tape emulation: */

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
_TME_RCSID("$Id: scsi-tape.c,v 1.3 2003/10/16 02:48:25 fredette Exp $");

/* includes: */
#include <tme/scsi/scsi-tape.h>
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else  /* HAVE_STDARG_H */
#include <varargs.h>
#endif /* HAVE_STDARG_H */

/* macros: */

/* globals: */

/* the list of tapes that we emulate: */
const struct {

  /* the type name: */
  const char *_tme_scsi_tape_list_type;

  /* the initialization function: */
  int (*_tme_scsi_tape_list_init) _TME_P((struct tme_scsi_tape *));
} _tme_scsi_tape_list[] = {
  
#if 0
  /* the generic TME SCSI-2 tape: */
  { "tme-scsi-2", tme_scsi_tape_tme_init },
#endif
  
  /* the Emulex MT02 emulation: */
  { "emulex-mt02", tme_scsi_tape_emulexmt02_init },
};

/* this is the LUN addresser for LUN-aware tape devices: */
int
tme_scsi_tape_address_lun_aware(struct tme_scsi_device *scsi_device)
{
  struct tme_scsi_tape *scsi_tape;
  struct tme_scsi_tape_connection *conn_scsi_tape;
  struct tme_scsi_device_sense *sense;
  int lun;

  /* recover our data structure: */
  scsi_tape = (struct tme_scsi_tape *) scsi_device;

  /* if an IDENTIFY message was sent, use that LUN: */
  lun = scsi_device->tme_scsi_device_addressed_lun;

  /* otherwise, get the LUN from bits 5-7 of the second
     CDB byte: */
  if (lun < 0) {
    lun = (scsi_device->tme_scsi_device_cdb[1] >> 5);
    scsi_device->tme_scsi_device_addressed_lun = lun;
  }
  
  /* get this LUN's tape connection: */
  conn_scsi_tape = scsi_tape->tme_scsi_tape_connections[lun];

  /* if this LUN is not defined, and this isn't a REQUEST SENSE
     command: */ 
  if (!(scsi_device->tme_scsi_device_luns
	& TME_BIT(lun))
      && (scsi_device->tme_scsi_device_cdb[0]
	  != TME_SCSI_CDB_REQUEST_SENSE)) {

    /* form the ILLEGAL REQUEST sense: */
    sense = &scsi_device->tme_scsi_device_sense[lun];
    sense->tme_scsi_device_sense_data[2] = 0x05;
  }

  /* otherwise, this LUN is defined.  an INQUIRY or REQUEST SENSE
     command is always allowed for a defined LUN: */
  else if ((scsi_device->tme_scsi_device_cdb[0]
	    == TME_SCSI_CDB_INQUIRY)
	   || (scsi_device->tme_scsi_device_cdb[0]
	       == TME_SCSI_CDB_REQUEST_SENSE)) {
    sense = NULL;
  }

  /* otherwise, if the tape at this LUN has an attention condition: */
  else if (conn_scsi_tape->tme_scsi_tape_connection_flags
	   & TME_SCSI_TAPE_FLAG_ATTENTION) {

    /* clear the attention condition: */
    conn_scsi_tape->tme_scsi_tape_connection_flags
      &= ~TME_SCSI_TAPE_FLAG_ATTENTION;

    /* form the UNIT ATTENTION sense: */
    sense = &scsi_device->tme_scsi_device_sense[lun];
    sense->tme_scsi_device_sense_data[2] = 0x06;
  }

  /* otherwise, if the tape at this LUN is not loaded: */
  else if (!(conn_scsi_tape->tme_scsi_tape_connection_flags
	     & TME_SCSI_TAPE_FLAG_LOADED)) {

    /* form the NOT READY sense: */
    sense = &scsi_device->tme_scsi_device_sense[lun];
    sense->tme_scsi_device_sense_data[2] = 0x02;
  }

  /* otherwise, this command is okay: */
  else {
    sense = NULL;
  }

  /* if addressing this LUN caused some sense: */
  if (sense != NULL) {

    /* this target must support extended sense: */
    assert (!scsi_device->tme_scsi_device_sense_no_extended);

    /* the error class and error code: */
    sense->tme_scsi_device_sense_data[0]
      = 0x70;

    /* the additional sense length: */
    sense->tme_scsi_device_sense_data[7]
      = 0x00;

    sense->tme_scsi_device_sense_valid
      = TRUE;

    /* return the CHECK CONDITION status: */
    tme_scsi_device_target_do_smf(scsi_device,
				  TME_SCSI_STATUS_CHECK_CONDITION,
				  TME_SCSI_MSG_CMD_COMPLETE);
    return (EINVAL);
  }

  return (TME_OK);
}

/* this is the LUN addresser for LUN-unaware devices: */
int
tme_scsi_tape_address_lun_unaware(struct tme_scsi_device *scsi_device)
{

  /* we always force a LUN of zero: */
  scsi_device->tme_scsi_device_addressed_lun = 0;

  return (tme_scsi_tape_address_lun_aware(scsi_device));
}

/* this determines the status of a tape READ or WRITE command: */
tme_uint8_t
tme_scsi_tape_xfer_status(struct tme_scsi_tape *scsi_tape,
			  int flags,
			  unsigned long count_xfer_got)
{
  int lun;
  tme_uint8_t *cdb;
  tme_uint8_t status;
  unsigned long count_xfer_wanted;
  struct tme_scsi_device_sense *sense;

  /* assume that this command completed successfully: */
  status = TME_SCSI_STATUS_GOOD;

  /* if there are some tape check condition flags: */
  if (flags & ~TME_TAPE_FLAG_FIXED) {

    /* there is a condition to check: */
    status = TME_SCSI_STATUS_CHECK_CONDITION;

    /* get the addressed LUN: */
    lun = scsi_tape->tme_scsi_tape_device.tme_scsi_device_addressed_lun;

    /* get the original transfer length: */
    cdb = &scsi_tape->tme_scsi_tape_device.tme_scsi_device_cdb[0];
    count_xfer_wanted = cdb[2];
    count_xfer_wanted = (count_xfer_wanted << 8) | cdb[3];
    count_xfer_wanted = (count_xfer_wanted << 8) | cdb[4];

    /* set the sense: */
    sense
      = &scsi_tape->tme_scsi_tape_device.tme_scsi_device_sense[lun];
    
    /* the Valid, Error Class, and Error Code values: */
    sense->tme_scsi_device_sense_data[0]
      = (0x80 | 0x70);

    /* the Filemark, EOM, ILI, and Sense Key (NO SENSE) values: */
    sense->tme_scsi_device_sense_data[2]
      = (((flags & TME_TAPE_FLAG_MARK)
	  ? 0x80
	  : 0x00)
	 | ((flags & TME_TAPE_FLAG_EOM)
	    ? 0x40
	    : 0x00)
	 | ((flags & TME_TAPE_FLAG_ILI)
	    ? 0x20
	    : 0x00)
	 | 0x00);

    /* set the Information Bytes (for a tape, the residue): */
    count_xfer_wanted -= count_xfer_got;
    sense->tme_scsi_device_sense_data[3]
      = (count_xfer_wanted >> 24) & 0xff;
    sense->tme_scsi_device_sense_data[4]
      = (count_xfer_wanted >> 16) & 0xff;
    sense->tme_scsi_device_sense_data[5]
      = (count_xfer_wanted >>  8) & 0xff;
    sense->tme_scsi_device_sense_data[6]
      = (count_xfer_wanted >>  0) & 0xff;

    /* there are no additional bytes: */
    sense->tme_scsi_device_sense_data[7]
      = 0x00;
    
    /* this sense is valid: */
    sense->tme_scsi_device_sense_valid
      = TRUE;
  }

  /* done: */
  return (status);
}

/* this finishes a WRITE command: */
_TME_SCSI_DEVICE_PHASE_DECL(tme_scsi_tape_target_do_write)
{
  struct tme_scsi_tape *scsi_tape;
  struct tme_scsi_tape_connection *conn_scsi_tape;
  struct tme_tape_connection *conn_tape;
  int lun;
  unsigned long count;
  int flags;
  tme_uint8_t status;
  int rc;

  /* recover our tape: */
  scsi_tape = (struct tme_scsi_tape *) scsi_device;

  /* get the addressed LUN: */
  lun = scsi_device->tme_scsi_device_addressed_lun;

  /* get the tape connection: */
  conn_scsi_tape
    = scsi_tape->tme_scsi_tape_connections[lun];
  conn_tape
    = ((struct tme_tape_connection *)
       conn_scsi_tape->tme_scsi_tape_connection.tme_tape_connection.tme_connection_other);

  /* release the buffer: */
  rc
    = ((*conn_tape->tme_tape_connection_release)
       (conn_tape,
	&flags,
	&count));
  assert (rc == TME_OK);

  /* get the status: */
  status
    = ((*scsi_tape->tme_scsi_tape_xfer_status)
       (scsi_tape,
	flags,
	count));;

  /* finish the command: */
  tme_scsi_device_target_do_smf(scsi_device,
				status,
				TME_SCSI_MSG_CMD_COMPLETE);
}

/* this implements the tape Group 0 READ and WRITE commands: */
void
tme_scsi_tape_cdb_xfer0(struct tme_scsi_device *scsi_device,
			int read)
{
  struct tme_scsi_tape *scsi_tape;
  struct tme_scsi_tape_connection *conn_scsi_tape;
  struct tme_tape_connection *conn_tape;
  int lun;
  tme_uint8_t *cdb;
  unsigned long count_xfer;
  int flags;
  tme_uint8_t status;
  int rc;

  /* recover our tape: */
  scsi_tape = (struct tme_scsi_tape *) scsi_device;

  /* get the addressed LUN: */
  lun = scsi_device->tme_scsi_device_addressed_lun;

  /* get the tape connection: */
  conn_scsi_tape
    = scsi_tape->tme_scsi_tape_connections[lun];
  conn_tape
    = ((struct tme_tape_connection *)
       conn_scsi_tape->tme_scsi_tape_connection.tme_tape_connection.tme_connection_other);

  cdb = &scsi_device->tme_scsi_device_cdb[0];

  /* get the fixed bit: */
  flags = (cdb[1] & 0x01) * TME_TAPE_FLAG_FIXED;

  /* get the transfer length: */
  count_xfer = cdb[2];
  count_xfer = (count_xfer << 8) | cdb[3];
  count_xfer = (count_xfer << 8) | cdb[4];
  
  /* if this is a read: */
  if (read) {

    /* get the tape buffer: */
    rc
      = ((*conn_tape->tme_tape_connection_read)
	 (conn_tape,
	  &flags,
	  &count_xfer,
	  &scsi_device->tme_scsi_device_dma.tme_scsi_dma_resid,
	  &scsi_device->tme_scsi_device_dma.tme_scsi_dma_out));
    scsi_device->tme_scsi_device_dma.tme_scsi_dma_in = NULL;

    /* get the status: */
    status
      = ((*scsi_tape->tme_scsi_tape_xfer_status)
	 (scsi_tape,
	  flags,
	  count_xfer));
    
    /* finish the command: */
    tme_scsi_device_target_do_dsmf(scsi_device,
				   status,
				   TME_SCSI_MSG_CMD_COMPLETE);
  }
  else {

    /* get the tape buffer: */
    rc
      = ((*conn_tape->tme_tape_connection_write)
	 (conn_tape,
	  flags,
	  count_xfer,
	  &scsi_device->tme_scsi_device_dma.tme_scsi_dma_resid,
	  &scsi_device->tme_scsi_device_dma.tme_scsi_dma_in));
    scsi_device->tme_scsi_device_dma.tme_scsi_dma_out = NULL;

    /* enter the DATA OUT phase to transfer all of the data to be
       written: */
    /* XXX when in fixed-block mode, we should go a block at a time,
       so we can report errors as soon as they happen: */
    tme_scsi_device_target_phase(scsi_device,
				 TME_SCSI_SIGNAL_BSY
				 | TME_SCSI_PHASE_DATA_OUT);

    /* when the DATA OUT phase is done, we'll write the data end
       enter the STATUS phase: */
    scsi_device->tme_scsi_device_phase
      = tme_scsi_tape_target_do_write;
  }

  /* if we couldn't get the tape buffer: */
  if (rc != TME_OK) {
    
    /* XXX we should return MEDIUM ERROR or HARDWARE ERROR sense here: */
    abort();
  }
}

/* this implements the tape REWIND command: */
_TME_SCSI_DEVICE_CDB_DECL(tme_scsi_tape_cdb_rewind)
{
  struct tme_scsi_tape *scsi_tape;
  struct tme_scsi_tape_connection *conn_scsi_tape;
  struct tme_tape_connection *conn_tape;
  int lun;
  int rc;

  /* recover our tape: */
  scsi_tape = (struct tme_scsi_tape *) scsi_device;

  /* get the addressed LUN: */
  lun = scsi_device->tme_scsi_device_addressed_lun;

  /* get the tape connection: */
  conn_scsi_tape
    = scsi_tape->tme_scsi_tape_connections[lun];
  conn_tape
    = ((struct tme_tape_connection *)
       conn_scsi_tape->tme_scsi_tape_connection.tme_tape_connection.tme_connection_other);

  /* call out a REWIND control: */
  rc = 
    ((*conn_tape->tme_tape_connection_control)
     (conn_tape,
      TME_TAPE_CONTROL_REWIND));
  assert (rc == TME_OK);

  /* finish the command: */
  tme_scsi_device_target_do_smf(scsi_device,
				TME_SCSI_STATUS_GOOD,
				TME_SCSI_MSG_CMD_COMPLETE);
}

/* this implements the tape READ BLOCK LIMITS command: */
_TME_SCSI_DEVICE_CDB_DECL(tme_scsi_tape_cdb_block_limits)
{
  abort();
}

/* this implements the tape Group 0 READ command: */
_TME_SCSI_DEVICE_CDB_DECL(tme_scsi_tape_cdb_read0)
{
  tme_scsi_tape_cdb_xfer0(scsi_device, TRUE);
}

/* this implements the tape Group 0 WRITE command: */
_TME_SCSI_DEVICE_CDB_DECL(tme_scsi_tape_cdb_write0)
{
  tme_scsi_tape_cdb_xfer0(scsi_device, FALSE);
}

/* this implements the tape INQUIRY command: */
_TME_SCSI_DEVICE_CDB_DECL(tme_scsi_tape_cdb_inquiry)
{
  int lun;
  struct tme_scsi_device_inquiry inquiry;
  tme_uint8_t *data;
  
  /* get the active LUN: */
  lun = scsi_device->tme_scsi_device_addressed_lun;

  /* this is a sequential-access device: */
  inquiry.tme_scsi_device_inquiry_type = TME_SCSI_TYPE_TAPE;

  /* if this LUN is defined: */
  inquiry.tme_scsi_device_inquiry_lun_state
    = ((scsi_device->tme_scsi_device_luns
	& TME_BIT(lun))
       ? TME_SCSI_LUN_PRESENT
       : TME_SCSI_LUN_NOT_PRESENT);

  /* the device type qualifier: */
  inquiry.tme_scsi_device_inquiry_type_qualifier = 0x00;

  /* nonzero iff the LUN is removable: */
  inquiry.tme_scsi_device_inquiry_lun_removable = TRUE;

  /* the various standards versions: */
  inquiry.tme_scsi_device_inquiry_std_ansi = 2;
  inquiry.tme_scsi_device_inquiry_std_ecma = 2;
  inquiry.tme_scsi_device_inquiry_std_iso = 2;

  /* the response format: */
  inquiry.tme_scsi_device_response_format = TME_SCSI_FORMAT_CCS;

  /* make the inquiry data: */
  data
    = tme_scsi_device_make_inquiry_data(scsi_device,
					&inquiry);
  scsi_device->tme_scsi_device_dma.tme_scsi_dma_resid
    = (data
       - scsi_device->tme_scsi_device_dma.tme_scsi_dma_out);
  
  /* finish the command: */
  tme_scsi_device_target_do_dsmf(scsi_device,
				 TME_SCSI_STATUS_GOOD,
				 TME_SCSI_MSG_CMD_COMPLETE);
}

/* this implements the tape WRITE MARKS command: */
_TME_SCSI_DEVICE_CDB_DECL(tme_scsi_tape_cdb_write_marks)
{
  abort();
}

/* this implements the tape SPACE command: */
_TME_SCSI_DEVICE_CDB_DECL(tme_scsi_tape_cdb_space)
{
  struct tme_scsi_tape *scsi_tape;
  struct tme_scsi_tape_connection *conn_scsi_tape;
  struct tme_tape_connection *conn_tape;
  int lun;
  tme_uint8_t *cdb;
  tme_int32_t count;
  int rc;

  /* recover our tape: */
  scsi_tape = (struct tme_scsi_tape *) scsi_device;

  /* get the addressed LUN: */
  lun = scsi_device->tme_scsi_device_addressed_lun;

  /* get the tape connection: */
  conn_scsi_tape
    = scsi_tape->tme_scsi_tape_connections[lun];
  conn_tape
    = ((struct tme_tape_connection *)
       conn_scsi_tape->tme_scsi_tape_connection.tme_tape_connection.tme_connection_other);

  cdb = &scsi_device->tme_scsi_device_cdb[0];

  /* get the signed count: */
  count = ((tme_int8_t *) cdb)[2];
  count = (count << 8) | cdb[3];
  count = (count << 8) | cdb[4];

  /* dispatch on the SPACE code: */
  switch (cdb[1] & 0x03) {

    /* blocks: */
  case 0x00:
    abort();

    /* filemarks: */
  case 0x01:

    /* call out a MARK_SKIPF or MARK_SKIPR control: */
    rc = 
      (count < 0
       ? ((*conn_tape->tme_tape_connection_control)
	  (conn_tape,
	   TME_TAPE_CONTROL_MARK_SKIPR,
	   (unsigned int) (-count)))
       : ((*conn_tape->tme_tape_connection_control)
	  (conn_tape,
	   TME_TAPE_CONTROL_MARK_SKIPF,
	   (unsigned int) count)));
    assert (rc == TME_OK);
    break;

    /* sequential filemarks: */
  case 0x02:
    abort();

    /* physical end-of-data: */
  case 0x03:
    abort();
  }

  /* finish the command: */
  tme_scsi_device_target_do_smf(scsi_device,
				TME_SCSI_STATUS_GOOD,
				TME_SCSI_MSG_CMD_COMPLETE);
}

/* this implements the tape MODE SELECT command: */
_TME_SCSI_DEVICE_CDB_DECL(tme_scsi_tape_cdb_mode_select)
{
  abort();
}

/* this implements the tape MODE SENSE command: */
_TME_SCSI_DEVICE_CDB_DECL(tme_scsi_tape_cdb_mode_sense)
{
  abort();
}

/* this implements the tape LOAD/UNLOAD command: */
_TME_SCSI_DEVICE_CDB_DECL(tme_scsi_tape_cdb_load_unload)
{
  /* XXX TBD */

  /* finish the command: */
  tme_scsi_device_target_do_smf(scsi_device,
				TME_SCSI_STATUS_GOOD,
				TME_SCSI_MSG_CMD_COMPLETE);
}

/* this implements the tape PREVENT/ALLOW command: */
_TME_SCSI_DEVICE_CDB_DECL(tme_scsi_tape_cdb_prevent_allow)
{
  /* XXX TBD */

  /* finish the command: */
  tme_scsi_device_target_do_smf(scsi_device,
				TME_SCSI_STATUS_GOOD,
				TME_SCSI_MSG_CMD_COMPLETE);
}

/* the tape control handler: */
#ifdef HAVE_STDARG_H
static int _tme_scsi_tape_control(struct tme_tape_connection *conn_tape,
				  unsigned int control,
				  ...)
#else  /* HAVE_STDARG_H */
static int _tme_scsi_tape_control(conn_tape, control, va_alist)
     struct tme_tape_connection *conn_tape;
     unsigned int control;
     va_dcl
#endif /* HAVE_STDARG_H */
{
  struct tme_scsi_tape *scsi_tape;
  struct tme_scsi_tape_connection *conn_scsi_tape;
  va_list control_args;

  /* recover our data structures: */
  conn_scsi_tape = (struct tme_scsi_tape_connection *) conn_tape;
  scsi_tape = (struct tme_scsi_tape *) conn_tape->tme_tape_connection.tme_connection_element->tme_element_private;

  /* lock the mutex: */
  tme_mutex_lock(&scsi_tape->tme_scsi_tape_mutex);

  /* start the variable arguments: */
#ifdef HAVE_STDARG_H
  va_start(control_args, control);
#else  /* HAVE_STDARG_H */
  va_start(control_args);
#endif /* HAVE_STDARG_H */

  /* dispatch on the sequence type: */
  switch (control) {

  case TME_TAPE_CONTROL_LOAD:
    /* a tape has been loaded: */
    conn_scsi_tape->tme_scsi_tape_connection_flags
      = (conn_scsi_tape->tme_scsi_tape_connection_flags
	 | (TME_SCSI_TAPE_FLAG_LOADED
	    | TME_SCSI_TAPE_FLAG_ATTENTION));
    break;

  case TME_TAPE_CONTROL_UNLOAD:
    /* a tape has been unloaded: */
    conn_scsi_tape->tme_scsi_tape_connection_flags
      = ((conn_scsi_tape->tme_scsi_tape_connection_flags
	  & ~TME_SCSI_TAPE_FLAG_LOADED)
	 | TME_SCSI_TAPE_FLAG_ATTENTION);
    break;

    abort();

  case TME_TAPE_CONTROL_DENSITY_GET:
    abort();

  case TME_TAPE_CONTROL_DENSITY_SET:
    abort();

  case TME_TAPE_CONTROL_BLOCK_SIZE_GET:
    abort();

  case TME_TAPE_CONTROL_BLOCK_SIZE_SET:
    abort();

  case TME_TAPE_CONTROL_REWIND:
  case TME_TAPE_CONTROL_MARK_WRITE:
  case TME_TAPE_CONTROL_MARK_SKIPF:
  case TME_TAPE_CONTROL_MARK_SKIPR:
  default:
    abort();
  }

  /* end the variable arguments: */
  va_end(control_args);

  /* unlock the mutex: */
  tme_mutex_unlock(&scsi_tape->tme_scsi_tape_mutex);

  return (TME_OK);
}

/* this breaks a connection: */
static int 
_tme_scsi_tape_connection_break(struct tme_connection *conn,
				unsigned int state)
{
  abort();
}

/* this makes a new tape connection: */
static int
_tme_scsi_tape_connection_make(struct tme_connection *conn,
			       unsigned int state)
{
  struct tme_scsi_tape *scsi_tape;
  struct tme_scsi_tape_connection *conn_scsi_tape;
  struct tme_tape_connection *conn_tape;
  int lun;
  int loaded;
  int rc;

  /* both sides must be tape connections: */
  assert (conn->tme_connection_type == TME_CONNECTION_TAPE);
  assert (conn->tme_connection_other->tme_connection_type == TME_CONNECTION_TAPE);

  /* recover our data structures: */
  scsi_tape = conn->tme_connection_element->tme_element_private;
  conn_scsi_tape = (struct tme_scsi_tape_connection *) conn;

  /* we're always set up to answer calls across the connection,
     so we only have to do work when the connection has gone full,
     namely taking the other side of the connection: */
  if (state == TME_CONNECTION_FULL) {

    /* lock the mutex: */
    tme_mutex_lock(&scsi_tape->tme_scsi_tape_mutex);

    /* make this tape connection: */
    lun = conn_scsi_tape->tme_scsi_tape_connection_lun;
    assert (scsi_tape->tme_scsi_tape_connections[lun]
	    == NULL);
    scsi_tape->tme_scsi_tape_connections[lun]
      = conn_scsi_tape;
    scsi_tape->tme_scsi_tape_device.tme_scsi_device_luns
      |= (1 << lun);

    /* call any type-specific connection function: */
    if (scsi_tape->tme_scsi_tape_connected != NULL) {
      (*scsi_tape->tme_scsi_tape_connected)(scsi_tape, lun);
    }

    /* call out a LOAD control to see if the tape is currently loaded: */
    conn_tape
      = ((struct tme_tape_connection *)
	 conn_scsi_tape->tme_scsi_tape_connection.tme_tape_connection.tme_connection_other);
    rc = 
      ((*conn_tape->tme_tape_connection_control)
       (conn_tape,
	TME_TAPE_CONTROL_LOAD,
	&loaded));
    assert (rc == TME_OK);
    conn_scsi_tape->tme_scsi_tape_connection_flags
      = (loaded
	 ? (TME_SCSI_TAPE_FLAG_LOADED
	    | TME_SCSI_TAPE_FLAG_ATTENTION)
	 : 0);

    /* unlock the mutex: */
    tme_mutex_unlock(&scsi_tape->tme_scsi_tape_mutex);
  }
  
  return (TME_OK);
}

/* this returns the new connections possible: */
static int
_tme_scsi_tape_connections_new(struct tme_element *element,
			       const char * const *args,
			       struct tme_connection **_conns,
			       char **_output)
{
  struct tme_scsi_tape *scsi_tape;
  struct tme_scsi_tape_connection *conn_scsi_tape;
  struct tme_tape_connection *conn_tape;
  struct tme_connection *conn;
  int lun;
  int arg_i;
  int usage;
  int rc;

  /* recover our device: */
  scsi_tape = (struct tme_scsi_tape *) element->tme_element_private;

  /* check our arguments: */
  lun = -1;
  arg_i = 1;
  usage = FALSE;

  /* loop reading our arguments: */
  for (;;) {

    /* the LUN to attach to: */
    if (TME_ARG_IS(args[arg_i + 0], "lun")
	&& lun < 0
	&& (lun = tme_scsi_lun_parse(args[arg_i + 1])) >= 0
	&& lun < TME_SCSI_DEVICE_LUN_COUNT
	&& scsi_tape->tme_scsi_tape_connections[lun] == NULL) {
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
			    "%s %s [ lun %s ]",
			    _("usage:"),
			    args[0],
			    _("LOGICAL-UNIT"));
    return (EINVAL);
  }

  /* return any SCSI device SCSI connection: */
  rc = tme_scsi_device_connections_new(element,
				       args,
				       _conns,
				       _output);
  if (rc != TME_OK) {
    return (rc);
  }

  /* if we don't have a particular lun, see if there is a free lun.
     if there isn't a free lun, return now: */
  if (lun < 0) {
    for (lun = 0;
	 lun < TME_SCSI_DEVICE_LUN_COUNT;
	 lun++) {
      if (scsi_tape->tme_scsi_tape_connections[lun] == NULL) {
	break;
      }
    }
    if (lun == TME_SCSI_DEVICE_LUN_COUNT) {
      return (TME_OK);
    }
  }

  /* create our side of a tape connection: */
  conn_scsi_tape = tme_new0(struct tme_scsi_tape_connection, 1);
  conn_tape = &conn_scsi_tape->tme_scsi_tape_connection;
  conn = &conn_tape->tme_tape_connection;

  /* fill in the generic connection: */
  conn->tme_connection_next = *_conns;
  conn->tme_connection_type = TME_CONNECTION_TAPE;
  conn->tme_connection_score = tme_tape_connection_score;
  conn->tme_connection_make = _tme_scsi_tape_connection_make;
  conn->tme_connection_break = _tme_scsi_tape_connection_break;

  /* fill in the tape connection: */
  conn_tape->tme_tape_connection_control = _tme_scsi_tape_control;

  /* fill in the internal tape connection: */
  conn_scsi_tape->tme_scsi_tape_connection_lun = lun;

  /* return the connection side possibility: */
  *_conns = conn;
  return (TME_OK);
}

/* the new SCSI tape function: */
TME_ELEMENT_SUB_NEW_DECL(tme_scsi,tape) {
  int id;
  const char *tape_type;
  const char *vendor;
  const char *product;
  const char *revision;
  struct tme_scsi_tape *scsi_tape;
  struct tme_scsi_device *scsi_device;
  int arg_i;
  int usage;
  unsigned int tape_list_i;
  int (*tape_init) _TME_P((struct tme_scsi_tape *));
  int rc;

  /* check our arguments: */
  id = -1;
  tape_type = NULL;
  vendor = NULL;
  product = NULL;
  revision = NULL;
  arg_i = 1;
  usage = FALSE;

  /* loop reading our arguments: */
  for (;;) {

    /* the SCSI ID: */
    if (TME_ARG_IS(args[arg_i], "id")
	&& id < 0
	&& (id = tme_scsi_id_parse(args[arg_i + 1])) >= 0) {
      arg_i += 2;
    }

    /* the tape type: */
    else if (TME_ARG_IS(args[arg_i], "type")
	     && tape_type == NULL
	     && args[arg_i + 1] != NULL) {
      tape_type = args[arg_i + 1];
      arg_i += 2;
    }

    /* any inquiry vendor, product, or revision: */
    else if (TME_ARG_IS(args[arg_i], "vendor")
	     && vendor == NULL
	     && args[arg_i + 1] != NULL) {
      vendor = args[arg_i + 1];
      arg_i += 2;
    }
    else if (TME_ARG_IS(args[arg_i], "product")
	     && product == NULL
	     && args[arg_i + 1] != NULL) {
      product = args[arg_i + 1];
      arg_i += 2;
    }
    else if (TME_ARG_IS(args[arg_i], "revision")
	     && revision == NULL
	     && args[arg_i + 1] != NULL) {
      revision = args[arg_i + 1];
      arg_i += 2;
    }

    /* if we've run out of arguments: */
    else if (args[arg_i + 0] == NULL) {

      /* we must have been given an ID and a type: */
      if (id < 0
	  || tape_type == NULL) {
	usage = TRUE;
      }
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
			    "%s %s id %s type %s [ vendor %s ] [ product %s ] [ revision %s ]",
			    _("usage:"),
			    args[0],
			    _("TYPE"),
			    _("ID"),
			    _("VENDOR"),
			    _("PRODUCT"),
			    _("REVISION"));
    return (EINVAL);
  }

  /* make sure that this tape type is known: */
  tape_init = NULL;
  for (tape_list_i = 0;
       tape_list_i < TME_ARRAY_ELS(_tme_scsi_tape_list);
       tape_list_i++) {
    if (!strcmp(_tme_scsi_tape_list[tape_list_i]._tme_scsi_tape_list_type,
		tape_type)) {
      tape_init = _tme_scsi_tape_list[tape_list_i]._tme_scsi_tape_list_init;
      break;
    }
  }
  if (tape_init == NULL) {
    tme_output_append_error(_output, "%s", tape_type);
    return (ENOENT);
  }

  /* start the tape structure: */
  scsi_tape = tme_new0(struct tme_scsi_tape, 1);
  scsi_tape->tme_scsi_tape_element = element;
  scsi_tape->tme_scsi_tape_type = tme_strdup(tape_type);

  /* initialize the generic SCSI device structure: */
  scsi_device = &scsi_tape->tme_scsi_tape_device;
  rc = tme_scsi_device_new(scsi_device, id);
  assert (rc == TME_OK);

  scsi_device->tme_scsi_device_vendor
    = tme_strdup((vendor == NULL)
		 ? "TME"
		 : vendor);
  scsi_device->tme_scsi_device_product
    = tme_strdup((product == NULL)
		 ? "TAPE"
		 : product);
  scsi_device->tme_scsi_device_revision
    = tme_strdup((revision == NULL)
		 ? "0000"
		 : revision);
  
  /* set the commands for sequential-access devices: */
  TME_SCSI_DEVICE_DO_CDB(scsi_device,
			 TME_SCSI_CDB_INQUIRY,
			 tme_scsi_tape_cdb_inquiry);
  TME_SCSI_DEVICE_DO_CDB(scsi_device,
			 TME_SCSI_CDB_TAPE_REWIND,
			 tme_scsi_tape_cdb_rewind);
  TME_SCSI_DEVICE_DO_CDB(scsi_device,
			 TME_SCSI_CDB_TAPE_BLOCK_LIMITS,
			 tme_scsi_tape_cdb_block_limits);
  TME_SCSI_DEVICE_DO_CDB(scsi_device,
			 TME_SCSI_CDB_TAPE_READ0,
			 tme_scsi_tape_cdb_read0);
  TME_SCSI_DEVICE_DO_CDB(scsi_device,
			 TME_SCSI_CDB_TAPE_WRITE0,
			 tme_scsi_tape_cdb_write0);
  TME_SCSI_DEVICE_DO_CDB(scsi_device,
			 TME_SCSI_CDB_TAPE_WRITE_MARKS,
			 tme_scsi_tape_cdb_write_marks);
  TME_SCSI_DEVICE_DO_CDB(scsi_device,
			 TME_SCSI_CDB_TAPE_SPACE,
			 tme_scsi_tape_cdb_space);
  TME_SCSI_DEVICE_DO_CDB(scsi_device,
			 TME_SCSI_CDB_TAPE_MODE_SELECT,
			 tme_scsi_tape_cdb_mode_select);
  TME_SCSI_DEVICE_DO_CDB(scsi_device,
			 TME_SCSI_CDB_TAPE_MODE_SENSE,
			 tme_scsi_tape_cdb_mode_sense);
  TME_SCSI_DEVICE_DO_CDB(scsi_device,
			 TME_SCSI_CDB_TAPE_LOAD_UNLOAD,
			 tme_scsi_tape_cdb_load_unload);
  TME_SCSI_DEVICE_DO_CDB(scsi_device,
			 TME_SCSI_CDB_TAPE_PREVENT_ALLOW,
			 tme_scsi_tape_cdb_prevent_allow);

  /* there is no type-specific connected function: */
  scsi_tape->tme_scsi_tape_connected = NULL;

  /* use the default transfer status function: */
  scsi_tape->tme_scsi_tape_xfer_status
    = tme_scsi_tape_xfer_status;

  /* use the default tape LUN addresser: */
  scsi_device->tme_scsi_device_address_lun
    = tme_scsi_tape_address_lun_aware;

  /* call the type-specific initialization function: */
  rc = (*tape_init)(scsi_tape);
  assert (rc == TME_OK);

  /* fill the element: */
  element->tme_element_private = scsi_tape;
  element->tme_element_connections_new = _tme_scsi_tape_connections_new;

  return (TME_OK);
}
