/* $Id: scsi.c,v 1.1 2003/07/29 18:18:35 fredette Exp $ */

/* generic/scsi.c - generic SCSI implementation support: */

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
_TME_RCSID("$Id: scsi.c,v 1.1 2003/07/29 18:18:35 fredette Exp $");

/* includes: */
#include <tme/generic/scsi.h>
#include <stdlib.h>

/* this scores a SCSI connection: */
int
tme_scsi_connection_score(struct tme_connection *conn, unsigned int *_score)
{
  struct tme_scsi_connection *conn_scsi;
  struct tme_scsi_connection *conn_scsi_other;

  /* both sides must be SCSI connections: */
  assert(conn->tme_connection_type == TME_CONNECTION_SCSI);
  assert(conn->tme_connection_other->tme_connection_type == TME_CONNECTION_SCSI);

  /* you cannot connect a bus to a bus, or a device to a device: */
  conn_scsi
    = (struct tme_scsi_connection *) conn;
  conn_scsi_other 
    = (struct tme_scsi_connection *) conn->tme_connection_other;
  *_score
    = ((conn_scsi->tme_scsi_connection_sequence_get
	== NULL)
       != (conn_scsi_other->tme_scsi_connection_sequence_get
	   == NULL));
  return (TME_OK);
}

/* this parses a SCSI ID: */
int
tme_scsi_id_parse(const char *id_string)
{
  unsigned long id;
  char *p1;

  /* catch a NULL string: */
  if (id_string == NULL) {
    return (-1);
  }

  /* convert the string: */
  id = strtoul(id_string, &p1, 0);
  if (p1 == id_string
      || *p1 != '\0') {
    return (-1);
  }
  return (id);
}
