/* $Id: bsd-bpf.c,v 1.3 2003/05/18 00:06:06 fredette Exp $ */

/* host/bsd/bsd-bpf.c - BSD Berkeley Packet Filter Ethernet support: */

/*
 * Copyright (c) 2001, 2003 Matt Fredette
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
_TME_RCSID("$Id: bsd-bpf.c,v 1.3 2003/05/18 00:06:06 fredette Exp $");

/* includes: */
#include "bsd-impl.h"
#include <tme/generic/ethernet.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <net/if.h>
#include <netinet/in_systm.h>
#include <netinet/in.h>
#if defined(HAVE_SYS_SOCKIO_H)
#include <sys/sockio.h>
#elif defined(HAVE_SYS_SOCKETIO_H)
#include <sys/socketio.h> 
#endif /* HAVE_SYS_SOCKETIO_H */
#include <sys/ioctl.h>
#ifdef HAVE_IOCTLS_H
#include <ioctls.h>
#endif /* HAVE_IOCTLS_H */
#ifdef HAVE_NET_IF_ETHER_H
#include <net/if_ether.h>
#endif /* HAVE_NET_IF_ETHER_H */
#ifdef HAVE_NET_ETHERNET_H
#include <net/ethernet.h>
#endif /* HAVE_NET_ETHERNET_H */
#include <netinet/ip.h>
#ifdef HAVE_NET_IF_DL_H
#include <net/if_dl.h>
#endif /* HAVE_NET_IF_DL_H */
#include <arpa/inet.h>
#include <net/bpf.h>

/* macros: */

/* the callout flags: */
#define TME_BSD_BPF_CALLOUT_CHECK	(0)
#define TME_BSD_BPF_CALLOUT_RUNNING	TME_BIT(0)
#define TME_BSD_BPF_CALLOUTS_MASK	(-2)
#define  TME_BSD_BPF_CALLOUT_CTRL	TME_BIT(1)
#define  TME_BSD_BPF_CALLOUT_READ	TME_BIT(2)

/* structures: */

/* our internal data structure: */
struct tme_bsd_bpf {

  /* backpointer to our element: */
  struct tme_element *tme_bsd_bpf_element;

  /* our mutex: */
  tme_mutex_t tme_bsd_bpf_mutex;

  /* our reader and writer conditions: */
  tme_cond_t tme_bsd_bpf_cond_reader;
  tme_cond_t tme_bsd_bpf_cond_writer;

  /* the callout flags: */
  unsigned int tme_bsd_bpf_callout_flags;

  /* our Ethernet connection: */
  struct tme_ethernet_connection *tme_bsd_bpf_eth_connection;

  /* the BPF file descriptor: */
  int tme_bsd_bpf_fd;

  /* the size of the packet buffer for the interface: */
  size_t tme_bsd_bpf_buffer_size;

  /* the packet buffer for the interface: */
  tme_uint8_t *tme_bsd_bpf_buffer;

  /* the next offset within the packet buffer, and the end of the data
     in the packet buffer: */
  size_t tme_bsd_bpf_buffer_offset;
  size_t tme_bsd_bpf_buffer_end;
};

/* the accept and reject packet insns: */
static const struct bpf_insn _tme_bsd_bpf_insn_accept = BPF_STMT(BPF_RET + BPF_K, (u_int) -1);
static const struct bpf_insn _tme_bsd_bpf_insn_reject = BPF_STMT(BPF_RET + BPF_K, 0);

/* this creates a BPF filter that accepts Ethernet packets with
   destination addresses in the configured set.  the broadcast address
   must be in this set, it isn't accepted automatically: */
static int
_tme_bsd_bpf_filter(struct tme_ethernet_config *config, 
		    const tme_uint8_t *prefix,
		    unsigned int prefix_len,
		    struct bpf_insn *bpf_filter,
		    int bpf_filter_size,
		    int *_first_pc)
{
  unsigned int addr_i;
  tme_uint8_t byte;
  tme_uint8_t byte_bitmap[(1 << (8 * sizeof(byte))) >> 3];
  int match_pc, miss_pc, this_pc;

  /* clear the byte bitmap: */
  memset(byte_bitmap, 0, sizeof(byte_bitmap));

  /* the last instruction jumps to the reject insn when it fails: */
  miss_pc = bpf_filter_size - 1;

  /* loop over all of the addresses: */
  for (addr_i = 0;
       addr_i < config->tme_ethernet_config_addr_count;
       addr_i++) {

    /* skip this address if it doesn't match the prefix: */
    if (prefix_len > 0
	&& memcmp(config->tme_ethernet_config_addrs[addr_i],
		  prefix,
		  prefix_len)) {
      continue;
    }

    /* get the next byte, and skip this address if this byte has
       already been done: */
    byte = config->tme_ethernet_config_addrs[addr_i][prefix_len];
    if (byte_bitmap[byte >> 3] & TME_BIT(byte & 7)) {
      continue;
    }
    byte_bitmap[byte >> 3] |= TME_BIT(byte & 7);

    /* get the PC of the instruction to branch to if this byte
       matches.  if this is the last byte of the address, the branch
       target is the accept insn, otherwise recurse and get the first
       insn of the rest of the matcher: */
    match_pc = ((prefix_len == (TME_ETHERNET_ADDR_SIZE - 1))
		? bpf_filter_size - 2
		: _tme_bsd_bpf_filter(config,
				      config->tme_ethernet_config_addrs[addr_i],
				      prefix_len + 1,
				      bpf_filter,
				      bpf_filter_size,
				      _first_pc));

    /* add this testing instruction: */
    this_pc = --(*_first_pc);
    assert(this_pc >= 0);
    bpf_filter[this_pc].code = BPF_JMP + BPF_JEQ + BPF_K;
    bpf_filter[this_pc].jt = match_pc - (this_pc + 1);
    bpf_filter[this_pc].jf = miss_pc - (this_pc + 1);
    bpf_filter[this_pc].k = byte;

    /* update the miss pc: */
    miss_pc = this_pc;
  }

  /* add this load instruction: */
  this_pc = --(*_first_pc);
  assert(this_pc >= 0);
  bpf_filter[this_pc].code = BPF_LD + BPF_B + BPF_ABS;
  bpf_filter[this_pc].k = prefix_len;

  /* return our pc: */
  return (this_pc);
}

/* this dumps a BPF filter.  not all insns are supported, just
   those used by our address matching filters: */
void
_tme_bsd_bpf_dump_filter(const struct bpf_program *program)
{
  int pc;
  FILE *fp;
  const struct bpf_insn *insn;
  char ldsize;
  const char *opc;

  fp = stderr;
  for (pc = 0, insn = program->bf_insns;
       pc < program->bf_len;
       pc++, insn++) {
    
    /* the PC: */
    fprintf(fp, "%d:\t", pc);

    /* dispatch on the instruction class: */
    switch (BPF_CLASS(insn->code)) {

    case BPF_LD:

      switch (BPF_SIZE(insn->code)) {
      case BPF_B: ldsize = 'b'; break;
      case BPF_H: ldsize = 'w'; break;
      case BPF_W: ldsize = 'l'; break;
      default: ldsize = '?'; break;
      }
      fprintf(fp, "ld.%c ", ldsize);

      switch (BPF_MODE(insn->code)) {
      case BPF_ABS: fprintf(fp, "0x%x", insn->k); break;
      default: fprintf(fp, "??");
      }

      break;

    case BPF_JMP:

      switch (BPF_OP(insn->code)) {
      case BPF_JEQ: opc = "jeq"; break;
      default: opc = "??"; break;
      }
      fprintf(fp, "%s ", opc);

      switch (BPF_SRC(insn->code)) {
      case BPF_K: fprintf(fp, "#0x%x", insn->k); break;
      case BPF_X: fprintf(fp, "x"); break;
      default: fprintf(fp, "??"); break;
      }

      fprintf(fp, ", %d, %d", pc + 1 + insn->jt, pc + 1 + insn->jf);
      break;

    case BPF_RET:
      switch (BPF_RVAL(insn->code)) {
      case BPF_A: fprintf(fp, "ret a"); break;
      case BPF_X: fprintf(fp, "ret x"); break;
      case BPF_K: fprintf(fp, "ret #0x%x", insn->k); break;
      default: fprintf(fp, "ret ??"); break;
      }
      break;

    default:
      fprintf(fp, "??");
      break;
    }

    putc('\n', fp);
  }
}

/* the bpf callout function.  it must be called with the mutex locked: */
static void
_tme_bsd_bpf_callout(struct tme_bsd_bpf *bpf, int new_callouts)
{
  struct tme_ethernet_connection *conn_eth;
  int callouts, later_callouts;
  unsigned int ctrl;
  int rc;
  tme_ethernet_fid_t frame_id;
  struct tme_ethernet_frame_chunk frame_chunk_buffer;
  tme_uint8_t frame[TME_ETHERNET_FRAME_MAX];
  
  /* add in any new callouts: */
  bpf->tme_bsd_bpf_callout_flags |= new_callouts;

  /* if this function is already running in another thread, simply
     return now.  the other thread will do our work: */
  if (bpf->tme_bsd_bpf_callout_flags & TME_BSD_BPF_CALLOUT_RUNNING) {
    return;
  }

  /* callouts are now running: */
  bpf->tme_bsd_bpf_callout_flags |= TME_BSD_BPF_CALLOUT_RUNNING;

  /* assume that we won't need any later callouts: */
  later_callouts = 0;

  /* loop while callouts are needed: */
  for (; (callouts = bpf->tme_bsd_bpf_callout_flags) & TME_BSD_BPF_CALLOUTS_MASK; ) {

    /* clear the needed callouts: */
    bpf->tme_bsd_bpf_callout_flags = callouts & ~TME_BSD_BPF_CALLOUTS_MASK;
    callouts &= TME_BSD_BPF_CALLOUTS_MASK;

    /* get our Ethernet connection: */
    conn_eth = bpf->tme_bsd_bpf_eth_connection;

    /* if we need to call out new control information: */
    if (callouts & TME_BSD_BPF_CALLOUT_CTRL) {

      /* form the new ctrl: */
      ctrl = 0;
      if (bpf->tme_bsd_bpf_buffer_offset
	  < bpf->tme_bsd_bpf_buffer_end) {
	ctrl |= TME_ETHERNET_CTRL_OK_READ;
      }

      /* unlock the mutex: */
      tme_mutex_unlock(&bpf->tme_bsd_bpf_mutex);
      
      /* do the callout: */
      rc = (conn_eth != NULL
	    ? ((*conn_eth->tme_ethernet_connection_ctrl)
	       (conn_eth,
		ctrl))
	    : TME_OK);
	
      /* lock the mutex: */
      tme_mutex_lock(&bpf->tme_bsd_bpf_mutex);
      
      /* if the callout was unsuccessful, remember that at some later
	 time this callout should be attempted again: */
      if (rc != TME_OK) {
	later_callouts |= TME_BSD_BPF_CALLOUT_CTRL;
      }
    }
      
    /* if the Ethernet is readable: */
    if (callouts & TME_BSD_BPF_CALLOUT_READ) {

      /* unlock the mutex: */
      tme_mutex_unlock(&bpf->tme_bsd_bpf_mutex);
      
      /* make a frame chunk to receive this frame: */
      frame_chunk_buffer.tme_ethernet_frame_chunk_next = NULL;
      frame_chunk_buffer.tme_ethernet_frame_chunk_bytes = frame;
      frame_chunk_buffer.tme_ethernet_frame_chunk_bytes_count
	= sizeof(frame);

      /* do the callout: */
      rc = (conn_eth == NULL
	    ? TME_OK
	    : ((*conn_eth->tme_ethernet_connection_read)
	       (conn_eth,
		&frame_id,
		&frame_chunk_buffer,
		TME_ETHERNET_READ_NEXT)));
      
      /* lock the mutex: */
      tme_mutex_lock(&bpf->tme_bsd_bpf_mutex);
      
      /* if the read was successful: */
      if (rc > 0) {

	/* do the write: */
	tme_thread_write(bpf->tme_bsd_bpf_fd, frame, rc);

	/* mark that we need to loop to callout to read more frames: */
	bpf->tme_bsd_bpf_callout_flags |= TME_BSD_BPF_CALLOUT_READ;
      }

      /* otherwise, the read failed.  convention dictates that we
	 forget that the connection was readable, which we already
	 have done by clearing the CALLOUT_READ flag: */
    }

  }
  
  /* put in any later callouts, and clear that callouts are running: */
  bpf->tme_bsd_bpf_callout_flags = later_callouts;
}

/* the BPF reader thread: */
static void
_tme_bsd_bpf_th_reader(struct tme_bsd_bpf *bpf)
{
  ssize_t buffer_end;
  
  /* lock the mutex: */
  tme_mutex_lock(&bpf->tme_bsd_bpf_mutex);

  /* loop forever: */
  for (;;) {

    /* if the buffer is not empty, wait until it is: */
    if (bpf->tme_bsd_bpf_buffer_offset
	< bpf->tme_bsd_bpf_buffer_end) {
      tme_cond_wait_yield(&bpf->tme_bsd_bpf_cond_reader,
			  &bpf->tme_bsd_bpf_mutex);
    }

    /* unlock the mutex: */
    tme_mutex_unlock(&bpf->tme_bsd_bpf_mutex);

    /* read the BPF socket: */
    tme_log(&bpf->tme_bsd_bpf_element->tme_element_log_handle, 1, TME_OK,
	    (&bpf->tme_bsd_bpf_element->tme_element_log_handle,
	     _("calling read")));
    buffer_end = 
      tme_thread_read_yield(bpf->tme_bsd_bpf_fd,
			    bpf->tme_bsd_bpf_buffer,
			    bpf->tme_bsd_bpf_buffer_size);

    /* lock the mutex: */
    tme_mutex_lock(&bpf->tme_bsd_bpf_mutex);

    /* if the read failed: */
    if (buffer_end <= 0) {
      tme_log(&bpf->tme_bsd_bpf_element->tme_element_log_handle, 1, errno,
	      (&bpf->tme_bsd_bpf_element->tme_element_log_handle,
	       _("failed to read packets")));
      continue;
    }

    /* the read succeeded: */
    tme_log(&bpf->tme_bsd_bpf_element->tme_element_log_handle, 1, TME_OK,
	    (&bpf->tme_bsd_bpf_element->tme_element_log_handle,
	     _("read %ld bytes of packets"), (long) buffer_end));
    bpf->tme_bsd_bpf_buffer_offset = 0;
    bpf->tme_bsd_bpf_buffer_end = buffer_end;

    /* call out that we can be read again: */
    _tme_bsd_bpf_callout(bpf, TME_BSD_BPF_CALLOUT_CTRL);
  }
  /* NOTREACHED */
}

/* this is called when the ethernet configuration changes: */
static int
_tme_bsd_bpf_config(struct tme_ethernet_connection *conn_eth, 
		    struct tme_ethernet_config *config)
{
  struct tme_bsd_bpf *bpf;
  struct bpf_insn *bpf_filter;
  struct bpf_program program;
  int bpf_filter_size, first_pc;
  int rc;

  /* recover our data structures: */
  bpf = conn_eth->tme_ethernet_connection.tme_connection_element->tme_element_private;

  /* assume we will succeed: */
  rc = TME_OK;

  /* lock the mutex: */
  tme_mutex_lock(&bpf->tme_bsd_bpf_mutex);

  /* allocate space for the worst-case filter: one insn for the packet
     accept, one insn for the packet reject, and TME_ETHERNET_ADDR_SIZE
     * 2 insns for each address - one insn to load an address byte and
     one insn to test it and branch: */
  bpf_filter_size = (1
		     + 1
		     + ((1 + 1)
			* TME_ETHERNET_ADDR_SIZE
			* config->tme_ethernet_config_addr_count));
  bpf_filter = tme_new(struct bpf_insn, bpf_filter_size);
  first_pc = bpf_filter_size;

  /* if this Ethernet is promiscuous, we will accept all packets: */
  if (config->tme_ethernet_config_flags & TME_ETHERNET_CONFIG_PROMISC) {
    bpf_filter[--first_pc] = _tme_bsd_bpf_insn_accept;
  }

  /* if this Ethernet does have a set of addresses, we will accept all
     packets for one of those addresses: */
  else if (config->tme_ethernet_config_addr_count > 0) {

    /* the last insn in the filter is always the packet reject,
       and the next-to-last insn in the filter is always the
       packet accept.  _tme_bsd_bpf_filter depends on this: */
    bpf_filter[--first_pc] = _tme_bsd_bpf_insn_reject;
    bpf_filter[--first_pc] = _tme_bsd_bpf_insn_accept;

    /* make the address filter: */
    _tme_bsd_bpf_filter(config, 
			NULL,
			0,
			bpf_filter,
			bpf_filter_size,
			&first_pc);
  }

  /* otherwise this filter doesn't need to accept any packets: */
  else {
    bpf_filter[--first_pc] = _tme_bsd_bpf_insn_reject;
  }

  /* set the filter on the BPF device: */
  program.bf_len = bpf_filter_size - first_pc;
  program.bf_insns = bpf_filter + first_pc;
  if (ioctl(bpf->tme_bsd_bpf_fd, BIOCSETF, &program) < 0) {
    tme_log(&bpf->tme_bsd_bpf_element->tme_element_log_handle, 1, errno,
	    (&bpf->tme_bsd_bpf_element->tme_element_log_handle,
	     _("failed to set the filter")));
    rc = errno;
  }

  /* free the filter: */
  tme_free(bpf_filter);

  /* unlock the mutex: */
  tme_mutex_unlock(&bpf->tme_bsd_bpf_mutex);

  /* done: */
  return (rc);
}

/* this is called when control lines change: */
static int
_tme_bsd_bpf_ctrl(struct tme_ethernet_connection *conn_eth, 
		  unsigned int ctrl)
{
  struct tme_bsd_bpf *bpf;
  int new_callouts;

  /* recover our data structures: */
  bpf = conn_eth->tme_ethernet_connection.tme_connection_element->tme_element_private;

  /* assume that we won't need any new callouts: */
  new_callouts = 0;

  /* lock the mutex: */
  tme_mutex_lock(&bpf->tme_bsd_bpf_mutex);

  /* if this connection is readable, call out a read: */
  if (ctrl & TME_ETHERNET_CTRL_OK_READ) {
    new_callouts |= TME_BSD_BPF_CALLOUT_READ;
  }

  /* make any new callouts: */
  _tme_bsd_bpf_callout(bpf, new_callouts);

  /* unlock the mutex: */
  tme_mutex_unlock(&bpf->tme_bsd_bpf_mutex);

  return (TME_OK);
}

/* this is called to read a frame: */
static int
_tme_bsd_bpf_read(struct tme_ethernet_connection *conn_eth, 
		  tme_ethernet_fid_t *_frame_id,
		  struct tme_ethernet_frame_chunk *frame_chunks,
		  unsigned int flags)
{
  struct tme_bsd_bpf *bpf;
  struct bpf_hdr the_bpf_header;
  struct tme_ethernet_frame_chunk frame_chunk_buffer;
  unsigned int count;
  int rc;

  /* recover our data structure: */
  bpf = conn_eth->tme_ethernet_connection.tme_connection_element->tme_element_private;

  /* lock our mutex: */
  tme_mutex_lock(&bpf->tme_bsd_bpf_mutex);

  /* assume that we won't be able to return a packet: */
  rc = -ENOENT;

  /* loop until we have a good captured packet or until we 
     exhaust the buffer: */
  for (;;) {
    
    /* if there's not enough for a BPF header, flush the buffer: */
    if ((bpf->tme_bsd_bpf_buffer_offset
	 + sizeof(the_bpf_header))
	> bpf->tme_bsd_bpf_buffer_end) {
      tme_log(&bpf->tme_bsd_bpf_element->tme_element_log_handle, 1, TME_OK,
	      (&bpf->tme_bsd_bpf_element->tme_element_log_handle,
	       _("flushed garbage BPF header bytes")));
      bpf->tme_bsd_bpf_buffer_end = 0;
      break;
    }

    /* get the BPF header and check it: */
    memcpy(&the_bpf_header,
	   bpf->tme_bsd_bpf_buffer
	   + bpf->tme_bsd_bpf_buffer_offset,
	   sizeof(the_bpf_header));
    bpf->tme_bsd_bpf_buffer_offset += the_bpf_header.bh_hdrlen;

    /* if we're missing some part of the packet: */
    if (the_bpf_header.bh_caplen != the_bpf_header.bh_datalen
	|| ((bpf->tme_bsd_bpf_buffer_offset + the_bpf_header.bh_datalen)
	    > bpf->tme_bsd_bpf_buffer_end)) {
      tme_log(&bpf->tme_bsd_bpf_element->tme_element_log_handle, 1, TME_OK,
	      (&bpf->tme_bsd_bpf_element->tme_element_log_handle,
	       _("flushed truncated BPF packet")));
      bpf->tme_bsd_bpf_buffer_offset += the_bpf_header.bh_datalen;
      continue;
    }

    /* if this packet isn't big enough to even have an Ethernet header: */
    if (the_bpf_header.bh_datalen < sizeof(struct tme_ethernet_header)) {
      tme_log(&bpf->tme_bsd_bpf_element->tme_element_log_handle, 1, TME_OK,
	      (&bpf->tme_bsd_bpf_element->tme_element_log_handle,
	       _("flushed short BPF packet")));
      bpf->tme_bsd_bpf_buffer_offset += the_bpf_header.bh_datalen;
      continue;
    }

    /* form the single frame chunk: */
    frame_chunk_buffer.tme_ethernet_frame_chunk_next = NULL;
    frame_chunk_buffer.tme_ethernet_frame_chunk_bytes
      = bpf->tme_bsd_bpf_buffer + bpf->tme_bsd_bpf_buffer_offset;
    frame_chunk_buffer.tme_ethernet_frame_chunk_bytes_count
      = the_bpf_header.bh_datalen;

    /* copy out the frame: */
    count = tme_ethernet_chunks_copy(frame_chunks, &frame_chunk_buffer);

    /* if this isn't a peek: */
    if (!(flags & TME_ETHERNET_READ_PEEK)) {

      /* update the buffer pointer: */
      bpf->tme_bsd_bpf_buffer_offset += the_bpf_header.bh_datalen;
    }

    /* success: */
    rc = count;
    break;
  }

  /* if the buffer is empty, notify the reader that we need more data: */
  if (bpf->tme_bsd_bpf_buffer_offset
      >= bpf->tme_bsd_bpf_buffer_end) {
    tme_cond_notify(&bpf->tme_bsd_bpf_cond_reader, TRUE);
  }

  /* unlock our mutex: */
  tme_mutex_unlock(&bpf->tme_bsd_bpf_mutex);

  /* done: */
  return (rc);
}

/* this makes a new Ethernet connection: */
static int
_tme_bsd_bpf_connection_make(struct tme_connection *conn, unsigned int state)
{
  struct tme_bsd_bpf *bpf;
  struct tme_ethernet_connection *conn_eth;
  struct tme_ethernet_connection *conn_eth_other;

  /* recover our data structures: */
  bpf = conn->tme_connection_element->tme_element_private;
  conn_eth = (struct tme_ethernet_connection *) conn;
  conn_eth_other = (struct tme_ethernet_connection *) conn->tme_connection_other;

  /* both sides must be Ethernet connections: */
  assert(conn->tme_connection_type == TME_CONNECTION_ETHERNET);
  assert(conn->tme_connection_other->tme_connection_type == TME_CONNECTION_ETHERNET);

  /* we're always set up to answer calls across the connection, so we
     only have to do work when the connection has gone full, namely
     taking the other side of the connection: */
  if (state == TME_CONNECTION_FULL) {

    /* lock our mutex: */
    tme_mutex_lock(&bpf->tme_bsd_bpf_mutex);

    /* save our connection: */
    bpf->tme_bsd_bpf_eth_connection = conn_eth_other;

    /* unlock our mutex: */
    tme_mutex_unlock(&bpf->tme_bsd_bpf_mutex);
  }

  return (TME_OK);
}

/* this breaks a connection: */
static int
_tme_bsd_bpf_connection_break(struct tme_connection *conn, unsigned int state)
{
  abort();
}

/* this makes a new connection side for a BPF: */
static int
_tme_bsd_bpf_connections_new(struct tme_element *element, 
			     const char * const *args, 
			     struct tme_connection **_conns,
			     char **_output)
{
  struct tme_bsd_bpf *bpf;
  struct tme_ethernet_connection *conn_eth;
  struct tme_connection *conn;

  /* recover our data structure: */
  bpf = (struct tme_bsd_bpf *) element->tme_element_private;

  /* if we already have an Ethernet connection, do nothing: */
  if (bpf->tme_bsd_bpf_eth_connection != NULL) {
    return (TME_OK);
  }

  /* allocate the new Ethernet connection: */
  conn_eth = tme_new0(struct tme_ethernet_connection, 1);
  conn = &conn_eth->tme_ethernet_connection;
  
  /* fill in the generic connection: */
  conn->tme_connection_next = *_conns;
  conn->tme_connection_type = TME_CONNECTION_ETHERNET;
  conn->tme_connection_score = tme_ethernet_connection_score;
  conn->tme_connection_make = _tme_bsd_bpf_connection_make;
  conn->tme_connection_break = _tme_bsd_bpf_connection_break;

  /* fill in the Ethernet connection: */
  conn_eth->tme_ethernet_connection_config = _tme_bsd_bpf_config;
  conn_eth->tme_ethernet_connection_ctrl = _tme_bsd_bpf_ctrl;
  conn_eth->tme_ethernet_connection_read = _tme_bsd_bpf_read;

  /* return the connection side possibility: */
  *_conns = conn;

  /* done: */
  return (TME_OK);
}

/* the new BPF function: */
TME_ELEMENT_SUB_NEW_DECL(tme_host_bsd,bpf) {
  struct tme_bsd_bpf *bpf;
  int bpf_fd;
#define DEV_BPF_FORMAT "/dev/bpf%d"
  char dev_bpf_filename[sizeof(DEV_BPF_FORMAT) + (sizeof(int) * 3) + 1];
  int minor;
  int saved_errno;
  u_int bpf_opt;
  struct bpf_version version;
  u_int packet_buffer_size;
  const char *ifr_name_user;
  struct ifreq *ifr;
  int arg_i;
  int usage;
  int rc;
  
  /* check our arguments: */
  usage = 0;
  ifr_name_user = NULL;
  arg_i = 1;
  for (;;) {

    /* the interface we're supposed to use: */
    if (TME_ARG_IS(args[arg_i + 0], "interface")
	&& args[arg_i + 1] != NULL) {
      ifr_name_user = args[arg_i + 1];
      arg_i += 2;
    }

    /* if we ran out of arguments: */
    else if (args[arg_i + 0] == NULL) {
      break;
    }

    /* otherwise this is a bad argument: */
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
			    "%s %s [ interface %s ]",
			    _("usage:"),
			    args[0],
			    _("INTERFACE"));
    return (EINVAL);
  }

  /* find the interface we will use: */
  rc = tme_bsd_if_find(ifr_name_user, &ifr, NULL, NULL);
  if (rc != TME_OK) {
    tme_output_append_error(_output, _("couldn't find an interface"));
    return (ENOENT);
  }
  tme_log(&element->tme_element_log_handle, 1, TME_OK, 
	  (&element->tme_element_log_handle, 
	   "using interface %s",
	   ifr->ifr_name));

  /* loop trying to open a /dev/bpf device: */
  for (minor = 0;; minor++) {
    
    /* form the name of the next device to try, then try opening
       it. if we succeed, we're done: */
    sprintf(dev_bpf_filename, DEV_BPF_FORMAT, minor);
    tme_log(&element->tme_element_log_handle, 1, TME_OK,
	    (&element->tme_element_log_handle,
	     "trying %s",
	     dev_bpf_filename));
    if ((bpf_fd = open(dev_bpf_filename, O_RDWR)) >= 0) {
      tme_log(&element->tme_element_log_handle, 1, TME_OK,
	      (&element->tme_element_log_handle,
	       "opened %s",
	       dev_bpf_filename));
      break;
    }

    /* we failed to open this device.  if this device was simply
       busy, loop: */
    saved_errno = errno;
    tme_log(&element->tme_element_log_handle, 1, saved_errno,
	    (&element->tme_element_log_handle, 
	     "%s", dev_bpf_filename));
    if (saved_errno == EBUSY
	|| saved_errno == EACCES) {
      continue;
    }

    /* otherwise, we have failed: */
    return (saved_errno);
  }

  /* this macro helps in closing the BPF socket on error: */
#define _TME_BPF_RAW_OPEN_ERROR(x) saved_errno = errno; x; errno = saved_errno

  /* check the BPF version: */
  if (ioctl(bpf_fd, BIOCVERSION, &version) < 0) {
    tme_log(&element->tme_element_log_handle, 1, errno,
	    (&element->tme_element_log_handle,
	     _("failed to get the BPF version on %s"),
	     dev_bpf_filename));
    _TME_BPF_RAW_OPEN_ERROR(close(bpf_fd));
    return (errno);
  }
  if (version.bv_major != BPF_MAJOR_VERSION
      || version.bv_minor < BPF_MINOR_VERSION) {
    tme_log(&element->tme_element_log_handle, 1, errno,
	    (&element->tme_element_log_handle,
	     _("kernel BPF version is %d.%d, my BPF version is %d.%d"),
	     version.bv_major, version.bv_minor,
	     BPF_MAJOR_VERSION, BPF_MINOR_VERSION));
    close(bpf_fd);
    return (ENXIO);
  }

  /* put the BPF device into immediate mode: */
  bpf_opt = TRUE;
  if (ioctl(bpf_fd, BIOCIMMEDIATE, &bpf_opt) < 0) {
    tme_log(&element->tme_element_log_handle, 1, errno,
	    (&element->tme_element_log_handle,
	     _("failed to put %s into immediate mode"),
	     dev_bpf_filename));
    _TME_BPF_RAW_OPEN_ERROR(close(bpf_fd));
    return (errno);
  }

  /* tell the BPF device we're providing complete Ethernet headers: */
  bpf_opt = TRUE;
  if (ioctl(bpf_fd, BIOCSHDRCMPLT, &bpf_opt) < 0) {
    tme_log(&element->tme_element_log_handle, 1, errno,
	    (&element->tme_element_log_handle,
	     _("failed to put %s into complete-headers mode"),
	     dev_bpf_filename));
    _TME_BPF_RAW_OPEN_ERROR(close(bpf_fd));
    return (errno);
  }

  /* point the BPF device at the interface we're using: */
  if (ioctl(bpf_fd, BIOCSETIF, ifr) < 0) {
    tme_log(&element->tme_element_log_handle, 1, errno,
	    (&element->tme_element_log_handle,
	     _("failed to point BPF socket at %s"),
	     ifr->ifr_name));
    saved_errno = errno;
    close(bpf_fd);
    errno = saved_errno;
    return (errno);
  }

  /* get the BPF read buffer size: */
  if (ioctl(bpf_fd, BIOCGBLEN, &packet_buffer_size) < 0) {
    tme_log(&element->tme_element_log_handle, 1, errno,
	    (&element->tme_element_log_handle,
	     _("failed to read the buffer size for %s"),
	     dev_bpf_filename));
    _TME_BPF_RAW_OPEN_ERROR(close(bpf_fd));
    return (errno);
  }
  tme_log(&element->tme_element_log_handle, 1, errno,
	  (&element->tme_element_log_handle,
	   _("buffer size for %s is %u"),
	   dev_bpf_filename, packet_buffer_size));

  /* set the interface into promiscuous mode: */
  if (ioctl(bpf_fd, BIOCPROMISC) < 0) {
    tme_log(&element->tme_element_log_handle, 1, errno,
	    (&element->tme_element_log_handle,
	     _("failed to set promiscuous mode on %s"),
	     dev_bpf_filename));
    _TME_BPF_RAW_OPEN_ERROR(close(bpf_fd));
    return (errno);
  }
  
  /* start our data structure: */
  bpf = tme_new0(struct tme_bsd_bpf, 1);
  bpf->tme_bsd_bpf_element = element;
  bpf->tme_bsd_bpf_fd = bpf_fd;
  bpf->tme_bsd_bpf_buffer_size = packet_buffer_size;
  bpf->tme_bsd_bpf_buffer = tme_new(tme_uint8_t, packet_buffer_size);

  /* start the threads: */
  tme_mutex_init(&bpf->tme_bsd_bpf_mutex);
  tme_cond_init(&bpf->tme_bsd_bpf_cond_writer);
  tme_thread_create((tme_thread_t) _tme_bsd_bpf_th_reader, bpf);

  /* fill the element: */
  element->tme_element_private = bpf;
  element->tme_element_connections_new = _tme_bsd_bpf_connections_new;

  return (TME_OK);
#undef _TME_BPF_RAW_OPEN_ERROR
}
