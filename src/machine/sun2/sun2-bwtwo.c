/* $Id: sun2-bwtwo.c,v 1.2 2003/07/29 18:25:17 fredette Exp $ */

/* machine/sun2/sun2-bwtwo.c - Sun2 bwtwo emulation implementation: */

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
_TME_RCSID("$Id: sun2-bwtwo.c,v 1.2 2003/07/29 18:25:17 fredette Exp $");

/* includes: */
#include <tme/machine/sun.h>
#include <tme/generic/bus-device.h>
#include <tme/generic/fb.h>

/* macros: */

/* screen dimensions: */
#define TME_SUN2BW2_LO_WIDTH	(1152)
#define TME_SUN2BW2_LO_HEIGHT	(900)
#define TME_SUN2BW2_HI_WIDTH	(1024)
#define TME_SUN2BW2_HI_HEIGHT	(1024)

/* register offsets and sizes: */
#define TME_SUN2BW2_REG_MEM		(0)
#define TME_SUN2BW2_SIZ_MEM		(0x20000)
#define TME_SUN2BW2_REG_CSR_OBMEM	(0x81800)
#define TME_SUN2BW2_REG_CSR_OBIO	(0x20000)
#define TME_SUN2BW2_SIZ_CSR		(0x00002)
#define TME_SUN2BW2_SIZ_CSR_PAGE	(0x00800)

/* the bits in the Control/Status register: */
#define TME_SUN2BW2_CSR_ENABLE_VIDEO	(0x8000)	/* enable video */
#define TME_SUN2BW2_CSR_ENABLE_COPY	(0x4000)	/* enable copy mode */
#define TME_SUN2BW2_CSR_ENABLE_INT	(0x2000)	/* interrupt enable */
#define TME_SUN2BW2_CSR_INT_ACTIVE	(0x1000)	/* interrupt is active */
#define TME_SUN2BW2_CSR_JUMPER_B	(0x0800)	/* jumper B */
#define TME_SUN2BW2_CSR_JUMPER_A	(0x0400)	/* jumper A */
#define TME_SUN2BW2_CSR_JUMPER_COLOR	(0x0200)	/* jumper color */
#define TME_SUN2BW2_CSR_JUMPER_HIRES	(0x0100)	/* jumper hires */
#define TME_SUN2BW2_CSR_COPYBASE_MASK	(0x007E)	/* copybase mask */

#if 0
#define TME_SUN2BW2_DEBUG
#endif

/* structures: */

/* the card: */
struct tme_sun2bw2 {

  /* our simple bus device header: */
  struct tme_bus_device tme_sun2bw2_device;
#define tme_sun2bw2_element tme_sun2bw2_device.tme_bus_device_element

  /* the mutex protecting the card: */
  tme_mutex_t tme_sun2bw2_mutex;

  /* the rwlock protecting the card: */
  tme_rwlock_t tme_sun2bw2_rwlock;

  /* the framebuffer connection: */
  struct tme_fb_connection *tme_sun2bw2_fb_connection;

  /* if our interrupt line is currently asserted: */
  int tme_sun2bw2_int_asserted;

  /* the (relative) bus address of our csr register: */
  tme_bus_addr_t tme_sun2bw2_csr_address;

  /* the (relative) bus address of the first byte after displayed
     framebuffer memory: */
  tme_bus_addr_t tme_sun2bw2_end_address;

  /* the framebuffer memory: */
  tme_uint8_t *tme_sun2bw2_fb_memory;

  /* any pad memory: */
  tme_uint8_t *tme_sun2bw2_pad_memory;

  /* our csr: */
  tme_uint16_t tme_sun2bw2_csr;
};

/* globals: */

static const struct tme_bus_subregion _tme_sun2bw2_subregion_csr_obmem = {
  TME_SUN2BW2_REG_CSR_OBMEM,
  TME_SUN2BW2_REG_CSR_OBMEM
  + TME_SUN2BW2_SIZ_CSR_PAGE
  - 1,
  NULL };
static const struct tme_bus_subregion _tme_sun2bw2_subregion_csr_obio = {
  TME_SUN2BW2_REG_CSR_OBIO,
  TME_SUN2BW2_REG_CSR_OBIO
  + TME_SUN2BW2_SIZ_CSR_PAGE
  - 1,
  NULL };

#ifdef TME_SUN2BW2_DEBUG
static int
_tme_sun2bw2_update_debug(struct tme_fb_connection *conn_fb)
{
  struct tme_sun2bw2 *sun2bw2;
  static int y = -1;
  static int x;
  unsigned long pixel;
  unsigned int pixel_byte;
  tme_uint8_t pixel_bit;
  int box, box_y, box_x;

  sun2bw2 = conn_fb->tme_fb_connection.tme_connection_element->tme_element_private;

  for (box = 0; box < 2; box++) {
    if (y < 0) {
      y = 16;
      x = 0;
      continue;
    }
    for (box_y = 0; box_y < 2; box_y++) {
      for (box_x = 0; box_x < 2; box_x++) {
	pixel = (((y + box_y)
		  * TME_SUN2BW2_LO_WIDTH)
		 + x
		 + box_x);
	pixel_byte = (pixel / 8);
	pixel_bit = (0x80 >> (pixel % 8));
	sun2bw2->tme_sun2bw2_fb_memory[pixel_byte] ^= pixel_bit;
      }
    }
    if (box == 0) {
      x += 2;
      if (x == TME_SUN2BW2_LO_WIDTH) {
	x = 0;
	y += 2;
	if (y == TME_SUN2BW2_LO_HEIGHT) {
	  y = 0;
	}
      }
    }
  }
    
  return (TME_OK);
}
#endif /* TME_SUN2BW2_DEBUG */

/* the sun2bw2 framebuffer bus cycle handler: */
static int
_tme_sun2bw2_bus_cycle_fb(void *_sun2bw2, struct tme_bus_cycle *cycle_init)
{
  struct tme_sun2bw2 *sun2bw2;

  /* recover our data structure: */
  sun2bw2 = (struct tme_sun2bw2 *) _sun2bw2;

  /* lock the mutex: */
  tme_mutex_lock(&sun2bw2->tme_sun2bw2_mutex);

  /* run the cycle: */
  assert (cycle_init->tme_bus_cycle_address
	  >= TME_SUN2BW2_REG_MEM);
  tme_bus_cycle_xfer_memory(cycle_init, 
			    (sun2bw2->tme_sun2bw2_fb_memory
			     - TME_SUN2BW2_REG_MEM),
			    sun2bw2->tme_sun2bw2_end_address - 1);
  
  /* unlock the mutex: */
  tme_mutex_unlock(&sun2bw2->tme_sun2bw2_mutex);

  /* no faults: */
  return (TME_OK);
}

/* the sun2bw2 pad bus cycle handler: */
static int
_tme_sun2bw2_bus_cycle_pad(void *_sun2bw2, struct tme_bus_cycle *cycle_init)
{
  struct tme_sun2bw2 *sun2bw2;

  /* recover our data structure: */
  sun2bw2 = (struct tme_sun2bw2 *) _sun2bw2;

  /* lock the mutex: */
  tme_mutex_lock(&sun2bw2->tme_sun2bw2_mutex);

  /* run the cycle: */
  assert (cycle_init->tme_bus_cycle_address
	  >= sun2bw2->tme_sun2bw2_end_address);
  assert (sun2bw2->tme_sun2bw2_end_address
	  < TME_SUN2BW2_SIZ_MEM);
  tme_bus_cycle_xfer_memory(cycle_init, 
			    (sun2bw2->tme_sun2bw2_pad_memory
			     - sun2bw2->tme_sun2bw2_end_address),
			    ((TME_SUN2BW2_SIZ_MEM
			      - sun2bw2->tme_sun2bw2_end_address)
			     - 1));

  /* unlock the mutex: */
  tme_mutex_unlock(&sun2bw2->tme_sun2bw2_mutex);

  /* no faults: */
  return (TME_OK);
}

/* the sun2bw2 CSR bus cycle handler: */
static int
_tme_sun2bw2_bus_cycle_csr(void *_sun2bw2, struct tme_bus_cycle *cycle_init)
{
  struct tme_sun2bw2 *sun2bw2;
  tme_uint16_t csr_old, csr_new;
  tme_bus_addr_t undecoded;

  /* recover our data structure: */
  sun2bw2 = (struct tme_sun2bw2 *) _sun2bw2;

  /* lock the mutex: */
  tme_mutex_lock(&sun2bw2->tme_sun2bw2_mutex);

  /* get the old CSR value: */
  csr_old = tme_betoh_u16(sun2bw2->tme_sun2bw2_csr);

  /* the entire 2KB (one page's) worth of addresses at
     tme_sun2bw2_csr_address are all decoded (or, rather, not decoded)
     as the CSR: */
  undecoded
    = (cycle_init->tme_bus_cycle_address
       & (TME_SUN2BW2_SIZ_CSR_PAGE - 2));
  cycle_init->tme_bus_cycle_address
    -= undecoded;

  /* run the cycle: */
  assert (cycle_init->tme_bus_cycle_address
	  >= sun2bw2->tme_sun2bw2_csr_address);
  tme_bus_cycle_xfer_memory(cycle_init, 
			    (((tme_uint8_t *) &sun2bw2->tme_sun2bw2_csr)
			     - sun2bw2->tme_sun2bw2_csr_address),
			    (sun2bw2->tme_sun2bw2_csr_address
			     + sizeof(sun2bw2->tme_sun2bw2_csr)
			     - 1));
  cycle_init->tme_bus_cycle_address
    += undecoded;

  /* get the new CSR value: */
  csr_new = tme_betoh_u16(sun2bw2->tme_sun2bw2_csr);

  /* put back the unchanging bits: */
  csr_new
    = ((csr_new
	& ~(TME_SUN2BW2_CSR_INT_ACTIVE
	    | TME_SUN2BW2_CSR_JUMPER_B
	    | TME_SUN2BW2_CSR_JUMPER_A
	    | TME_SUN2BW2_CSR_JUMPER_COLOR
	    | TME_SUN2BW2_CSR_JUMPER_HIRES))
       | (csr_old
	  & (TME_SUN2BW2_CSR_INT_ACTIVE
	     | TME_SUN2BW2_CSR_JUMPER_B
	     | TME_SUN2BW2_CSR_JUMPER_A
	     | TME_SUN2BW2_CSR_JUMPER_COLOR
	     | TME_SUN2BW2_CSR_JUMPER_HIRES)));

  /* we do not support these bits: */
  if (csr_new
      & (TME_SUN2BW2_CSR_ENABLE_COPY
	 | TME_SUN2BW2_CSR_ENABLE_INT)) {
    abort();
  }

  /* set the new CSR value: */
  sun2bw2->tme_sun2bw2_csr = tme_htobe_u16(csr_new);

  /* unlock the mutex: */
  tme_mutex_unlock(&sun2bw2->tme_sun2bw2_mutex);

  /* no faults: */
  return (TME_OK);
}

/* the sun2bw2 TLB filler: */
static int
_tme_sun2bw2_tlb_fill(void *_sun2bw2, struct tme_bus_tlb *tlb, 
		      tme_bus_addr_t address, unsigned int cycles)
{
  struct tme_sun2bw2 *sun2bw2;

  /* recover our data structure: */
  sun2bw2 = (struct tme_sun2bw2 *) _sun2bw2;

  /* initialize the TLB entry: */
  tme_bus_tlb_initialize(tlb);

  /* if the address falls in the CSR: */
  if ((sun2bw2->tme_sun2bw2_csr_address
       <= address)
      && (address
	  < (sun2bw2->tme_sun2bw2_csr_address
	     + TME_SUN2BW2_SIZ_CSR_PAGE))) {

    tlb->tme_bus_tlb_cycle = _tme_sun2bw2_bus_cycle_csr;

    /* this TLB entry covers this range: */
    TME_ATOMIC_WRITE(tme_bus_addr_t, 
		     tlb->tme_bus_tlb_addr_first,
		     sun2bw2->tme_sun2bw2_csr_address);
    TME_ATOMIC_WRITE(tme_bus_addr_t, 
		     tlb->tme_bus_tlb_addr_last, 
		     (sun2bw2->tme_sun2bw2_csr_address
		      + TME_SUN2BW2_SIZ_CSR
		      - 1));

    /* this TLB entry cannot allow fast reading, since the page the
       CSR is on isn't fully decoded - all words on the 2KB page are
       the CSR: */
  }

  /* if this address falls in the displayed framebuffer memory: */
  else if ((TME_SUN2BW2_REG_MEM
	    <= address)
	   && (address
	       < sun2bw2->tme_sun2bw2_end_address)) {

    assert (sun2bw2->tme_sun2bw2_fb_connection != NULL);

    tlb->tme_bus_tlb_cycle = _tme_sun2bw2_bus_cycle_fb;

    /* this TLB entry covers this range: */
    TME_ATOMIC_WRITE(tme_bus_addr_t,
		     tlb->tme_bus_tlb_addr_first, 
		     TME_SUN2BW2_REG_MEM);
    TME_ATOMIC_WRITE(tme_bus_addr_t,
		     tlb->tme_bus_tlb_addr_last,
		     sun2bw2->tme_sun2bw2_end_address);

    /* this TLB entry allows fast reading and writing: */
    tlb->tme_bus_tlb_emulator_off_read
      = (sun2bw2->tme_sun2bw2_fb_memory
	 - TME_SUN2BW2_REG_MEM);
    tlb->tme_bus_tlb_emulator_off_write
      = (sun2bw2->tme_sun2bw2_fb_memory
	 - TME_SUN2BW2_REG_MEM);
  }

  /* if this address falls in the pad memory: */
  else if ((sun2bw2->tme_sun2bw2_end_address
	    <= address)
	   && (address
	       < (TME_SUN2BW2_REG_MEM
		  + TME_SUN2BW2_SIZ_MEM))) {

    tlb->tme_bus_tlb_cycle = _tme_sun2bw2_bus_cycle_pad;

    /* this TLB entry covers this range: */
    TME_ATOMIC_WRITE(tme_bus_addr_t,
		     tlb->tme_bus_tlb_addr_first, 
		     sun2bw2->tme_sun2bw2_end_address);
    TME_ATOMIC_WRITE(tme_bus_addr_t,
		     tlb->tme_bus_tlb_addr_last,
		     (TME_SUN2BW2_REG_MEM
		      + TME_SUN2BW2_SIZ_MEM));

    /* this TLB entry allows fast reading and writing: */
    tlb->tme_bus_tlb_emulator_off_read
      = (sun2bw2->tme_sun2bw2_pad_memory
	 - sun2bw2->tme_sun2bw2_end_address);
    tlb->tme_bus_tlb_emulator_off_write
      = (sun2bw2->tme_sun2bw2_pad_memory
	 - sun2bw2->tme_sun2bw2_end_address);
  }

  /* the fast reading and writing rwlock: */
  tlb->tme_bus_tlb_rwlock = &sun2bw2->tme_sun2bw2_rwlock;

  /* allow reading and writing: */
  tlb->tme_bus_tlb_cycles_ok = TME_BUS_CYCLE_READ | TME_BUS_CYCLE_WRITE;

  /* our bus cycle handler private data: */
  tlb->tme_bus_tlb_cycle_private = _sun2bw2;

  return (TME_OK);
}

/* this makes a new framebuffer connection: */
static int
_tme_sun2bw2_connection_make(struct tme_connection *conn, unsigned int state)
{
  struct tme_sun2bw2 *sun2bw2;
  struct tme_fb_connection *conn_fb;
  struct tme_fb_connection *conn_fb_other;
  int rc;

  /* recover our data structures: */
  sun2bw2 = conn->tme_connection_element->tme_element_private;
  conn_fb = (struct tme_fb_connection *) conn;
  conn_fb_other = (struct tme_fb_connection *) conn->tme_connection_other;

  /* both sides must be framebuffer connections: */
  assert(conn->tme_connection_type == TME_CONNECTION_FRAMEBUFFER);
  assert(conn->tme_connection_other->tme_connection_type == TME_CONNECTION_FRAMEBUFFER);

  /* lock our mutex: */
  tme_mutex_lock(&sun2bw2->tme_sun2bw2_mutex);

  /* once the connection is made, we know whether or not the other
     side of the connection is supplying specific memory that it wants
     us to use, or if we should allocate memory ourselves: */
  if (conn_fb->tme_fb_connection_buffer == NULL) {
    rc = tme_fb_xlat_alloc_src(conn_fb);
    assert (rc == TME_OK);
    sun2bw2->tme_sun2bw2_fb_memory = conn_fb->tme_fb_connection_buffer;
  }

  /* we're always set up to answer calls across the connection, so we
     only have to do work when the connection has gone full, namely
     taking the other side of the connection: */
  if (state == TME_CONNECTION_FULL) {

    /* save our connection: */
    sun2bw2->tme_sun2bw2_fb_connection = conn_fb_other;
  }

  /* unlock our mutex: */
  tme_mutex_unlock(&sun2bw2->tme_sun2bw2_mutex);

  return (TME_OK);
}

/* this breaks a connection: */
static int
_tme_sun2bw2_connection_break(struct tme_connection *conn, unsigned int state)
{
  abort();
}

/* this makes a new connection side for a sun2bw2: */
static int
_tme_sun2bw2_connections_new(struct tme_element *element,
			     const char * const *args,
			     struct tme_connection **_conns,
			     char **_output)
{
  struct tme_sun2bw2 *sun2bw2;
  struct tme_fb_connection *conn_fb;
  struct tme_connection *conn;
  int rc;

  /* recover our data structure: */
  sun2bw2 = (struct tme_sun2bw2 *) element->tme_element_private;

  /* make the generic bus device connection side: */
  rc = tme_bus_device_connections_new(element, args, _conns, _output);
  if (rc != TME_OK) {
    return (rc);
  }

  /* if we don't have a framebuffer connection, make one: */
  if (sun2bw2->tme_sun2bw2_fb_connection == NULL) {

    /* allocate the new framebuffer connection: */
    conn_fb = tme_new0(struct tme_fb_connection, 1);
    conn = &conn_fb->tme_fb_connection;
    
    /* fill in the generic connection: */
    conn->tme_connection_next = *_conns;
    conn->tme_connection_type = TME_CONNECTION_FRAMEBUFFER;
    conn->tme_connection_score = tme_fb_connection_score;
    conn->tme_connection_make = _tme_sun2bw2_connection_make;
    conn->tme_connection_break = _tme_sun2bw2_connection_break;

    /* fill in the framebuffer connection: */
    conn_fb->tme_fb_connection_mode_change = NULL;
#ifdef TME_SUN2BW2_DEBUG
    conn_fb->tme_fb_connection_update = _tme_sun2bw2_update_debug;
#else  /* !TME_SUN2BW2_DEBUG */ 
    conn_fb->tme_fb_connection_update = NULL;
#endif /* !TME_SUN2BW2_DEBUG */ 

    /* height and width depend on the hires jumper: */
    if (sun2bw2->tme_sun2bw2_csr
	& TME_SUN2BW2_CSR_JUMPER_HIRES) {
      conn_fb->tme_fb_connection_width = TME_SUN2BW2_HI_WIDTH;
      conn_fb->tme_fb_connection_height = TME_SUN2BW2_HI_HEIGHT;
    }
    else {
      conn_fb->tme_fb_connection_width = TME_SUN2BW2_LO_WIDTH;
      conn_fb->tme_fb_connection_height = TME_SUN2BW2_LO_HEIGHT;
    }

    /* we are monochrome: */
    conn_fb->tme_fb_connection_depth = 1;
    conn_fb->tme_fb_connection_bits_per_pixel = 1;

    /* we skip no pixels at the start of the scanline: */
    conn_fb->tme_fb_connection_skipx = 0;

    /* we pad to 32-bit boundaries: */
    conn_fb->tme_fb_connection_scanline_pad = 32;

    /* we are big-endian: */
    conn_fb->tme_fb_connection_order = TME_ENDIAN_BIG;

    /* we don't allocate memory until the connection is made, in case
       the other side of the connection wants to provide us with a
       specific memory region to use (maybe we're on a system with a
       real bwtwo and we can write directly to its buffer): */
    conn_fb->tme_fb_connection_buffer = NULL;

    /* return the connection side possibility: */
    *_conns = conn;
  }

  /* done: */
  return (TME_OK);
}

/* the new _sun2bw2 function: */
TME_ELEMENT_SUB_NEW_DECL(tme_machine_sun2,bwtwo) {
  struct tme_sun2bw2 *sun2bw2;
  struct tme_bus_subregion *subregion;
  const char *bw2_type;
  int arg_i;
  int usage;

  /* check our arguments: */
  usage = 0;
  bw2_type = NULL;
  arg_i = 1;
  for (;;) {

    /* the framebuffer type: */
    if (TME_ARG_IS(args[arg_i + 0], "type")) {
      bw2_type = args[arg_i + 1];
      if (bw2_type == NULL
	  || (strcmp(bw2_type, "obmem")
	      && strcmp(bw2_type, "obio"))) {
	usage = TRUE;
	break;
      }
      arg_i += 2;
    }

    /* if we ran out of arguments: */
    else if (args[arg_i] == NULL) {

      break;
    }

    /* otherwise this is a bad argument: */
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
			    "%s %s type { obmem | obio }",
			    _("usage:"),
			    args[0]);
    return (EINVAL);
  }

  /* start the sun2bw2 structure: */
  sun2bw2 = tme_new0(struct tme_sun2bw2, 1);
  sun2bw2->tme_sun2bw2_element = element;
  tme_mutex_init(&sun2bw2->tme_sun2bw2_mutex);
  tme_rwlock_init(&sun2bw2->tme_sun2bw2_rwlock);

  /* set our initial CSR: */
  sun2bw2->tme_sun2bw2_csr
    = tme_htobe_u16(TME_SUN2BW2_CSR_ENABLE_VIDEO);

  /* if we're high-resolution: */
  if (sun2bw2->tme_sun2bw2_csr
      & TME_SUN2BW2_CSR_JUMPER_HIRES) {

    /* set the address after the end of displayed framebuffer memory: */
    sun2bw2->tme_sun2bw2_end_address
      = (TME_SUN2BW2_REG_MEM
	 + (TME_SUN2BW2_HI_WIDTH
	    * TME_SUN2BW2_HI_HEIGHT
	    / 8));

    /* we don't need any pad memory: */
    sun2bw2->tme_sun2bw2_pad_memory = NULL;
  }

  /* otherwise, we're low-resolution: */
  else {

    /* set the address after the end of displayed framebuffer memory: */
    sun2bw2->tme_sun2bw2_end_address
      = (TME_SUN2BW2_REG_MEM
	 + (TME_SUN2BW2_LO_WIDTH
	    * TME_SUN2BW2_LO_HEIGHT
	    / 8));

    /* allocate the pad memory: */
    sun2bw2->tme_sun2bw2_pad_memory
      = tme_new0(tme_uint8_t,
		 (TME_SUN2BW2_REG_MEM
		  + TME_SUN2BW2_SIZ_MEM
		  - sun2bw2->tme_sun2bw2_end_address));
  }

  /* initialize our simple bus device descriptor: */
  sun2bw2->tme_sun2bw2_device.tme_bus_device_element = element;
  sun2bw2->tme_sun2bw2_device.tme_bus_device_tlb_fill = _tme_sun2bw2_tlb_fill;
  subregion = &sun2bw2->tme_sun2bw2_device.tme_bus_device_subregions;
  subregion->tme_bus_subregion_address_first
    = TME_SUN2BW2_REG_MEM;
  subregion->tme_bus_subregion_address_last
    = TME_SUN2BW2_SIZ_MEM;
  subregion->tme_bus_subregion_next
    = (!strcmp(bw2_type, "obmem")
       ? &_tme_sun2bw2_subregion_csr_obmem
       : &_tme_sun2bw2_subregion_csr_obio);
  sun2bw2->tme_sun2bw2_csr_address
    = subregion->tme_bus_subregion_next->tme_bus_subregion_address_first;

  /* fill the element: */
  element->tme_element_private = sun2bw2;
  element->tme_element_connections_new = _tme_sun2bw2_connections_new;

  return (TME_OK);
}

