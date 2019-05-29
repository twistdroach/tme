/* $Id: sun-bwtwo.c,v 1.2 2005/04/30 15:12:56 fredette Exp $ */

/* machine/sun/sun-bwtwo.c - Sun bwtwo emulation: */

/*
 * Copyright (c) 2003, 2004 Matt Fredette
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
_TME_RCSID("$Id: sun-bwtwo.c,v 1.2 2005/04/30 15:12:56 fredette Exp $");

/* includes: */
#include <tme/machine/sun.h>
#include <tme/generic/bus-device.h>
#include <tme/generic/fb.h>
#include "sun-fb.h"

/* macros: */

/* bwtwo types: */
#define TME_SUNBW2_TYPE_NULL		(0)
#define TME_SUNBW2_TYPE_MULTIBUS	(1)
#define TME_SUNBW2_TYPE_OLD_ONBOARD	(2)
#define TME_SUNBW2_TYPE_ONBOARD		(3)
#define TME_SUNBW2_TYPE_P4		(4)

/* register offsets and sizes: */
#define TME_SUNBW2_REG_CSR_MULTIBUS	(0x81800)
#define TME_SUNBW2_REG_CSR_OLD_ONBOARD	(0x20000)
#define TME_SUNBW2_SIZ_CSR		(0x00002)
#define TME_SUNBW2_SIZ_CSR_PAGE		(0x00800)
#define TME_SUNBW2_REG_P4		(0x00000)
#define TME_SUNBW2_SIZ_P4_PAGE		(sizeof(tme_uint32_t))

/* the bits in the Multibus and old-onboard Control/Status register: */
#define TME_SUNBW2_CSR_ENABLE_VIDEO	(0x8000)	/* enable video */
#define TME_SUNBW2_CSR_ENABLE_COPY	(0x4000)	/* enable copy mode */
#define TME_SUNBW2_CSR_ENABLE_INT	(0x2000)	/* interrupt enable */
#define TME_SUNBW2_CSR_INT_ACTIVE	(0x1000)	/* interrupt is active */
#define TME_SUNBW2_CSR_JUMPER_B		(0x0800)	/* jumper B */
#define TME_SUNBW2_CSR_JUMPER_A		(0x0400)	/* jumper A */
#define TME_SUNBW2_CSR_JUMPER_COLOR	(0x0200)	/* jumper color */
#define TME_SUNBW2_CSR_JUMPER_HIRES	(0x0100)	/* jumper hires */
#define TME_SUNBW2_CSR_COPYBASE_MASK	(0x007E)	/* copybase mask */

#if 0
#define TME_SUNBW2_DEBUG
#endif

/* structures: */

/* the card: */
struct tme_sunbw2 {

  /* our simple bus device header: */
  struct tme_bus_device tme_sunbw2_device;
#define tme_sunbw2_element tme_sunbw2_device.tme_bus_device_element

  /* the mutex protecting the card: */
  tme_mutex_t tme_sunbw2_mutex;

  /* the rwlock protecting the card: */
  tme_rwlock_t tme_sunbw2_rwlock;

  /* the framebuffer connection: */
  struct tme_fb_connection *tme_sunbw2_fb_connection;

  /* the type of the bwtwo: */
  tme_uint32_t tme_sunbw2_type;

  /* the size of the bwtwo: */
  tme_uint32_t tme_sunbw2_size;

  /* the (relative) bus address of our csr register: */
  tme_bus_addr_t tme_sunbw2_csr_address;

  /* the (relative) bus addresses of the first byte of framebuffer
     memory, the last byte of displayed framebuffer memory, and the
     last byte of framebuffer memory: */
  tme_bus_addr_t tme_sunbw2_fb_address_first;
  tme_bus_addr_t tme_sunbw2_fb_address_last_displayed;
  tme_bus_addr_t tme_sunbw2_fb_address_last;

  /* the framebuffer memory: */
  tme_uint8_t *tme_sunbw2_fb_memory;

  /* any pad memory: */
  tme_uint8_t *tme_sunbw2_pad_memory;

  /* our csr: */
  tme_uint16_t tme_sunbw2_csr;

  /* our P4 register: */
  tme_uint32_t tme_sunbw2_p4;

  /* our second bus subregion: */
  struct tme_bus_subregion tme_sunbw2_subregion1;
};

#ifdef TME_SUNBW2_DEBUG
#define TME_SUNBW2_LO_WIDTH	(1152)
#define TME_SUNBW2_LO_HEIGHT	(900)
static int
_tme_sunbw2_update_debug(struct tme_fb_connection *conn_fb)
{
  struct tme_sunbw2 *sunbw2;
  static int y = -1;
  static int x;
  unsigned long pixel;
  unsigned int pixel_byte;
  tme_uint8_t pixel_bit;
  int box, box_y, box_x;

  sunbw2 = conn_fb->tme_fb_connection.tme_connection_element->tme_element_private;

  for (box = 0; box < 2; box++) {
    if (y < 0) {
      y = 16;
      x = 0;
      continue;
    }
    for (box_y = 0; box_y < 2; box_y++) {
      for (box_x = 0; box_x < 2; box_x++) {
	pixel = (((y + box_y)
		  * TME_SUNBW2_LO_WIDTH)
		 + x
		 + box_x);
	pixel_byte = (pixel / 8);
	pixel_bit = (0x80 >> (pixel % 8));
	sunbw2->tme_sunbw2_fb_memory[pixel_byte] ^= pixel_bit;
      }
    }
    if (box == 0) {
      x += 2;
      if (x == TME_SUNBW2_LO_WIDTH) {
	x = 0;
	y += 2;
	if (y == TME_SUNBW2_LO_HEIGHT) {
	  y = 0;
	}
      }
    }
  }
    
  return (TME_OK);
}
#undef TME_SUNBW2_LO_WIDTH
#undef TME_SUNBW2_LO_HEIGHT
#endif /* TME_SUNBW2_DEBUG */

/* the sunbw2 framebuffer bus cycle handler: */
static int
_tme_sunbw2_bus_cycle_fb(void *_sunbw2, struct tme_bus_cycle *cycle_init)
{
  struct tme_sunbw2 *sunbw2;

  /* recover our data structure: */
  sunbw2 = (struct tme_sunbw2 *) _sunbw2;

  /* lock the mutex: */
  tme_mutex_lock(&sunbw2->tme_sunbw2_mutex);

  /* run the cycle: */
  assert (cycle_init->tme_bus_cycle_address
	  >= sunbw2->tme_sunbw2_fb_address_first);
  tme_bus_cycle_xfer_memory(cycle_init, 
			    (sunbw2->tme_sunbw2_fb_memory
			     - sunbw2->tme_sunbw2_fb_address_first),
			    sunbw2->tme_sunbw2_fb_address_last_displayed);
  
  /* unlock the mutex: */
  tme_mutex_unlock(&sunbw2->tme_sunbw2_mutex);

  /* no faults: */
  return (TME_OK);
}

/* the sunbw2 pad bus cycle handler: */
static int
_tme_sunbw2_bus_cycle_pad(void *_sunbw2, struct tme_bus_cycle *cycle_init)
{
  struct tme_sunbw2 *sunbw2;

  /* recover our data structure: */
  sunbw2 = (struct tme_sunbw2 *) _sunbw2;

  /* lock the mutex: */
  tme_mutex_lock(&sunbw2->tme_sunbw2_mutex);

  /* run the cycle: */
  assert (cycle_init->tme_bus_cycle_address
	  > sunbw2->tme_sunbw2_fb_address_last_displayed);
  tme_bus_cycle_xfer_memory(cycle_init, 
			    (sunbw2->tme_sunbw2_pad_memory
			     - (sunbw2->tme_sunbw2_fb_address_last_displayed
				+ 1)),
			    sunbw2->tme_sunbw2_fb_address_last);

  /* unlock the mutex: */
  tme_mutex_unlock(&sunbw2->tme_sunbw2_mutex);

  /* no faults: */
  return (TME_OK);
}

/* the sunbw2 CSR bus cycle handler: */
static int
_tme_sunbw2_bus_cycle_csr(void *_sunbw2, struct tme_bus_cycle *cycle_init)
{
  struct tme_sunbw2 *sunbw2;
  tme_uint16_t csr_old, csr_new;
  tme_bus_addr_t undecoded;

  /* recover our data structure: */
  sunbw2 = (struct tme_sunbw2 *) _sunbw2;

  /* lock the mutex: */
  tme_mutex_lock(&sunbw2->tme_sunbw2_mutex);

  /* get the old CSR value: */
  csr_old = tme_betoh_u16(sunbw2->tme_sunbw2_csr);

  /* the entire 2KB (one page's) worth of addresses at
     tme_sunbw2_csr_address are all decoded (or, rather, not decoded)
     as the CSR: */
  undecoded
    = (cycle_init->tme_bus_cycle_address
       & (TME_SUNBW2_SIZ_CSR_PAGE - sizeof(sunbw2->tme_sunbw2_csr)));
  cycle_init->tme_bus_cycle_address
    -= undecoded;

  /* run the cycle: */
  assert (cycle_init->tme_bus_cycle_address
	  >= sunbw2->tme_sunbw2_csr_address);
  tme_bus_cycle_xfer_memory(cycle_init, 
			    (((tme_uint8_t *) &sunbw2->tme_sunbw2_csr)
			     - sunbw2->tme_sunbw2_csr_address),
			    (sunbw2->tme_sunbw2_csr_address
			     + sizeof(sunbw2->tme_sunbw2_csr)
			     - 1));
  cycle_init->tme_bus_cycle_address
    += undecoded;

  /* get the new CSR value: */
  csr_new = tme_betoh_u16(sunbw2->tme_sunbw2_csr);

  /* put back the unchanging bits: */
  csr_new
    = ((csr_new
	& ~(TME_SUNBW2_CSR_INT_ACTIVE
	    | TME_SUNBW2_CSR_JUMPER_B
	    | TME_SUNBW2_CSR_JUMPER_A
	    | TME_SUNBW2_CSR_JUMPER_COLOR
	    | TME_SUNBW2_CSR_JUMPER_HIRES))
       | (csr_old
	  & (TME_SUNBW2_CSR_INT_ACTIVE
	     | TME_SUNBW2_CSR_JUMPER_B
	     | TME_SUNBW2_CSR_JUMPER_A
	     | TME_SUNBW2_CSR_JUMPER_COLOR
	     | TME_SUNBW2_CSR_JUMPER_HIRES)));

  /* we do not support these bits: */
  if (csr_new
      & (TME_SUNBW2_CSR_ENABLE_COPY
	 | TME_SUNBW2_CSR_ENABLE_INT)) {
    abort();
  }

  /* set the new CSR value: */
  sunbw2->tme_sunbw2_csr = tme_htobe_u16(csr_new);

  /* unlock the mutex: */
  tme_mutex_unlock(&sunbw2->tme_sunbw2_mutex);

  /* no faults: */
  return (TME_OK);
}

/* the sunbw2 P4 bus cycle handler: */
static int
_tme_sunbw2_bus_cycle_p4(void *_sunbw2, struct tme_bus_cycle *cycle_init)
{
  struct tme_sunbw2 *sunbw2;
  tme_uint32_t p4_old, p4_new;
  tme_bus_addr_t undecoded;

  /* recover our data structure: */
  sunbw2 = (struct tme_sunbw2 *) _sunbw2;

  /* lock the mutex: */
  tme_mutex_lock(&sunbw2->tme_sunbw2_mutex);

  /* get the old P4 value: */
  p4_old = tme_betoh_u32(sunbw2->tme_sunbw2_p4);

  /* the entire ?KB (one page's) worth of addresses at
     TME_SUNBW2_REG_P4 are all decoded (or, rather, not decoded)
     as the P4: */
  undecoded
    = (cycle_init->tme_bus_cycle_address
       & (TME_SUNBW2_SIZ_P4_PAGE - sizeof(sunbw2->tme_sunbw2_p4)));
  cycle_init->tme_bus_cycle_address
    -= undecoded;

  /* run the cycle: */
  assert (cycle_init->tme_bus_cycle_address
	  >= TME_SUNBW2_REG_P4);
  tme_bus_cycle_xfer_memory(cycle_init, 
			    (((tme_uint8_t *) &sunbw2->tme_sunbw2_p4)
			     - TME_SUNBW2_REG_P4),
			    (TME_SUNBW2_REG_P4
			     + sizeof(sunbw2->tme_sunbw2_p4)
			     - 1));
  cycle_init->tme_bus_cycle_address
    += undecoded;

  /* get the new P4 value: */
  p4_new = tme_betoh_u32(sunbw2->tme_sunbw2_p4);

  /* put back the unchanging bits: */
  p4_new
    = ((p4_new
	& ~TME_SUN_P4_RO_MASK)
       | (p4_old
	  & TME_SUN_P4_RO_MASK));

  /* we do not support these bits: */
  if (p4_new
      & (TME_SUN_P4_REG_SYNC_RAMDAC
	 | TME_SUN_P4_REG_ENABLE_INT)) {
    abort();
  }

  /* set the new P4 value: */
  sunbw2->tme_sunbw2_p4 = tme_htobe_u32(p4_new);

  /* unlock the mutex: */
  tme_mutex_unlock(&sunbw2->tme_sunbw2_mutex);

  /* no faults: */
  return (TME_OK);
}

/* the sunbw2 TLB filler: */
static int
_tme_sunbw2_tlb_fill(void *_sunbw2, struct tme_bus_tlb *tlb, 
		      tme_bus_addr_t address, unsigned int cycles)
{
  struct tme_sunbw2 *sunbw2;

  /* recover our data structure: */
  sunbw2 = (struct tme_sunbw2 *) _sunbw2;

  /* initialize the TLB entry: */
  tme_bus_tlb_initialize(tlb);

  /* if this is a Multibus or old-onboard bwtwo, and the address falls in the CSR: */
  if (((sunbw2->tme_sunbw2_type
	== TME_SUNBW2_TYPE_MULTIBUS)
       || (sunbw2->tme_sunbw2_type
	   == TME_SUNBW2_TYPE_OLD_ONBOARD))
      && (sunbw2->tme_sunbw2_csr_address
	  <= address)
      && (address
	  < (sunbw2->tme_sunbw2_csr_address
	     + TME_SUNBW2_SIZ_CSR_PAGE))) {

    tlb->tme_bus_tlb_cycle = _tme_sunbw2_bus_cycle_csr;

    /* this TLB entry covers this range: */
    TME_ATOMIC_WRITE(tme_bus_addr_t, 
		     tlb->tme_bus_tlb_addr_first,
		     sunbw2->tme_sunbw2_csr_address);
    TME_ATOMIC_WRITE(tme_bus_addr_t, 
		     tlb->tme_bus_tlb_addr_last, 
		     (sunbw2->tme_sunbw2_csr_address
		      + TME_SUNBW2_SIZ_CSR
		      - 1));

    /* this TLB entry cannot allow fast reading, since the page the
       CSR is on isn't fully decoded - all words on the 2KB page are
       the CSR: */
  }

  /* if this is a P4 bwtwo, and the address falls in the P4 register: */
  else if ((sunbw2->tme_sunbw2_type
	    == TME_SUNBW2_TYPE_P4)
	   && (address
	       < (TME_SUNBW2_REG_P4
		  + TME_SUNBW2_SIZ_P4_PAGE))) {

    tlb->tme_bus_tlb_cycle = _tme_sunbw2_bus_cycle_p4;

    /* this TLB entry covers this range: */
    TME_ATOMIC_WRITE(tme_bus_addr_t, 
		     tlb->tme_bus_tlb_addr_first,
		     TME_SUNBW2_REG_P4);
    TME_ATOMIC_WRITE(tme_bus_addr_t, 
		     tlb->tme_bus_tlb_addr_last, 
		     (TME_SUNBW2_REG_P4
		      + TME_SUNBW2_SIZ_P4_PAGE
		      - 1));

    /* this TLB entry cannot allow fast reading, since the page the
       P4 register is on isn't fully decoded - all words on the ?KB page are
       the P4 register: */
  }

  /* if this address falls in the displayed framebuffer memory: */
  else if ((sunbw2->tme_sunbw2_fb_address_first
	    <= address)
	   && (address
	       <= sunbw2->tme_sunbw2_fb_address_last_displayed)) {

    assert (sunbw2->tme_sunbw2_fb_connection != NULL);

    tlb->tme_bus_tlb_cycle = _tme_sunbw2_bus_cycle_fb;

    /* this TLB entry covers this range: */
    TME_ATOMIC_WRITE(tme_bus_addr_t,
		     tlb->tme_bus_tlb_addr_first, 
		     sunbw2->tme_sunbw2_fb_address_first);
    TME_ATOMIC_WRITE(tme_bus_addr_t,
		     tlb->tme_bus_tlb_addr_last,
		     sunbw2->tme_sunbw2_fb_address_last_displayed);

    /* this TLB entry allows fast reading and writing: */
    tlb->tme_bus_tlb_emulator_off_read
      = (sunbw2->tme_sunbw2_fb_memory
	 - sunbw2->tme_sunbw2_fb_address_first);
    tlb->tme_bus_tlb_emulator_off_write
      = (sunbw2->tme_sunbw2_fb_memory
	 - sunbw2->tme_sunbw2_fb_address_first);
  }

  /* if this address falls in the pad memory: */
  else if ((sunbw2->tme_sunbw2_fb_address_last_displayed
	    < address)
	   && (address
	       <= sunbw2->tme_sunbw2_fb_address_last)) {

    tlb->tme_bus_tlb_cycle = _tme_sunbw2_bus_cycle_pad;

    /* this TLB entry covers this range: */
    TME_ATOMIC_WRITE(tme_bus_addr_t,
		     tlb->tme_bus_tlb_addr_first, 
		     (sunbw2->tme_sunbw2_fb_address_last_displayed
		      + 1));
    TME_ATOMIC_WRITE(tme_bus_addr_t,
		     tlb->tme_bus_tlb_addr_last,
		     sunbw2->tme_sunbw2_fb_address_last);

    /* this TLB entry allows fast reading and writing: */
    tlb->tme_bus_tlb_emulator_off_read
      = (sunbw2->tme_sunbw2_pad_memory
	 - (sunbw2->tme_sunbw2_fb_address_last_displayed
	    + 1));
    tlb->tme_bus_tlb_emulator_off_write
      = (sunbw2->tme_sunbw2_pad_memory
	 - (sunbw2->tme_sunbw2_fb_address_last_displayed
	    + 1));
  }

  /* the fast reading and writing rwlock: */
  tlb->tme_bus_tlb_rwlock = &sunbw2->tme_sunbw2_rwlock;

  /* allow reading and writing: */
  tlb->tme_bus_tlb_cycles_ok = TME_BUS_CYCLE_READ | TME_BUS_CYCLE_WRITE;

  /* our bus cycle handler private data: */
  tlb->tme_bus_tlb_cycle_private = _sunbw2;

  return (TME_OK);
}

/* this makes a new framebuffer connection: */
static int
_tme_sunbw2_connection_make(struct tme_connection *conn, unsigned int state)
{
  struct tme_sunbw2 *sunbw2;
  struct tme_fb_connection *conn_fb;
  struct tme_fb_connection *conn_fb_other;
  int rc;

  /* recover our data structures: */
  sunbw2 = conn->tme_connection_element->tme_element_private;
  conn_fb = (struct tme_fb_connection *) conn;
  conn_fb_other = (struct tme_fb_connection *) conn->tme_connection_other;

  /* both sides must be framebuffer connections: */
  assert(conn->tme_connection_type == TME_CONNECTION_FRAMEBUFFER);
  assert(conn->tme_connection_other->tme_connection_type == TME_CONNECTION_FRAMEBUFFER);

  /* lock our mutex: */
  tme_mutex_lock(&sunbw2->tme_sunbw2_mutex);

  /* once the connection is made, we know whether or not the other
     side of the connection is supplying specific memory that it wants
     us to use, or if we should allocate memory ourselves: */
  if (conn_fb->tme_fb_connection_buffer == NULL) {
    rc = tme_fb_xlat_alloc_src(conn_fb);
    assert (rc == TME_OK);
  }
  sunbw2->tme_sunbw2_fb_memory = conn_fb->tme_fb_connection_buffer;

  /* we're always set up to answer calls across the connection, so we
     only have to do work when the connection has gone full, namely
     taking the other side of the connection: */
  if (state == TME_CONNECTION_FULL) {

    /* save our connection: */
    sunbw2->tme_sunbw2_fb_connection = conn_fb_other;
  }

  /* unlock our mutex: */
  tme_mutex_unlock(&sunbw2->tme_sunbw2_mutex);

  return (TME_OK);
}

/* this breaks a connection: */
static int
_tme_sunbw2_connection_break(struct tme_connection *conn, unsigned int state)
{
  abort();
}

/* this makes a new connection side for a sunbw2: */
static int
_tme_sunbw2_connections_new(struct tme_element *element,
			     const char * const *args,
			     struct tme_connection **_conns,
			     char **_output)
{
  struct tme_sunbw2 *sunbw2;
  struct tme_fb_connection *conn_fb;
  struct tme_connection *conn;
  int rc;

  /* recover our data structure: */
  sunbw2 = (struct tme_sunbw2 *) element->tme_element_private;

  /* make the generic bus device connection side: */
  rc = tme_bus_device_connections_new(element, args, _conns, _output);
  if (rc != TME_OK) {
    return (rc);
  }

  /* if we don't have a framebuffer connection, make one: */
  if (sunbw2->tme_sunbw2_fb_connection == NULL) {

    /* allocate the new framebuffer connection: */
    conn_fb = tme_new0(struct tme_fb_connection, 1);
    conn = &conn_fb->tme_fb_connection;
    
    /* fill in the generic connection: */
    conn->tme_connection_next = *_conns;
    conn->tme_connection_type = TME_CONNECTION_FRAMEBUFFER;
    conn->tme_connection_score = tme_fb_connection_score;
    conn->tme_connection_make = _tme_sunbw2_connection_make;
    conn->tme_connection_break = _tme_sunbw2_connection_break;

    /* fill in the framebuffer connection: */
    conn_fb->tme_fb_connection_mode_change = NULL;
#ifdef TME_SUNBW2_DEBUG
    conn_fb->tme_fb_connection_update = _tme_sunbw2_update_debug;
#else  /* !TME_SUNBW2_DEBUG */ 
    conn_fb->tme_fb_connection_update = NULL;
#endif /* !TME_SUNBW2_DEBUG */ 

    /* height and width: */
    conn_fb->tme_fb_connection_width = tme_sun_fb_p4_size_width(sunbw2->tme_sunbw2_size);
    conn_fb->tme_fb_connection_height = tme_sun_fb_p4_size_height(sunbw2->tme_sunbw2_size);

    /* we are monochrome: */
    conn_fb->tme_fb_connection_class = TME_FB_XLAT_CLASS_MONOCHROME;
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

    /* our pixels don't have subfields: */
    conn_fb->tme_fb_connection_mask_g = 0;
    conn_fb->tme_fb_connection_mask_r = 0;
    conn_fb->tme_fb_connection_mask_b = 0;

    /* intensities are a single bit, linearly mapped, but inverted: */
    conn_fb->tme_fb_connection_map_bits = 1;
    conn_fb->tme_fb_connection_map_g = NULL;
    conn_fb->tme_fb_connection_map_r = NULL;
    conn_fb->tme_fb_connection_map_b = NULL;
    conn_fb->tme_fb_connection_inverted = TRUE;

    /* return the connection side possibility: */
    *_conns = conn;
  }

  /* done: */
  return (TME_OK);
}

/* the new sun bwtwo function: */
int
tme_sun_bwtwo(struct tme_element *element, const char * const *args, char **_output)
{
  struct tme_sunbw2 *sunbw2;
  struct tme_bus_subregion *fb_subregion;
  struct tme_bus_subregion *reg_subregion;
  tme_uint32_t bw2_type;
  tme_uint32_t bw2_size;
  tme_bus_addr_t fb_size;
  int arg_i;
  int usage;

  /* check our arguments: */
  usage = 0;
  bw2_type = TME_SUNBW2_TYPE_NULL;
  bw2_size = TME_SUN_P4_SIZE_1152_900;
  arg_i = 1;
  for (;;) {

    /* the framebuffer type: */
    if (TME_ARG_IS(args[arg_i + 0], "type")) {
      if (TME_ARG_IS(args[arg_i + 1], "multibus")) {
	bw2_type = TME_SUNBW2_TYPE_MULTIBUS;
      }
      else if (TME_ARG_IS(args[arg_i + 1], "old-onboard")) {
	bw2_type = TME_SUNBW2_TYPE_OLD_ONBOARD;
      }
      else if (TME_ARG_IS(args[arg_i + 1], "onboard")) {
	bw2_type = TME_SUNBW2_TYPE_ONBOARD;
      }
      else if (TME_ARG_IS(args[arg_i + 1], "P4")) {
	bw2_type = TME_SUNBW2_TYPE_P4;
      }
      else {
	usage = TRUE;
	break;
      }
      arg_i += 2;
    }

    /* the framebuffer size: */
    else if (TME_ARG_IS(args[arg_i + 0], "size")) {
      bw2_size = tme_sun_fb_p4_size(args[arg_i + 1]);
      if (bw2_size == TME_SUN_P4_SIZE_NULL) {
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

  /* dispatch on the bwtwo type to check that it and the size are
     valid: */
  switch (bw2_type) {

    /* no bwtwo type was specified: */
  case TME_SUNBW2_TYPE_NULL:
    /* XXX TBD */
    usage = TRUE;
    break;

    /* the original Multibus bwtwo and onboard bwtwo only support
       1152x900 and 1024x1024: */
  case TME_SUNBW2_TYPE_MULTIBUS:
  case TME_SUNBW2_TYPE_OLD_ONBOARD:
    if (bw2_size != TME_SUN_P4_SIZE_1152_900
	&& bw2_size != TME_SUN_P4_SIZE_1024_1024) {
      /* XXX TBD */
      usage = TRUE;
    }
    break;

    /* the sizes supported by a CSR-less bwtwo appear to depend on the 
       actual model; we assume the user knows what he is doing: */
  case TME_SUNBW2_TYPE_ONBOARD:
    break;

    /* we allow creating a P4 bwtwo with any of the P4 sizes, again
       assuming that the user knows what he is doing: */
  case TME_SUNBW2_TYPE_P4:
    break;

  default:
    assert(FALSE);
    break;
  }

  if (usage) {
    tme_output_append_error(_output, 
			    "%s %s type { multibus | old-onboard | onboard | P4 } [ size { 1600x1280 | 1152x900 | 1024x1024 | 1280x1024 | 1440x1440 | 640x480 } ]",
			    _("usage:"),
			    args[0]);
    return (EINVAL);
  }

  /* start the sunbw2 structure: */
  sunbw2 = tme_new0(struct tme_sunbw2, 1);
  sunbw2->tme_sunbw2_element = element;
  tme_mutex_init(&sunbw2->tme_sunbw2_mutex);
  tme_rwlock_init(&sunbw2->tme_sunbw2_rwlock);

  /* set the bwtwo type: */
  sunbw2->tme_sunbw2_type = bw2_type;

  /* set the bwtwo size: */
  sunbw2->tme_sunbw2_size = bw2_size;

  /* assume the (relative) bus address of the first byte of displayed
     framebuffer memory: */
  sunbw2->tme_sunbw2_fb_address_first = 0;

  /* assume that the number of bytes of framebuffer memory is the
     minimum number of bytes required, rounded up to the nearest power
     of two: */
  fb_size = ((tme_sun_fb_p4_size_width(bw2_size)
	      * tme_sun_fb_p4_size_height(bw2_size))
	     / 8);
  if ((fb_size & (fb_size - 1)) != 0) {
    for (; (fb_size & (fb_size - 1)) != 0; fb_size &= (fb_size - 1));
    fb_size <<= 1;
  }

  /* assume that we will attach to two bus subregions, and that the
     framebuffer memory will be first: */
  fb_subregion = &sunbw2->tme_sunbw2_device.tme_bus_device_subregions;
  fb_subregion->tme_bus_subregion_next = &sunbw2->tme_sunbw2_subregion1;
  sunbw2->tme_sunbw2_subregion1.tme_bus_subregion_next = NULL;

  /* dispatch on the bwtwo type: */
  switch (bw2_type) {
  case TME_SUNBW2_TYPE_MULTIBUS:
  case TME_SUNBW2_TYPE_OLD_ONBOARD:

    /* set our initial CSR: */
    sunbw2->tme_sunbw2_csr
      = tme_htobe_u16(TME_SUNBW2_CSR_ENABLE_VIDEO
		      | (bw2_size == TME_SUN_P4_SIZE_1024_1024
			 ? TME_SUNBW2_CSR_JUMPER_HIRES
			 : 0));

    /* set our CSR address: */
    sunbw2->tme_sunbw2_csr_address
      = (bw2_type == TME_SUNBW2_TYPE_MULTIBUS
	 ? TME_SUNBW2_REG_CSR_MULTIBUS
	 : TME_SUNBW2_REG_CSR_OLD_ONBOARD);

    /* our second bus subregion is for the CSR: */
    reg_subregion = &sunbw2->tme_sunbw2_subregion1;
    reg_subregion->tme_bus_subregion_address_first
      = sunbw2->tme_sunbw2_csr_address;
    reg_subregion->tme_bus_subregion_address_last
      = (reg_subregion->tme_bus_subregion_address_first
	 + TME_SUNBW2_SIZ_CSR_PAGE
	 - 1);
    break;

  case TME_SUNBW2_TYPE_ONBOARD:

    /* we attach to only one bus subregion: */
    fb_subregion->tme_bus_subregion_next = NULL;
    break;

  case TME_SUNBW2_TYPE_P4:

    /* set our initial P4 register: */
    sunbw2->tme_sunbw2_p4
      = tme_htobe_u16(TME_SUN_P4_ID_BWTWO
		      | bw2_size
		      | TME_SUN_P4_REG_ENABLE_VIDEO);

    /* the framebuffer memory begins at a fixed offset after the P4 register: */
    sunbw2->tme_sunbw2_fb_address_first = TME_SUN_P4_OFFSET_BWTWO;

    /* we attach the P4 register first, and the framebuffer memory second: */
    reg_subregion = &sunbw2->tme_sunbw2_device.tme_bus_device_subregions;
    fb_subregion = &sunbw2->tme_sunbw2_subregion1;
    reg_subregion->tme_bus_subregion_address_first = 0;
    reg_subregion->tme_bus_subregion_address_last
      = (reg_subregion->tme_bus_subregion_address_first
	 + sizeof(sunbw2->tme_sunbw2_p4)
	 - 1);
    break;
  }

  /* set the (relative) bus address of the last byte of displayed
     framebuffer memory: */
  sunbw2->tme_sunbw2_fb_address_last_displayed
    = (sunbw2->tme_sunbw2_fb_address_first
       + ((tme_sun_fb_p4_size_width(bw2_size)
	   * tme_sun_fb_p4_size_height(bw2_size))
	  / 8)
       - 1);

  /* set the (relative) bus address of the last byte of framebuffer
     memory: */
  sunbw2->tme_sunbw2_fb_address_last
    = (sunbw2->tme_sunbw2_fb_address_first
       + fb_size
       - 1);

  /* finish the framebuffer memory subregion: */
  fb_subregion->tme_bus_subregion_address_first
    = sunbw2->tme_sunbw2_fb_address_first;
  fb_subregion->tme_bus_subregion_address_last
    = sunbw2->tme_sunbw2_fb_address_last;

  /* assume that we don't need any pad (undisplayed) framebuffer memory: */
  sunbw2->tme_sunbw2_pad_memory = NULL;
  assert (sunbw2->tme_sunbw2_fb_address_last
	  >= sunbw2->tme_sunbw2_fb_address_last_displayed);

  /* if we need to, allocate pad (undisplayed) framebuffer memory: */
  if (sunbw2->tme_sunbw2_fb_address_last
      > sunbw2->tme_sunbw2_fb_address_last_displayed) {

    /* allocate the pad memory: */
    sunbw2->tme_sunbw2_pad_memory
      = tme_new0(tme_uint8_t,
		 (sunbw2->tme_sunbw2_fb_address_last
		  - sunbw2->tme_sunbw2_fb_address_last_displayed));
  }

  /* initialize our simple bus device descriptor: */
  sunbw2->tme_sunbw2_device.tme_bus_device_element = element;
  sunbw2->tme_sunbw2_device.tme_bus_device_tlb_fill = _tme_sunbw2_tlb_fill;

  /* fill the element: */
  element->tme_element_private = sunbw2;
  element->tme_element_connections_new = _tme_sunbw2_connections_new;

  return (TME_OK);
}

