/* $Id: sun2-mmu.c,v 1.13 2007/02/15 01:34:34 fredette Exp $ */

/* machine/sun2/sun2-mmu.c - implementation of Sun 2 MMU emulation: */

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
_TME_RCSID("$Id: sun2-mmu.c,v 1.13 2007/02/15 01:34:34 fredette Exp $");

/* includes: */
#include "sun2-impl.h"

/* macros: */

/* real PTE entry bits: */
#define TME_SUN2_PTE_VALID		0x80000000
#define TME_SUN2_PTE_PROT		0x7C000000
#define TME_SUN2_PTE_FOD		0x02000000
#define TME_SUN2_PTE_PGTYPE		0x00C00000
#define  TME_SUN2_PTE_PGTYPE_MASK	 0x00000003
#define TME_SUN2_PTE_REF		0x00200000
#define TME_SUN2_PTE_MOD		0x00100000
#define TME_SUN2_PTE_PGFRAME		0x00000FFF

/* real PTE page types: */
#define TME_SUN2_PGTYPE_OBMEM	(0)
#define TME_SUN2_PGTYPE_OBIO	(1)
#define TME_SUN2_PGTYPE_MBMEM	(2)
#define TME_SUN2_PGTYPE_VME0	(2)
#define TME_SUN2_PGTYPE_MBIO	(3)
#define TME_SUN2_PGTYPE_VME8	(3)

/* real bus error register bits: */
#define TME_SUN2_BUSERR_PARERR_L	TME_BIT(0)	/* parity error, lower byte */
#define TME_SUN2_BUSERR_PARERR_U	TME_BIT(1)	/* parity error, upper byte */
#define TME_SUN2_BUSERR_TIMEOUT		TME_BIT(2)	/* bus access timed out */
#define TME_SUN2_BUSERR_PROTERR		TME_BIT(3)	/* protection error */
#define TME_SUN2_BUSERR_VMEBUSERR	TME_BIT(6)	/* bus error signaled on VMEbus */
#define TME_SUN2_BUSERR_VALID		TME_BIT(7)	/* page map was valid */

/* this logs a bus error: */
#ifndef TME_NO_LOG
static void
_tme_sun2_bus_fault_log(struct tme_sun2 *sun2, struct tme_bus_tlb *tlb, struct tme_bus_cycle *cycle)
{
  tme_bus_addr_t virtual_address;
  struct tme_sun_mmu_pte pte;
  tme_uint32_t pte_sun2;
  const char *bus_name;
  tme_bus_addr_t physical_address;
  int rc;

  /* this silences gcc -Wuninitialized: */
  bus_name = NULL;

  /* recover the virtual address used: */
  virtual_address = cycle->tme_bus_cycle_address - tlb->tme_bus_tlb_addr_offset;

  /* look up the PTE involved.  since this is a real bus error, and
     not a protection violation or page not present bus error, we
     assume the system context: */
  rc = tme_sun_mmu_pte_get(sun2->tme_sun2_mmu, 
			   sun2->tme_sun2_context_system,
			   virtual_address,
			   &pte);
  assert(rc == TME_OK);
  pte_sun2 = pte.tme_sun_mmu_pte_raw;
    
  /* form the physical address and get the bus name: */
  physical_address = (((pte_sun2 & TME_SUN2_PTE_PGFRAME) << TME_SUN2_PAGE_SIZE_LOG2)
		      | (virtual_address & (TME_SUN2_PAGE_SIZE - 1)));
  switch ((pte_sun2 & TME_SUN2_PTE_PGTYPE) / (TME_SUN2_PTE_PGTYPE / TME_SUN2_PTE_PGTYPE_MASK)) {
  case TME_SUN2_PGTYPE_OBMEM: bus_name = "obmem"; break;
  case TME_SUN2_PGTYPE_OBIO: bus_name = "obio"; break;
  case TME_SUN2_PGTYPE_MBMEM:
    if (sun2->tme_sun2_has_vme) {
      bus_name = "VME";
    }
    else {
      bus_name = "mbmem";
    }
    break;
  case TME_SUN2_PGTYPE_MBIO:
    if (sun2->tme_sun2_has_vme) {
      bus_name = "VME";
      physical_address |= 0x800000;
    }
    else {
      bus_name = "mbio";
    }
    break;
  }

  /* log this bus error: */
  tme_log(TME_SUN2_LOG_HANDLE(sun2), 1000, TME_OK,
	  (TME_SUN2_LOG_HANDLE(sun2), 
	   _("%s bus error, physical 0x%08x, virtual 0x%08x, buserr = 0x%02x"),
	   bus_name,
	   physical_address,
	   virtual_address,
	   sun2->tme_sun2_buserr));
}
#else  /* TME_NO_LOG */
#define _tme_sun2_bus_fault_log(a, b, c) do { } while (/* CONSTCOND */ 0)
#endif /* TME_NO_LOG */

/* our general bus fault handler: */
static int
_tme_sun2_bus_fault_handler(struct tme_sun2 *sun2, 
			    struct tme_bus_tlb *tlb,
			    struct tme_bus_cycle *cycle,
			    int rc)
{
  tme_uint16_t buserr;

  /* dispatch on our fault code: */
  switch (rc) {

    /* bus address nonexistent: */
  case ENOENT:
    buserr = TME_SUN2_BUSERR_VALID | TME_SUN2_BUSERR_TIMEOUT;
    break;

    /* anything else is just a fault: */
  default:
    buserr = TME_SUN2_BUSERR_VALID;
    break;
  }

  /* set the bus error register: */
  sun2->tme_sun2_buserr = buserr;

  /* log the fault: */
  _tme_sun2_bus_fault_log(sun2, tlb, cycle);

  return (rc);
}

/* our obio bus fault handler: */
static int
_tme_sun2_obio_fault_handler(void *_sun2, struct tme_bus_tlb *tlb, struct tme_bus_cycle *cycle, int rc)
{
  tme_uint8_t all_bits_one[sizeof(tme_uint16_t)];

  /* the sun2 obio bus doesn't generate bus errors, it just reads
     all-bits-one: */
  memset(all_bits_one, 0xff, sizeof(all_bits_one));
  tme_bus_cycle_xfer_memory(cycle, 
			    &all_bits_one[0] - cycle->tme_bus_cycle_address,
			    cycle->tme_bus_cycle_address + sizeof(all_bits_one));
  return (TME_OK);
}

/* our obmem bus fault handler: */
static int
_tme_sun2_obmem_fault_handler(void *_sun2, struct tme_bus_tlb *tlb, struct tme_bus_cycle *cycle, int rc)
{
  tme_uint8_t all_bits_one[sizeof(tme_uint16_t)];

  /* the sun2 obmem bus apparently doesn't generate bus errors below
     0x700000, and instead just reads all-bits-one: */
  if (cycle->tme_bus_cycle_address < 0x700000) {
    memset(all_bits_one, 0xff, sizeof(all_bits_one));
    tme_bus_cycle_xfer_memory(cycle, 
			      &all_bits_one[0] - cycle->tme_bus_cycle_address,
			      cycle->tme_bus_cycle_address + sizeof(all_bits_one));
    return (TME_OK);
  }

  /* call the common bus fault handler: */
  return (_tme_sun2_bus_fault_handler((struct tme_sun2 *) _sun2, tlb, cycle, rc));
}

/* our Multibus fault handler: */
static int
_tme_sun2_multibus_fault_handler(void *_sun2, struct tme_bus_tlb *tlb, struct tme_bus_cycle *cycle, int rc)
{

  /* call the common bus fault handler: */
  return (_tme_sun2_bus_fault_handler((struct tme_sun2 *) _sun2, tlb, cycle, rc));
}

/* our VMEbus fault handler: */
static int
_tme_sun2_vmebus_fault_handler(void *_sun2, struct tme_bus_tlb *tlb, struct tme_bus_cycle *cycle, int rc)
{
  struct tme_sun2 *sun2;

  /* recover our sun2: */
  sun2 = (struct tme_sun2 *) _sun2;

  /* call the common bus fault handler: */
  rc = _tme_sun2_bus_fault_handler((struct tme_sun2 *) _sun2, tlb, cycle, rc);

  /* this bus fault happened on the VMEbus: */
  sun2->tme_sun2_buserr |= TME_SUN2_BUSERR_VMEBUSERR;

  /* return the fault: */
  return (rc);
}

/* our page-invalid cycle handler: */
static int
_tme_sun2_mmu_invalid(void *_sun2, struct tme_bus_cycle *cycle)
{
  struct tme_sun2 *sun2;

  /* recover our sun2: */
  sun2 = (struct tme_sun2 *) _sun2;

  /* log this bus error: */
  tme_log(TME_SUN2_LOG_HANDLE(sun2), 1000, TME_OK,
	  (TME_SUN2_LOG_HANDLE(sun2), 
	   _("page invalid bus error")));

  /* set the bus error register: */
  sun2->tme_sun2_buserr = TME_SUN2_BUSERR_PROTERR;

  /* return the fault: */
  return (EFAULT);
}

/* our protection error cycle handler: */
static int
_tme_sun2_mmu_proterr(void *_sun2, struct tme_bus_cycle *cycle)
{
  struct tme_sun2 *sun2;

  /* recover our sun2: */
  sun2 = (struct tme_sun2 *) _sun2;

  /* log this bus error: */
  tme_log(TME_SUN2_LOG_HANDLE(sun2), 1000, TME_OK,
	  (TME_SUN2_LOG_HANDLE(sun2),
	   _("page protection bus error")));

  /* set the bus error register: */
  sun2->tme_sun2_buserr = TME_SUN2_BUSERR_VALID | TME_SUN2_BUSERR_PROTERR;

  /* return the fault: */
  return (EFAULT);
}

/* our m68k TLB filler: */
int
_tme_sun2_m68k_tlb_fill(struct tme_m68k_bus_connection *conn_m68k, struct tme_m68k_tlb *tlb_m68k,
			unsigned int function_code, tme_uint32_t address, unsigned int cycles)
{
  struct tme_sun2 *sun2;
  struct tme_bus_tlb *tlb;
  unsigned short tlb_flags;

  /* recover our sun2: */
  sun2 = (struct tme_sun2 *) conn_m68k->tme_m68k_bus_connection.tme_bus_connection.tme_connection_element->tme_element_private;

  /* get the generic bus TLB: */
  tlb = &tlb_m68k->tme_m68k_tlb_bus_tlb;

  /* if this is function code three, we handle this ourselves: */
  if (function_code == TME_M68K_FC_3) {

    /* initialize the TLB entry: */
    tme_bus_tlb_initialize(tlb);

    /* we cover the entire address space: */
    tlb->tme_bus_tlb_addr_first = 0;
    tlb->tme_bus_tlb_addr_last = ((tme_bus_addr_t) 0) - 1;

    /* we allow reading and writing: */
    tlb->tme_bus_tlb_cycles_ok = TME_BUS_CYCLE_READ | TME_BUS_CYCLE_WRITE;

    /* our bus cycle handler: */
    tlb->tme_bus_tlb_cycle_private = sun2;
    tlb->tme_bus_tlb_cycle = _tme_sun2_control_cycle_handler;

    /* this is good for function code three only: */
    tlb_m68k->tme_m68k_tlb_function_codes_mask = TME_BIT(TME_M68K_FC_3);
  }

  /* if this is a supervisor function code and we're in the PROM
     address range, we handle this ourselves: */
  else if ((function_code == TME_M68K_FC_SD
	    || function_code == TME_M68K_FC_SP)
	   && address >= TME_SUN2_PROM_BASE
	   && address < (TME_SUN2_PROM_BASE + TME_SUN2_PROM_SIZE)) {

    /* fill this TLB entry directly from the obmem bus: */
    (*sun2->tme_sun2_obmem->tme_bus_tlb_fill)
      (sun2->tme_sun2_obmem,
       tlb,
       address,
       cycles);

    /* this is good for supervisor data and supervisor program function codes: */
    tlb_m68k->tme_m68k_tlb_function_codes_mask = (TME_BIT(TME_M68K_FC_SD)
						  | TME_BIT(TME_M68K_FC_SP));
  }

  /* this is a normal function code: */
  else {

    /* fill this TLB entry from the MMU: */
    tlb_flags = ((function_code == TME_M68K_FC_UD
		  || function_code == TME_M68K_FC_UP)
		 ? tme_sun_mmu_tlb_fill(sun2->tme_sun2_mmu,
					tlb,
					sun2->tme_sun2_context_user,
					address,
					((cycles & TME_BUS_CYCLE_WRITE)
					 ? TME_SUN_MMU_PTE_PROT_USER(TME_SUN_MMU_PTE_PROT_RW)
					 : TME_SUN_MMU_PTE_PROT_USER(TME_SUN_MMU_PTE_PROT_RO)))
		 : tme_sun_mmu_tlb_fill(sun2->tme_sun2_mmu,
					tlb,
					sun2->tme_sun2_context_system,
					address,
					((cycles & TME_BUS_CYCLE_WRITE)
					 ? TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_RW)
					 : TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_RO))));
    
    /* TLB entries are good only for the program and data function
       codes for the user or supervisor, but never both, because
       the two types of accesses go through different contexts: */
    tlb_m68k->tme_m68k_tlb_function_codes_mask =
      ((function_code == TME_M68K_FC_UD
	|| function_code == TME_M68K_FC_UP)
       ? (TME_BIT(TME_M68K_FC_UD)
	  | TME_BIT(TME_M68K_FC_UP))
       : (TME_BIT(TME_M68K_FC_SD)
	  | TME_BIT(TME_M68K_FC_SP)));
  }

  return (TME_OK);
}

/* our bus TLB filler: */
int
_tme_sun2_bus_tlb_fill(struct tme_bus_connection *conn_bus, struct tme_bus_tlb *tlb,
		       tme_uint32_t address, unsigned int cycles)
{
  struct tme_sun2 *sun2;
  struct tme_sun2_bus_connection *conn_sun2;
  tme_uint32_t base, size;
  struct tme_bus_tlb tlb_bus;

  /* recover our sun2: */
  sun2 = (struct tme_sun2 *) conn_bus->tme_bus_connection.tme_connection_element->tme_element_private;

  /* recover the sun2 internal mainbus connection: */
  conn_sun2 = (struct tme_sun2_bus_connection *) conn_bus;

  /* turn the bus address into a DVMA address: */
  switch (conn_sun2->tme_sun2_bus_connection_which) {

    /* obio devices can actually see the whole address space: */
  case TME_SUN2_BUS_OBIO:
    base = 0x000000;
    size = 0x1000000;
    break;

  case TME_SUN2_BUS_MBMEM:
    base = 0xf00000;
    size = TME_SUN2_DVMA_SIZE_MBMEM;
    break;

  case TME_SUN2_BUS_VME:
    base = 0xf00000;
    size = TME_SUN2_DVMA_SIZE_VME;
    break;

  default: abort();
  }

  assert (!(address & base)
	  && (address < size));

  /* fill this TLB entry from the MMU: */
  tme_sun_mmu_tlb_fill(sun2->tme_sun2_mmu,
		       tlb,
		       sun2->tme_sun2_context_system,
		       address | base,
		       ((cycles & TME_BUS_CYCLE_WRITE)
			? TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_RW)
			: TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_RO)));

  /* create the mapping TLB entry.  we do this even if base == 0,
     because the TLB entry as currently filled may cover more address
     space than DVMA space on this machine is supposed to cover: */
  tlb_bus.tme_bus_tlb_addr_first = 0;
  tlb_bus.tme_bus_tlb_addr_last = size - 1;
  tlb_bus.tme_bus_tlb_cycles_ok
    = (TME_BUS_CYCLE_READ
       | TME_BUS_CYCLE_WRITE);
  
  /* map the filled TLB entry: */
  tme_bus_tlb_map(tlb, address | base, &tlb_bus, address);

  return (TME_OK);
}

/* our post-MMU TLB filler: */
static int
_tme_sun2_tlb_fill_mmu(void *_sun2, struct tme_bus_tlb *tlb, 
		       struct tme_sun_mmu_pte *pte,
		       tme_uint32_t *_address,
		       unsigned int cycles)
{
  struct tme_sun2 *sun2;
  tme_uint32_t address;
  unsigned int bus_type;
  struct tme_bus_connection *conn_bus;
  tme_bus_fault_handler bus_fault_handler;
  int rc;

  /* recover our sun2: */
  sun2 = (struct tme_sun2 *) _sun2;

  /* get the physical page frame and bus type: */
  address = ((pte->tme_sun_mmu_pte_raw & TME_SUN2_PTE_PGFRAME) << TME_SUN2_PAGE_SIZE_LOG2);
  bus_type = (pte->tme_sun_mmu_pte_raw & TME_SUN2_PTE_PGTYPE) / (TME_SUN2_PTE_PGTYPE / TME_SUN2_PTE_PGTYPE_MASK);

  /* any mapping of the *first* page of obio space means the PROM.
     the virtual page frame is actually used to form the physical
     address: */
  if (address == 0
      && bus_type == TME_SUN2_PGTYPE_OBIO) {
    address = TME_SUN2_PROM_BASE | (*_address & ((TME_SUN2_PROM_SIZE - 1) & ~(TME_SUN2_PAGE_SIZE - 1)));
    bus_type = TME_SUN2_PGTYPE_OBMEM;
  }

  /* add in the page offset to finish the address: */
  address |= *_address & (TME_SUN2_PAGE_SIZE - 1);
  *_address = address;

  /* if this is obio: */
  if (bus_type == TME_SUN2_PGTYPE_OBIO) {
    conn_bus = sun2->tme_sun2_obio;
    bus_fault_handler = _tme_sun2_obio_fault_handler;
  }
  
  /* if this is obmem: */
  else if (bus_type == TME_SUN2_PGTYPE_OBMEM) {
    conn_bus = sun2->tme_sun2_obmem;
    bus_fault_handler = _tme_sun2_obmem_fault_handler;
  }

  /* if this is the VME bus: */
  else if (sun2->tme_sun2_has_vme) {
    
    if (bus_type == TME_SUN2_PGTYPE_VME8) {
      address |= 0x800000;
    }
    else {
      assert(bus_type == TME_SUN2_PGTYPE_VME0);
    }

    bus_fault_handler = _tme_sun2_vmebus_fault_handler;

    /* TBD: */
    abort();
  }

  /* if this is mbmem: */
  else if (bus_type == TME_SUN2_PGTYPE_MBMEM) {
    conn_bus = sun2->tme_sun2_mbmem;
    bus_fault_handler = _tme_sun2_multibus_fault_handler;
  }

  /* otherwise, this is mbio: */
  else {
    assert(bus_type == TME_SUN2_PGTYPE_MBIO);
    conn_bus = sun2->tme_sun2_mbio;
    bus_fault_handler = _tme_sun2_multibus_fault_handler;
  }

  /* call the bus TLB filler: */
  rc = ((*conn_bus->tme_bus_tlb_fill)
	(conn_bus, tlb, address, cycles));

  /* if the bus TLB filler succeeded, add our bus fault handler: */
  if (rc == TME_OK) {
    TME_BUS_TLB_FAULT_HANDLER(tlb, bus_fault_handler, sun2);
  }

  return (rc);
}

/* this gets a PTE from the MMU: */
int
_tme_sun2_mmu_pte_get(struct tme_sun2 *sun2, tme_uint32_t address, tme_uint32_t *_pte_sun2)
{
  struct tme_sun_mmu_pte pte;
  tme_uint32_t pte_sun2;
  unsigned int pte_flags;
  int rc;

  /* get the PTE from the MMU: */
  rc = tme_sun_mmu_pte_get(sun2->tme_sun2_mmu, 
			   sun2->tme_sun2_context_user,
			   address,
			   &pte);
  assert(rc == TME_OK);
    
  /* form the Sun-2 PTE: */
  pte_sun2 = pte.tme_sun_mmu_pte_raw;
  pte_flags = pte.tme_sun_mmu_pte_flags;
  if (pte_flags & TME_SUN_MMU_PTE_REF) {
    pte_sun2 |= TME_SUN2_PTE_REF;
  }
  if (pte_flags & TME_SUN_MMU_PTE_MOD) {
    pte_sun2 |= TME_SUN2_PTE_MOD;
  }

  /* done: */
  *_pte_sun2 = pte_sun2;
  tme_log(TME_SUN2_LOG_HANDLE(sun2), 1000, TME_OK,
	  (TME_SUN2_LOG_HANDLE(sun2),
	   _("pte_get: PGMAP[%d:0x%08x] -> 0x%08x"), 
	   sun2->tme_sun2_context_user,
	   address,
	   pte_sun2));
  return (TME_OK);
}

/* this sets a PTE into the MMU: */
int
_tme_sun2_mmu_pte_set(struct tme_sun2 *sun2, tme_uint32_t address, tme_uint32_t pte_sun2)
{
  struct tme_sun_mmu_pte pte;
  unsigned int pte_flags;
#ifndef TME_NO_LOG
  const char *bus_name;
  tme_bus_addr_t physical_address;
      
  /* this silences gcc -Wuninitialized: */
  bus_name = NULL;

  /* log this setting: */
  physical_address = ((pte_sun2 & TME_SUN2_PTE_PGFRAME) << TME_SUN2_PAGE_SIZE_LOG2);
  switch ((pte_sun2 & TME_SUN2_PTE_PGTYPE) / (TME_SUN2_PTE_PGTYPE / TME_SUN2_PTE_PGTYPE_MASK)) {
  case TME_SUN2_PGTYPE_OBMEM: bus_name = "obmem"; break;
  case TME_SUN2_PGTYPE_OBIO: bus_name = "obio"; break;
  case TME_SUN2_PGTYPE_MBMEM:
    if (sun2->tme_sun2_has_vme) {
      bus_name = "VME";
    }
    else {
      bus_name = "mbmem";
    }
    break;
  case TME_SUN2_PGTYPE_MBIO:
    if (sun2->tme_sun2_has_vme) {
      bus_name = "VME";
      physical_address |= 0x800000;
    }
    else {
      bus_name = "mbio";
    }
    break;
  }
  tme_log(TME_SUN2_LOG_HANDLE(sun2), 1000, TME_OK,
	  (TME_SUN2_LOG_HANDLE(sun2),
	   _("pte_set: PGMAP[%d:0x%08x] <- 0x%08x (%s 0x%08x)"), 
	   sun2->tme_sun2_context_user,
	   address,
	   pte_sun2,
	   bus_name,
	   physical_address));
#endif /* !TME_NO_LOG */

  pte.tme_sun_mmu_pte_raw = pte_sun2;
      
  pte_flags = 0;
  if (pte_sun2 & TME_SUN2_PTE_MOD) {
    pte_flags |= TME_SUN_MMU_PTE_MOD;
  }
  if (pte_sun2 & TME_SUN2_PTE_REF) {
    pte_flags |= TME_SUN_MMU_PTE_REF;
  }
  switch (pte_sun2 & TME_SUN2_PTE_PROT) {

    /* with this protection, the system can read and write,
       and the user gets a protection error: */
  case 0x70000000:
  case 0x74000000:
  case 0x60000000:
    pte_flags |= 
      (TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_RW)
       | TME_SUN_MMU_PTE_PROT_USER(TME_SUN_MMU_PTE_PROT_ERROR));
    break;

    /* with this protection, the system gets a protection error,
       and the user gets a protection error: */
  case 0x30000000:
  case 0x20000000:
  case 0x10000000:
  case 0x00000000:
  case 0x04000000:
    pte_flags |= 
      (TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_ERROR)
       | TME_SUN_MMU_PTE_PROT_USER(TME_SUN_MMU_PTE_PROT_ERROR));
    break;

    /* with this protection, the system can read and write,
       and the user can read and write: */
  case 0x7C000000:
  case 0x6C000000:
    pte_flags |= 
      (TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_RW)
       | TME_SUN_MMU_PTE_PROT_USER(TME_SUN_MMU_PTE_PROT_RW));
    break;

    /* with this protection, the system can read and write,
       and the user can read: */
  case 0x78000000:
    pte_flags |= 
      (TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_RW)
       | TME_SUN_MMU_PTE_PROT_USER(TME_SUN_MMU_PTE_PROT_RO));
    break;

    /* with this protection, the system can read,
       and the user can read: */
  case 0x58000000:
    pte_flags |= 
      (TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_RO)
       | TME_SUN_MMU_PTE_PROT_USER(TME_SUN_MMU_PTE_PROT_RO));
    break;

    /* with this protection, the system can read,
       and the user can read and write: */
  case 0x5C000000:
  case 0x4C000000:
    pte_flags |= 
      (TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_RO)
       | TME_SUN_MMU_PTE_PROT_USER(TME_SUN_MMU_PTE_PROT_RW));
    break;

    /* with this protection, the system can read,
       and the user gets a protection error: */
  case 0x50000000:
  case 0x40000000:
    pte_flags |= 
      (TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_RO)
       | TME_SUN_MMU_PTE_PROT_USER(TME_SUN_MMU_PTE_PROT_ERROR));
    break;

    /* with this protection, the system gets a protection error,
       and the user can read and write: */
  case 0x3c000000:
  case 0x0c000000:
    pte_flags |= 
      (TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_ERROR)
       | TME_SUN_MMU_PTE_PROT_USER(TME_SUN_MMU_PTE_PROT_RW));
    break;

    /* with this protection, the system gets a protection error,
       and the user can read: */
  case 0x08000000:
    pte_flags |= 
      (TME_SUN_MMU_PTE_PROT_SYSTEM(TME_SUN_MMU_PTE_PROT_ERROR)
       | TME_SUN_MMU_PTE_PROT_USER(TME_SUN_MMU_PTE_PROT_RO));
    break;

  default: abort();
  }
  if (pte_sun2 & TME_SUN2_PTE_VALID) {
    pte_flags |= TME_SUN_MMU_PTE_VALID;
  }
  pte.tme_sun_mmu_pte_flags = pte_flags;
  
  return (tme_sun_mmu_pte_set(sun2->tme_sun2_mmu, 
			      sun2->tme_sun2_context_user,
			      address,
			      &pte));
}

/* this is called when the system context register is set: */
void
_tme_sun2_mmu_context_system_set(struct tme_sun2 *sun2)
{
  /* system context register changes are assumed to be rare.  if they
     were frequent, we'd have to allocate 64 TLB sets for each TLB
     user - one for each possible combination of user context and
     system context.  instead, when the system context register
     changes, we simply invalidate all TLB entries everywhere: */
  tme_log(TME_SUN2_LOG_HANDLE(sun2), 1000, TME_OK,
	  (TME_SUN2_LOG_HANDLE(sun2),
	   _("system context now #%d"),
	   sun2->tme_sun2_context_system));
  tme_sun_mmu_tlbs_invalidate(sun2->tme_sun2_mmu);
}

/* this is called when the user context register is set: */
void
_tme_sun2_mmu_context_user_set(struct tme_sun2 *sun2)
{
  tme_log(TME_SUN2_LOG_HANDLE(sun2), 1000, TME_OK,
	  (TME_SUN2_LOG_HANDLE(sun2),
	   _("user context now #%d"),
	   sun2->tme_sun2_context_user));
  tme_sun_mmu_tlbs_context_set(sun2->tme_sun2_mmu, sun2->tme_sun2_context_user);
}

/* this allocates a new TLB set: */
int
_tme_sun2_mmu_tlb_set_allocate(struct tme_bus_connection *conn_bus_asker,
			       unsigned int count, unsigned int sizeof_one, 
			       struct tme_bus_tlb * tme_shared * _tlbs,
			       tme_rwlock_t *_tlbs_rwlock)
{
  struct tme_sun2 *sun2;
  int rc;

  /* recover our sun2: */
  sun2 = (struct tme_sun2 *) conn_bus_asker->tme_bus_connection.tme_connection_element->tme_element_private;

  /* get the MMU to allocate the TLB set: */
  rc = tme_sun_mmu_tlb_set_allocate(sun2->tme_sun2_mmu, count, sizeof_one, _tlbs, _tlbs_rwlock);

  /* if this is the TLB set for our CPU, remember where the context
     zero TLBs are, and try to reset the MMU now: */
  /* FIXME - this assumes that the *first* TLB set allocated by
     the CPU is for its data: */
  if (rc == TME_OK
      && conn_bus_asker->tme_bus_connection.tme_connection_type == TME_CONNECTION_BUS_M68K
      && sun2->tme_sun2_reset_tlbs == NULL) {
    assert(sizeof_one == sizeof(struct tme_m68k_tlb));
    sun2->tme_sun2_reset_tlbs = (struct tme_m68k_tlb *) tme_memory_atomic_pointer_read(struct tme_bus_tlb *, *_tlbs, _tlbs_rwlock);
    sun2->tme_sun2_reset_tlb_count = count;
    _tme_sun2_mmu_reset(sun2);
  }

  return (rc);
}

/* the first four 16-bit read cycles that the m68010 does after it comes
   out of reset are to fetch the reset vector (one 32-bit word for the
   initial SSP, one 32-bit word for the initial PC).

   apparently the Sun-2 reset circuitry has some special logic that
   is able to direct these read cycles to the PROM, where the reset
   vector is located.  we simulate this logic by forcing the CPU's
   context zero TLB entries to all point to ROM for addresses 0-7,
   but only for the first four cycles, after which we invalidate 
   all of the CPU TLB entries. */

/* this is a special bus cycle handling function used at reset time.
   it is used only for the first four m68010 read cycles: */
static int
_tme_sun2_reset_cycle(void *_sun2, struct tme_bus_cycle *cycle)
{
  struct tme_sun2 *sun2;
  int rc;

  /* recover our sun2: */
  sun2 = (struct tme_sun2 *) _sun2;

  /* this must be a 16-bit read: */
  assert(cycle->tme_bus_cycle_size == sizeof(tme_uint16_t));
  tme_log(TME_SUN2_LOG_HANDLE(sun2), 1000, TME_OK,
	  (TME_SUN2_LOG_HANDLE(sun2),
	   _("reset cycle #%d"),
	   sun2->tme_sun2_reset_cycles));

  /* run the real cycle: */
  rc = ((*sun2->tme_sun2_reset_cycle)
	(sun2->tme_sun2_reset_cycle_private,
	 cycle));

  /* after the fourth read cycle, invalidate all of the TLBs
     that the CPU holds, ending these abnormal reset reads: */
  if (rc == TME_OK) {
    sun2->tme_sun2_reset_cycles++;
    if (sun2->tme_sun2_reset_cycles == 4) {
      tme_sun_mmu_tlbs_invalidate(sun2->tme_sun2_mmu);
    }
  }

  return (rc);
}

/* this initializes the context zero part of the CPU's TLB set to
   support four slow 16-bit reads from ROM at addresses 0, 2, 4, 6,
   respectively.  this emulates how the Sun-2 behaves as it comes out
   of reset: */
int
_tme_sun2_mmu_reset(struct tme_sun2 *sun2)
{
  struct tme_m68k_tlb *tlb_m68k;
  struct tme_bus_tlb *tlb, tlb_virtual;
  unsigned long tlb_i;

  /* we can only do this initialization once we have both
     the obmem bus and the CPU's TLBs: */
  tlb_m68k = sun2->tme_sun2_reset_tlbs;
  if (tlb_m68k == NULL
      || sun2->tme_sun2_obmem == NULL) {
    return (TME_OK);
  }
  sun2->tme_sun2_reset_tlbs = NULL;
  /* XXX FIXME - this is not thread-safe: */
  tlb = &tlb_m68k->tme_m68k_tlb_bus_tlb;
  tme_bus_tlb_busy(tlb);
  tme_bus_tlb_unbusy_fill(tlb);
  
  /* fill the TLB entry: */
  (*sun2->tme_sun2_obmem->tme_bus_tlb_fill)
    (sun2->tme_sun2_obmem,
     tlb,
     TME_SUN2_PROM_BASE,
     TME_BUS_CYCLE_READ);

  /* map the TLB entry: */
  tlb_virtual.tme_bus_tlb_addr_first = 0;
  tlb_virtual.tme_bus_tlb_addr_last = 7;
  tlb_virtual.tme_bus_tlb_cycles_ok = TME_BUS_CYCLE_READ;
  tme_bus_tlb_map(tlb, TME_SUN2_PROM_BASE, &tlb_virtual, 0);
  
  /* this TLB entry must allow slow reads from exactly the reset
     range: */
  if (!(tlb->tme_bus_tlb_cycles_ok & TME_BUS_CYCLE_READ)
      || tlb->tme_bus_tlb_addr_first != 0
      || tlb->tme_bus_tlb_addr_last != 7) {
    abort();
  }
  
  /* take over this TLB entry: */
  sun2->tme_sun2_reset_cycles = 0;
  sun2->tme_sun2_reset_cycle_private = tlb->tme_bus_tlb_cycle_private;
  sun2->tme_sun2_reset_cycle = tlb->tme_bus_tlb_cycle;
  tlb->tme_bus_tlb_emulator_off_read = TME_EMULATOR_OFF_UNDEF;
  tlb->tme_bus_tlb_cycle_private = sun2;
  tlb->tme_bus_tlb_cycle = _tme_sun2_reset_cycle;
  
  /* this TLB entry is usable by the supervisor: */
  tlb_m68k->tme_m68k_tlb_function_codes_mask = (TME_BIT(TME_M68K_FC_SD)
						| TME_BIT(TME_M68K_FC_SP));
  
  /* now copy this TLB entry into all of the others: */
  for (tlb_i = sun2->tme_sun2_reset_tlb_count - 1; tlb_i-- > 0; ) {
    tlb_m68k[1] = tlb_m68k[0];
    tlb_m68k++;
  }

  return (TME_OK);
}

/* this creates a Sun-2 MMU: */
void
_tme_sun2_mmu_new(struct tme_sun2 *sun2)
{
  struct tme_sun_mmu_info mmu_info;

  memset(&mmu_info, 0, sizeof(mmu_info));
  mmu_info.tme_sun_mmu_info_element = sun2->tme_sun2_element;
  mmu_info.tme_sun_mmu_info_address_bits = 24;
  mmu_info.tme_sun_mmu_info_pgoffset_bits = TME_SUN2_PAGE_SIZE_LOG2;
  mmu_info.tme_sun_mmu_info_pteindex_bits = 4;
  mmu_info.tme_sun_mmu_info_contexts = 8;
  mmu_info.tme_sun_mmu_info_pmegs = 256;
  mmu_info.tme_sun_mmu_info_tlb_fill_private = sun2;
  mmu_info.tme_sun_mmu_info_tlb_fill = _tme_sun2_tlb_fill_mmu;
  mmu_info.tme_sun_mmu_info_proterr_private = sun2;
  mmu_info.tme_sun_mmu_info_proterr = _tme_sun2_mmu_proterr;
  mmu_info.tme_sun_mmu_info_invalid_private = sun2;
  mmu_info.tme_sun_mmu_info_invalid = _tme_sun2_mmu_invalid;
  sun2->tme_sun2_mmu = tme_sun_mmu_new(&mmu_info);
}
