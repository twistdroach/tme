/* $Id: posix-memory.c,v 1.4 2003/05/16 21:48:09 fredette Exp $ */

/* host/posix/memory.c - implementation of memory on a POSIX system: */

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
_TME_RCSID("$Id: posix-memory.c,v 1.4 2003/05/16 21:48:09 fredette Exp $");

/* includes: */
#include <tme/generic/bus-device.h>
#include <fcntl.h>
#include <stdio.h>
#include <strings.h>
#include <sys/stat.h>
#ifdef HAVE_MMAP
#include <sys/types.h>
#include <sys/mman.h>
#endif /* HAVE_MMAP */

/* macros: */
#define TME_POSIX_MEMORY_RAM		(0)
#define TME_POSIX_MEMORY_ROM		(1)
#define TME_POSIX_MEMORY_PERSISTENT	(2)

/* structures: */
struct tme_posix_memory {

  /* our simple bus device header: */
  struct tme_bus_device tme_posix_memory_device;

  /* our memory type: */
  unsigned int tme_posix_memory_type;

  /* the file descriptor to any backing file: */
  int tme_posix_memory_fd;

  /* this is nonzero if the backing file is mmapped: */
  int tme_posix_memory_mapped;

  /* our rwlock: */
  tme_mutex_t tme_posix_memory_rwlock;

  /* our memory contents: */
  tme_uint8_t *tme_posix_memory_contents;
};

/* the memory bus cycle handler: */
static int
_tme_posix_memory_bus_cycle(void *_memory, struct tme_bus_cycle *cycle)
{
  struct tme_posix_memory *memory;

  /* recover our data structure: */
  memory = (struct tme_posix_memory *) _memory;

  /* run the cycle: */
  tme_bus_cycle_xfer_memory(cycle, 
			    ((cycle->tme_bus_cycle_type == TME_BUS_CYCLE_WRITE
			      && memory->tme_posix_memory_type == TME_POSIX_MEMORY_ROM)
			     ? NULL
			     : memory->tme_posix_memory_contents),
			    memory->tme_posix_memory_device.tme_bus_device_address_last);

  /* no faults: */
  return (TME_OK);
}

/* the memory TLB filler: */
static int
_tme_posix_memory_tlb_fill(void *_memory, struct tme_bus_tlb *tlb, 
			   tme_bus_addr_t address, unsigned int cycles)
{
  struct tme_posix_memory *memory;
  tme_bus_addr_t memory_address_last;

  /* recover our data structure: */
  memory = (struct tme_posix_memory *) _memory;

  /* the address must be within range: */
  memory_address_last = memory->tme_posix_memory_device.tme_bus_device_address_last;
  assert(address <= memory_address_last);

  /* initialize the TLB entry: */
  tme_bus_tlb_initialize(tlb);

  /* this TLB entry can cover the whole device: */
  TME_ATOMIC_WRITE(tme_bus_addr_t, tlb->tme_bus_tlb_addr_first, 0);
  TME_ATOMIC_WRITE(tme_bus_addr_t, tlb->tme_bus_tlb_addr_last, memory_address_last);

  /* all memory devices allow fast reading.  all memory devices except
     ROMs allow fast writing: */
  tlb->tme_bus_tlb_emulator_off_read = memory->tme_posix_memory_contents;
  if (memory->tme_posix_memory_type != TME_POSIX_MEMORY_ROM) {
    tlb->tme_bus_tlb_emulator_off_write = memory->tme_posix_memory_contents;
  }
  tlb->tme_bus_tlb_rwlock = &memory->tme_posix_memory_rwlock;

  /* all memory devices allow reading and writing: */
  tlb->tme_bus_tlb_cycles_ok = TME_BUS_CYCLE_READ | TME_BUS_CYCLE_WRITE;

  /* our bus cycle handler: */
  tlb->tme_bus_tlb_cycle_private = memory;
  tlb->tme_bus_tlb_cycle = _tme_posix_memory_bus_cycle;

  return (TME_OK);
}

/* the new memory function: */
TME_ELEMENT_SUB_NEW_DECL(tme_host_posix,memory) {
  unsigned int memory_type;
  unsigned long memory_size;
  const char *filename;
  int fd;
  struct stat statbuf;
  struct tme_posix_memory *memory;
  ssize_t bytes_read;
  int arg_i;
  int usage;

  /* assume we have no backing file: */
  filename = NULL;
  memory_type = -1;
  memory_size = 0;
  arg_i = 1;
  usage = FALSE;

  /* we are regular RAM if our arguments are:

     ram SIZE

  */
  if (TME_ARG_IS(args[arg_i + 0], "ram")
      && (memory_size = tme_bus_addr_parse(args[arg_i + 1], 0)) > 0) {
    memory_type = TME_POSIX_MEMORY_RAM;
    arg_i += 2;
  }

  /* we are ROM if our arguments are:

     rom FILE
     
  */
  else if (TME_ARG_IS(args[arg_i + 0], "rom")
	   && (filename = args[arg_i + 1]) != NULL) {
    memory_type = TME_POSIX_MEMORY_ROM;
    arg_i += 2;
  }

  /* we are nonvolatile storage if our arguments are:

     nonvolatile FILE

  */
  else if (TME_ARG_IS(args[arg_i + 0], "persistent")
	   && (filename = args[arg_i + 1]) != NULL) {
    memory_type = TME_POSIX_MEMORY_PERSISTENT;
    arg_i += 2;
  }
	   
  else {
    usage = TRUE;
  }

  if (args[arg_i + 0] != NULL) {
    tme_output_append_error(_output,
			    "%s %s", 
			    args[arg_i],
			    _("unexpected"));
    usage = TRUE;
  }

  if (usage) {
    tme_output_append_error(_output,
			    "%s %s { rom %s | ram %s | persistent %s }",
			    _("usage:"),
			    args[0],
			    _("ROM-FILE"),
			    _("SIZE"),
			    _("PERSISTENT-FILE"));
    return (-1);
  }

  /* start the memory structure: */
  memory = tme_new0(struct tme_posix_memory, 1);
  memory->tme_posix_memory_type = memory_type;

  /* if we have a backing file: */
  fd = -1;
  if (filename != NULL) {

    /* open the file for reading: */
    fd = open(filename, (memory_type == TME_POSIX_MEMORY_ROM
			 ? O_RDONLY
			 : O_RDWR));
    if (fd < 0) {
      tme_output_append_error(_output,
			      "%s",
			      filename);
      tme_free(memory);
      return (errno);
    }

    /* stat the file: */
    if (fstat(fd, &statbuf) < 0) {
      tme_output_append_error(_output,
			      "%s",
			      filename);
      close(fd);
      tme_free(memory);
      return (errno);
    }
    memory_size = statbuf.st_size;
    if (memory_size == 0) {
      tme_output_append_error(_output,
			      "%s",
			      filename);
      close(fd);
      tme_free(memory);
      return (EINVAL);
    }

#ifdef HAVE_MMAP    
    /* try to mmap the file: */
    memory->tme_posix_memory_contents = 
      mmap(NULL, 
	   statbuf.st_size, 
	   PROT_READ
	   | (memory_type != TME_POSIX_MEMORY_ROM
	      ? PROT_WRITE
	      : 0),
	   MAP_SHARED,
	   fd,
	   0);
    if (memory->tme_posix_memory_contents != MAP_FAILED) {
      memory->tme_posix_memory_mapped = TRUE;
    }
#endif /* HAVE_MMAP */
  }

  /* if we have to, allocate memory space: */
  if (!memory->tme_posix_memory_mapped) {
    memory->tme_posix_memory_contents = tme_new0(tme_uint8_t, memory_size);

    /* if we have to, read in the backing file: */
    if (fd >= 0) {
      bytes_read = read(fd, memory->tme_posix_memory_contents, memory_size);
      if (bytes_read < 0
	  || memory_size != (unsigned long) bytes_read) {
	/* XXX diagnostic: */
	close(fd);
	tme_free(memory->tme_posix_memory_contents);
	tme_free(memory);
	return (-1);
      }

      /* if this is a ROM, we can close the file now: */
      if (memory_type == TME_POSIX_MEMORY_ROM) {
	close(fd);
	fd = -1;
      }
    }
  }

  /* remember any backing fd: */
  memory->tme_posix_memory_fd = fd;

  /* initialize our rwlock: */
  tme_rwlock_init(&memory->tme_posix_memory_rwlock);

  /* initialize our simple bus device descriptor: */
  memory->tme_posix_memory_device.tme_bus_device_tlb_fill = _tme_posix_memory_tlb_fill;
  memory->tme_posix_memory_device.tme_bus_device_address_last = (memory_size - 1);

  /* fill the element: */
  element->tme_element_private = memory;
  element->tme_element_connections_new = tme_bus_device_connections_new;

  return (0);
}
