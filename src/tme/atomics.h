/* $Id: atomics.h,v 1.3 2005/02/17 12:37:26 fredette Exp $ */

/* tme/atomics.h - header file for atomically-updated values: */

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

#ifndef _TME_ATOMICS_H
#define _TME_ATOMICS_H

#include <tme/common.h>
_TME_RCSID("$Id: atomics.h,v 1.3 2005/02/17 12:37:26 fredette Exp $");

/* includes: */
#include <tme/threads.h>

/* if this system always guarantees that aligned writes are atomic, we
   simply rely on the compiler to always align values for us, and just
   do simple assignments.  we normally audit, however, to catch where
   we use atomics without using the accessors: */
#ifdef TME_ALIGNED_ACCESS_ATOMIC
#ifdef TME_NO_AUDIT_ATOMICS

/* no-audit atomic variables: */
#define TME_ATOMIC(typ, var) typ _tme_volatile var
#define TME_ATOMIC_POINTER(ptr) (ptr)
#define TME_ATOMIC_POINTER_TYPE(typ) typ _tme_volatile *
#define TME_ATOMIC_WRITE(typ, var, rvalue)	\
  (var = (rvalue))
#define TME_ATOMIC_READ(typ, var)		\
  (var)

#else  /* !TME_NO_AUDIT_ATOMICS */

/* auditable atomic variables: */
#define TME_ATOMIC(typ, var) struct { unsigned long __atomic_junk; typ _tme_volatile __atomic_value; } var
#define TME_ATOMIC_POINTER(ptr) ((unsigned long *) (ptr))
#define TME_ATOMIC_POINTER_TYPE(typ) unsigned long *
#define TME_ATOMIC_WRITE(typ, var, rvalue)	\
  (*((typ _tme_volatile *) (((unsigned long *) &(var)) + 1)) = (rvalue))
#define TME_ATOMIC_READ(typ, var)		\
  (*((typ _tme_volatile *) (((unsigned long *) &(var)) + 1)))

#endif /* !TME_NO_AUDIT_ATOMICS */

#else  /* !TME_ALIGNED_ACCESS_ATOMIC */

/* real atomic variable construction: */
#define TME_ATOMIC(typ, var) struct { tme_rwlock_t __atomic_rwlock; typ __atomic_value; } var
#define TME_ATOMIC_POINTER(ptr) ((tme_rwlock_t *) (ptr))
#define TME_ATOMIC_POINTER_TYPE(typ) tme_rwlock_t *
#define TME_ATOMIC_WRITE(typ, var, rvalue)	\
  ((typ) tme_atomic_write(((tme_rwlock_t *) &(var)), (unsigned long) (rvalue), sizeof(typ)))
#define TME_ATOMIC_READ(typ, var, rvalue)	\
  ((typ) tme_atomic_read(((tme_rwlock_t *) &(var)), sizeof(typ)))
#endif /* !TME_ALIGNED_ACCESS_ATOMIC */

#endif /* !_TME_ATOMICS_H */
