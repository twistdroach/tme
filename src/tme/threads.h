/* $Id: threads.h,v 1.6 2003/06/27 21:09:10 fredette Exp $ */

/* tme/threads.h - header file for threads: */

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

#ifndef _TME_THREADS_H
#define _TME_THREADS_H

#include <tme/common.h>
_TME_RCSID("$Id: threads.h,v 1.6 2003/06/27 21:09:10 fredette Exp $");

/* includes: */
#include <errno.h>

/* note that our locking model never allows recursive locking. */

/* setjmp/longjmp threading: */
#ifdef TME_THREADS_SJLJ

/* setjmp/longjmp threads are cooperative: */
#define TME_THREADS_COOPERATIVE

/* our errno convention: */
#define TME_EDEADLK		EBUSY
#define TME_EBUSY		EBUSY
#define TME_THREADS_ERRNO(rc)	(rc)

/* initializing and starting: */
void tme_sjlj_threads_init _TME_P((void));
#define tme_threads_init tme_sjlj_threads_init
#ifdef _TME_HAVE_GTK
void tme_sjlj_threads_gtk_init _TME_P((void));
#define tme_threads_gtk_init tme_sjlj_threads_gtk_init
#endif /* _TME_HAVE_GTK */
void tme_sjlj_threads_run _TME_P((void));
#define tme_threads_run tme_sjlj_threads_run

/* if we want speed over lock debugging, we can compile very simple
   rwlock operations: */
#ifdef TME_NO_DEBUG_LOCKS
typedef int tme_rwlock_t;
#define tme_rwlock_init(l) (*(l) = FALSE, TME_OK)
#define tme_rwlock_rdlock(l) (*(l) = TRUE, TME_OK)
#define tme_rwlock_tryrdlock(l) (*(l) ? TME_EBUSY : tme_rwlock_rdlock(l))
#define tme_rwlock_unlock(l) (*(l) = FALSE, TME_OK)
#else  /* !TME_NO_DEBUG_LOCKS */   

/* debugging rwlocks: */
typedef struct tme_sjlj_rwlock {

  /* nonzero iff the lock is locked: */
  int _tme_sjlj_rwlock_locked;

  /* the file and line number of the last locker or unlocker: */
  _tme_const char *_tme_sjlj_rwlock_file;
  unsigned long _tme_sjlj_rwlock_line;
} tme_rwlock_t;

/* lock operations: */
int tme_sjlj_rwlock_init _TME_P((struct tme_sjlj_rwlock *));
int tme_sjlj_rwlock_lock _TME_P((struct tme_sjlj_rwlock *, _tme_const char *, unsigned long, int));
int tme_sjlj_rwlock_unlock _TME_P((struct tme_sjlj_rwlock *, _tme_const char *, unsigned long));
#define tme_rwlock_init tme_sjlj_rwlock_init
#if defined(__FILE__) && defined(__LINE__)
#define tme_rwlock_rdlock(l) tme_sjlj_rwlock_lock(l, __FILE__, __LINE__, FALSE)
#define tme_rwlock_tryrdlock(l) tme_sjlj_rwlock_lock(l, __FILE__, __LINE__, TRUE)
#define tme_rwlock_unlock(l) tme_sjlj_rwlock_unlock(l, __FILE__, __LINE__)
#else  /* !defined(__FILE__) || !defined(__LINE__) */
#define tme_rwlock_rdlock(l) tme_sjlj_rwlock_lock(l, NULL, NULL, FALSE)
#define tme_rwlock_tryrdlock(l) tme_sjlj_rwlock_lock(l, NULL, NULL, TRUE)
#define tme_rwlock_unlock(l) tme_sjlj_rwlock_unlock(l, NULL, NULL)
#endif /* !defined(__FILE__) || !defined(__LINE__) */

#endif /* TME_NO_DEBUG_LOCKS */

/* since our thread model doesn't allow recursive locking, write locking
   is always the same as read locking: */
#define tme_rwlock_wrlock tme_rwlock_rdlock
#define tme_rwlock_trywrlock tme_rwlock_tryrdlock

/* with cooperative threads, it doesn't make any sense to wait for locks: */
#define tme_rwlock_timedrdlock(l, usec) tme_rwlock_tryrdlock(l)
#define tme_rwlock_timedwrlock(l, usec) tme_rwlock_trywrlock(l)

/* mutexes.  we use a read/write lock to represent a mutex, and always
   lock it for writing.  we do *not* allow recursive locking: */
#define tme_mutex_t tme_rwlock_t
#define tme_mutex_lock tme_rwlock_wrlock
#define tme_mutex_trylock tme_rwlock_trywrlock
#define tme_mutex_timedlock(t, usec) tme_mutex_trylock(t)
#define tme_mutex_unlock tme_rwlock_unlock
#define tme_mutex_init tme_rwlock_init

/* conditions: */
typedef int tme_cond_t;
#define tme_cond_init(x) do { } while (/* CONSTCOND */ 0)
void tme_sjlj_cond_wait_yield _TME_P((tme_cond_t *, tme_mutex_t *));
void tme_sjlj_cond_notify _TME_P((tme_cond_t *, int));
#define tme_cond_wait_yield tme_sjlj_cond_wait_yield
#define tme_cond_notify tme_sjlj_cond_notify

/* deadlock sleeping: */
#define TME_THREAD_TIMEDLOCK		(0)
#define TME_THREAD_DEADLOCK_SLEEP	abort

/* threads: */
typedef void (*tme_thread_t) _TME_P((void *));
void tme_sjlj_thread_create _TME_P((tme_thread_t, void *));
#define tme_thread_create tme_sjlj_thread_create
void tme_sjlj_yield _TME_P((void));
#define tme_thread_yield tme_sjlj_yield
void tme_sjlj_exit _TME_P((void));
#define tme_thread_exit tme_sjlj_exit

/* sleeping: */
void tme_sjlj_sleep _TME_P((unsigned long, unsigned long));
#define tme_thread_sleep tme_sjlj_sleep
void tme_sjlj_sleep_yield _TME_P((unsigned long, unsigned long));
#define tme_thread_sleep_yield tme_sjlj_sleep_yield

/* I/O: */
#define tme_thread_read read
#define tme_thread_write write
#define tme_thread_select select
int tme_sjlj_select_yield _TME_P((int, fd_set *, fd_set *, fd_set *, struct timeval *));
ssize_t tme_sjlj_read_yield _TME_P((int, void *, size_t));
ssize_t tme_sjlj_write_yield _TME_P((int, void *, size_t));
#define tme_thread_select_yield tme_sjlj_select_yield
#define tme_thread_read_yield tme_sjlj_read_yield
#define tme_thread_write_yield tme_sjlj_write_yield

#endif /* TME_THREADS_SJLJ */

/* on virtually all architectures, regardless of threading, or
   multiprocessing, or caching, or whatever, C assignments to and from
   size-aligned values in memory are atomic - i.e., it is not possible
   for any thread on any processor to see anything other than a
   completed update to an aligned value in memory.  however, just in
   case someday there is an architecture (or possibly a compiler!)
   where this isn't true, we try to capture all of the places where we
   make this assumption.

   it is much more likely that an architecture may not guarantee that
   an unaligned access (for those architectures that allow unaligned
   access) be atomic, so we try capture all of the places where we make
   that weaker assumption:

   with certain threading models, atomic writes are guaranteed
   because there's no preemption: */
#ifdef TME_THREADS_COOPERATIVE
#define TME_ALIGNED_ACCESS_ATOMIC
#define TME_UNALIGNED_ACCESS_ATOMIC
#define tme_memory_sequence_rdlock(l) do { } while (/* CONSTCOND */ 0)
#define tme_memory_sequence_wrlock(l) do { } while (/* CONSTCOND */ 0)
#define tme_memory_sequence_unlock(l) do { } while (/* CONSTCOND */ 0)
#else  /* !TME_THREADS_COOPERATIVE */
#define tme_memory_sequence_rdlock(l) tme_rwlock_rdlock(l)
#define tme_memory_sequence_wrlock(l) tme_rwlock_wrlock(l)
#define tme_memory_sequence_unlock(l) tme_rwlock_unlock(l)
#endif /* TME_THREADS_COOPERATIVE */

#ifdef TME_UNALIGNED_ACCESS_ATOMIC
#ifndef TME_ALIGNED_ACCESS_ATOMIC
#error "if unaligned access is always atomic, aligned access must always be atomic"
#endif /* !TME_ALIGNED_ACCESS_ATOMIC */
#endif /* TME_UNALIGNED_ACCESS_ATOMIC */

#ifdef TME_ALIGNED_ACCESS_ATOMIC
#define tme_memory_aligned_rdlock(l) do { } while (/* CONSTCOND */ 0)
#define tme_memory_aligned_wrlock(l) do { } while (/* CONSTCOND */ 0)
#define tme_memory_aligned_unlock(l) do { } while (/* CONSTCOND */ 0)
#else  /* !TME_ALIGNED_ACCESS_ATOMIC */
#define tme_memory_aligned_rdlock(l) tme_rwlock_rdlock(l)
#define tme_memory_aligned_wrlock(l) tme_rwlock_wrlock(l)
#define tme_memory_aligned_unlock(l) tme_rwlock_unlock(l)
#endif /* !TME_ALIGNED_ACCESS_ATOMIC */

#ifdef TME_UNALIGNED_ACCESS_ATOMIC
#define tme_memory_unaligned_rdlock(l) do { } while (/* CONSTCOND */ 0)
#define tme_memory_unaligned_wrlock(l) do { } while (/* CONSTCOND */ 0)
#define tme_memory_unaligned_unlock(l) do { } while (/* CONSTCOND */ 0)
#else  /* !TME_UNALIGNED_ACCESS_ATOMIC */
#define tme_memory_unaligned_rdlock(l) tme_rwlock_rdlock(l)
#define tme_memory_unaligned_wrlock(l) tme_rwlock_wrlock(l)
#define tme_memory_unaligned_unlock(l) tme_rwlock_unlock(l)
#endif /* !TME_UNALIGNED_ACCESS_ATOMIC */

/* on a host with strict alignment requirements, the alignment of a
   memory address being accessed makes us choose one of two
   strategies: if the address is misaligned we must do a sequence of
   memory accesses, with the memory rwlock locked for a sequence, else
   the address is aligned and we can do a single memory access with
   the memory rwlock locked for an aligned memory access.

   this choosing means branching, and when locking for an aligned
   access and locking for a sequence have the same cost (they're
   either both no-ops or both locks), we assume that always doing the
   sequence access is not costlier than the branching that would be
   needed for the choice: */
#if defined(TME_THREADS_COOPERATIVE) || !defined(TME_ALIGNED_ACCESS_ATOMIC)
#define TME_SEQUENCE_ACCESS_NOT_COSTLIER (TRUE)
#else  /* !TME_THREADS_COOPERATIVE && TME_ALIGNED_ACCESS_ATOMIC */
#define TME_SEQUENCE_ACCESS_NOT_COSTLIER (FALSE)
#endif /* TME_THREADS_COOPERATIVE || !TME_ALIGNED_ACCESS_ATOMIC */

#endif /* !_TME_THREADS_H */
