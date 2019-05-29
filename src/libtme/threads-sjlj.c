/* $Id: threads-sjlj.c,v 1.7 2003/05/16 21:48:12 fredette Exp $ */

/* libtme/threads-sjlj.c - implementation of setjmp/longjmp threads: */

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
_TME_RCSID("$Id: threads-sjlj.c,v 1.7 2003/05/16 21:48:12 fredette Exp $");

/* includes: */
#include <tme/threads.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <setjmp.h>

/* structures: */

/* a thread descriptor: */
struct tme_sjlj_thread {

  /* threads are kept on a doubly-linked list: */
  struct tme_sjlj_thread *next;
  struct tme_sjlj_thread **prev;

  /* the thread function: */
  void *tme_sjlj_thread_func_private;
  tme_thread_t tme_sjlj_thread_func;

  /* any condition that this thread is waiting on, a flag that is set
     when this thread is notified, and the associated mutex: */
  tme_cond_t *tme_sjlj_thread_cond;
  int tme_sjlj_thread_cond_notified;

  /* the file descriptors that this thread is waiting on: */
  int tme_sjlj_thread_max_fd;
  fd_set tme_sjlj_thread_fdset_read;
  fd_set tme_sjlj_thread_fdset_write;
  fd_set tme_sjlj_thread_fdset_except;

  /* if nonzero, the time that this thread is waiting until: */
  struct timeval tme_sjlj_thread_wakeup;
};

/* globals: */
static struct tme_sjlj_thread *tme_sjlj_threads_all;
static struct tme_sjlj_thread *tme_sjlj_thread_active;
static jmp_buf tme_sjlj_dispatcher;

/* this initializes the threads system: */
void
tme_sjlj_threads_init(void)
{

  /* there are no threads: */
  tme_sjlj_threads_all = NULL;
  tme_sjlj_thread_active = NULL;
}

/* this fills a struct timeval with some time in the future: */
static void
_tme_sjlj_then(struct timeval *then, unsigned long sec, unsigned long usec)
{
  gettimeofday(then, NULL);
  for (; usec >= 1000000; sec++, usec -= 1000000);
  if ((then->tv_usec += usec) >= 1000000) {
    sec++;
    then->tv_usec -= 1000000;
  }
  then->tv_sec += sec;
}

/* this subtracts one timeval from another to calculate a timeout: */
static void
_tme_sjlj_timeout(struct timeval *timeout, const struct timeval *then, const struct timeval *now)
{
  timeout->tv_sec = timeout->tv_usec = 0;
  if (now->tv_sec > then->tv_sec) {
    return;
  }
  if (now->tv_sec == then->tv_sec) {
    if (now->tv_usec >= then->tv_usec) {
      return;
    }
    timeout->tv_usec = then->tv_usec - now->tv_usec;
  }
  else {
    if (then->tv_usec < now->tv_usec) {
      timeout->tv_sec = (then->tv_sec - 1) - now->tv_sec;
      timeout->tv_usec = (then->tv_usec + 1000000) - now->tv_usec;
    }
    else {
      timeout->tv_sec = then->tv_sec - now->tv_sec;
      timeout->tv_usec = then->tv_usec - now->tv_usec;
    }
  }
}
  
/* this starts the threads dispatching: */
void
tme_sjlj_threads_run(void)
{
  struct tme_sjlj_thread *thread;
  int pass, runnable;
  int gates_total, gates_open;
  fd_set fdset_read_in, fdset_read_out;
  fd_set fdset_write_in, fdset_write_out;
  fd_set fdset_except_in, fdset_except_out;
  int fd, max_fd;
  struct timeval now, then, timeout;
  int now_defined;
  int rc;

  /* dispatch forever: */
  do { } while (setjmp(tme_sjlj_dispatcher));

  /* initialize max_fd to silence -Wuninitialized: */
  max_fd = -1;

  /* find the next runnable thread: */
  thread = tme_sjlj_thread_active;
  pass = 0;
  now_defined = FALSE;
  do {
    
    /* try to find another thread to check for runnability: */
    if (thread != NULL) {
      thread = thread->next;
    }
    if (thread == NULL) {
      
      /* we're about to pass over the entire threads list.  if we have
	 already made a pass, there aren't any runnable threads, so we
	 have to wait until something is runnable: */
      if (pass == 1) {
	
	/* copy the select masks: */
	fdset_read_out = fdset_read_in;
	fdset_write_out = fdset_write_in;
	fdset_except_out = fdset_except_in;

	/* if we don't have a wakeup time: */
	if (then.tv_sec == 0
	    && then.tv_usec == 0) {

	  /* if we have no file descriptors to select on, we are deadlocked: */
	  if (max_fd < 0) {
	    abort();
	  }

	  /* do the select: */
	  rc = select(max_fd + 1, &fdset_read_out, &fdset_write_out, &fdset_except_out, NULL);
	}

	/* otherwise, we have a wakeup time: */
	else {

	  /* calculate the sleep time: */
	  if (!now_defined) {
	    gettimeofday(&now, NULL);
	  }
	  _tme_sjlj_timeout(&timeout, &then, &now);

	  /* do the select: */
	  if (max_fd < 0) {
	    rc = select(-1, NULL, NULL, NULL, &timeout);
	  }
	  else {
	    rc = select(max_fd + 1, &fdset_read_out, &fdset_write_out, &fdset_except_out, &timeout);
	  }
	}

	/* since we just waited, our notion of the current time is
	   no longer correct: */
	now_defined = FALSE;
      }
      
      /* make a pass over the entire threads list.  if there are no threads,
	 return: */
      thread = tme_sjlj_threads_all;
      if (thread == NULL) {
	return;
      }
      pass = 1;
      max_fd = -1;
      FD_ZERO(&fdset_read_in);
      FD_ZERO(&fdset_write_in);
      FD_ZERO(&fdset_except_in);
      then.tv_sec = then.tv_usec = 0;
    }
    
    /* prepare to count the things gating this thread's runnability: */
    gates_total = gates_open = 0;
    
    /* if this thread is waiting on a condition: */
    if (thread->tme_sjlj_thread_cond != NULL) {
      gates_total++;

      /* if this thread was notified on that condition, this thread is
         runnable: */
      if (thread->tme_sjlj_thread_cond_notified) {
	gates_open++;
      }
    }
    
    /* if this thread is waiting for file descriptors: */
    if (thread->tme_sjlj_thread_max_fd >= 0) {
      gates_total++;
      
      /* if any of those file descriptors are ready, this thread is
         runnable: */
      fdset_read_out = thread->tme_sjlj_thread_fdset_read;
      fdset_write_out = thread->tme_sjlj_thread_fdset_write;
      fdset_except_out = thread->tme_sjlj_thread_fdset_except;
      timeout.tv_sec = timeout.tv_usec = 0;
      rc = select(thread->tme_sjlj_thread_max_fd + 1,
		  &fdset_read_out,
		  &fdset_write_out,
		  &fdset_except_out,
		  &timeout);
      if (rc > 0) {
	gates_open++;
      }

      /* otherwise add those file descriptors to the set we're waiting
         on: */
      else {
	for (fd = 0; fd <= thread->tme_sjlj_thread_max_fd; fd++) {
	  if (FD_ISSET(fd, &thread->tme_sjlj_thread_fdset_read)) {
	    FD_SET(fd, &fdset_read_in);
	    max_fd = TME_MAX(max_fd, fd);
	  }
	  if (FD_ISSET(fd, &thread->tme_sjlj_thread_fdset_write)) {
	    FD_SET(fd, &fdset_write_in);
	    max_fd = TME_MAX(max_fd, fd);
	  }
	  if (FD_ISSET(fd, &thread->tme_sjlj_thread_fdset_except)) {
	    FD_SET(fd, &fdset_except_in);
	    max_fd = TME_MAX(max_fd, fd);
	  }
	}
      }
    }

    /* if this thread has a wakeup time: */
    if (thread->tme_sjlj_thread_wakeup.tv_sec != 0
	|| thread->tme_sjlj_thread_wakeup.tv_usec != 0) {
      gates_total++;

      /* get now if we haven't already: */
      if (!now_defined) {
	gettimeofday(&now, NULL);
	now_defined = TRUE;
      }

      /* if the wakeup time has arrived, this thread is runnable: */
      if (thread->tme_sjlj_thread_wakeup.tv_sec < now.tv_sec
	  || (thread->tme_sjlj_thread_wakeup.tv_sec == now.tv_sec
	      && thread->tme_sjlj_thread_wakeup.tv_usec <= now.tv_usec)) {
	gates_open++;
      }

      /* otherwise, update the earliest wakeup time: */
      else {
	if ((then.tv_sec == 0
	     && then.tv_usec == 0)
	    || (then.tv_sec > thread->tme_sjlj_thread_wakeup.tv_sec
		|| (then.tv_sec == thread->tme_sjlj_thread_wakeup.tv_sec
		    && then.tv_usec > thread->tme_sjlj_thread_wakeup.tv_usec))) {
	  then = thread->tme_sjlj_thread_wakeup;
	}
      }
    }

    /* if this thread has nothing gating its runnability, or
       if even one of those gates is open, this thread is runnable: */
    runnable = (gates_total == 0 || gates_open > 0);

  } while (!runnable);

  /* run this thread.  if it happens to return, act like it exited: */
  tme_sjlj_thread_active = thread;
  thread->tme_sjlj_thread_cond = NULL;
  thread->tme_sjlj_thread_cond_notified = FALSE;
  thread->tme_sjlj_thread_max_fd = -1;
  thread->tme_sjlj_thread_wakeup.tv_sec = 0;
  thread->tme_sjlj_thread_wakeup.tv_usec = 0;
  (*thread->tme_sjlj_thread_func)(thread->tme_sjlj_thread_func_private);
  tme_sjlj_exit();
}

/* this creates a new thread: */
void
tme_sjlj_thread_create(tme_thread_t func, void *func_private)
{
  struct tme_sjlj_thread *thread;

  /* allocate a new thread descriptor and put it on the list. 
     if there is an active thread, put this thread after it so
     it runs after this one: */
  thread = tme_new(struct tme_sjlj_thread, 1);
  if (tme_sjlj_thread_active != NULL) {
    thread->prev = &tme_sjlj_thread_active->next;
  }
  else {
    thread->prev = &tme_sjlj_threads_all;
  }
  thread->next = *thread->prev;
  *thread->prev = thread;
  if (thread->next != NULL) {
    thread->next->prev = &thread->next;
  }

  /* fill the thread descriptor: */
  thread->tme_sjlj_thread_func_private = func_private;
  thread->tme_sjlj_thread_func = func;
  thread->tme_sjlj_thread_cond = NULL;
  thread->tme_sjlj_thread_max_fd = -1;
  thread->tme_sjlj_thread_wakeup.tv_sec = 0;
}

/* this makes a thread wait on a condition: */
void
tme_sjlj_cond_wait_yield(tme_cond_t *cond, tme_mutex_t *mutex)
{

  /* unlock the mutex: */
  tme_mutex_unlock(mutex);

  /* remember that this thread is waiting on this condition: */
  tme_sjlj_thread_active->tme_sjlj_thread_cond = cond;
  tme_sjlj_thread_active->tme_sjlj_thread_cond_notified = FALSE;

  /* yield: */
  tme_thread_yield();
}

/* this notifies one or more threads waiting on a condition: */
void
tme_sjlj_cond_notify(tme_cond_t *cond, int broadcast)
{
  struct tme_sjlj_thread *thread;

  for (thread = tme_sjlj_threads_all;
       thread != NULL;
       thread = thread->next) {
    if (thread->tme_sjlj_thread_cond == cond) {
      thread->tme_sjlj_thread_cond_notified = TRUE;
      if (!broadcast) {
	break;
      }
    }
  }
}
       
/* this yields the current thread: */
void
tme_sjlj_yield(void)
{
  longjmp(tme_sjlj_dispatcher, 1);
}

/* this sleeps: */
void
tme_sjlj_sleep(unsigned long sec, unsigned long usec)
{
  struct timeval then, now, timeout;
  
  /* get the wakeup time for the thread: */
  _tme_sjlj_then(&then, sec, usec);
  
  /* select for the sleep period: */
  for (;;) {
    gettimeofday(&now, NULL);
    _tme_sjlj_timeout(&timeout, &then, &now);
    if (timeout.tv_sec == 0
	&& timeout.tv_usec == 0) {
      return;
    }
    select(-1, NULL, NULL, NULL, &timeout);
  }
  /* NOTREACHED */
}

/* this sleeps and yields: */
void
tme_sjlj_sleep_yield(unsigned long sec, unsigned long usec)
{

  /* set the wakeup time for the thread: */
  _tme_sjlj_then(&tme_sjlj_thread_active->tme_sjlj_thread_wakeup,
		 sec, usec);

  /* yield: */
  tme_thread_yield();
}

/* this selects and yields: */
int
tme_sjlj_select_yield(int nfds,
		      fd_set *fdset_read_in,
		      fd_set *fdset_write_in,
		      fd_set *fdset_except_in,
		      struct timeval *timeout_in)
{
  fd_set fdset_read_out, fdset_read_in_buffer;
  fd_set fdset_write_out, fdset_write_in_buffer;
  fd_set fdset_except_out, fdset_except_in_buffer;
  struct timeval timeout_out;
  int rc;

  /* make sure we have all three descriptor sets: */
  if (fdset_read_in == NULL) {
    fdset_read_in = &fdset_read_in_buffer;
    FD_ZERO(fdset_read_in);
  }
  if (fdset_write_in == NULL) {
    fdset_write_in = &fdset_write_in_buffer;
    FD_ZERO(fdset_write_in);
  }
  if (fdset_except_in == NULL) {
    fdset_except_in = &fdset_except_in_buffer;
    FD_ZERO(fdset_except_in);
  }

  /* do a polling select: */
  fdset_read_out = *fdset_read_in;
  fdset_write_out = *fdset_write_in;
  fdset_except_out = *fdset_except_in;
  timeout_out.tv_sec = timeout_out.tv_usec = 0;
  rc = select(nfds, &fdset_read_out, &fdset_write_out, &fdset_except_out, &timeout_out);
  if (rc != 0) {
    *fdset_read_in = fdset_read_out;
    *fdset_write_in = fdset_write_out;
    *fdset_except_in = fdset_except_out;
    return (rc);
  }

  /* set the selecting file descriptors for this thread: */
  tme_sjlj_thread_active->tme_sjlj_thread_max_fd = nfds - 1;
  tme_sjlj_thread_active->tme_sjlj_thread_fdset_read = *fdset_read_in;
  tme_sjlj_thread_active->tme_sjlj_thread_fdset_write = *fdset_write_in;
  tme_sjlj_thread_active->tme_sjlj_thread_fdset_except = *fdset_except_in;

  /* set the wakeup time for the thread: */
  if (timeout_in != NULL) {
    _tme_sjlj_then(&tme_sjlj_thread_active->tme_sjlj_thread_wakeup,
		   timeout_in->tv_sec, timeout_in->tv_usec);
  }

  /* yield: */
  tme_thread_yield();
  /* NOTREACHED */
  return (0);
}

/* this reads, yielding if the fd is not ready: */
ssize_t
tme_sjlj_read_yield(int fd, void *data, size_t count)
{
  fd_set fdset_read_in;
  int rc;

  /* select on the fd for reading: */
  FD_ZERO(&fdset_read_in);
  FD_SET(fd, &fdset_read_in);
  rc = tme_sjlj_select_yield(fd + 1,
			     &fdset_read_in,
			     NULL,
			     NULL,
			     NULL);
  if (rc != 1) {
    return (rc);
  }

  /* do the read: */
  return (read(fd, data, count));
}

/* this writes, yielding if the fd is not ready: */
ssize_t
tme_sjlj_write_yield(int fd, void *data, size_t count)
{
  fd_set fdset_write_in;
  int rc;

  /* select on the fd for writing: */
  FD_ZERO(&fdset_write_in);
  FD_SET(fd, &fdset_write_in);
  rc = tme_sjlj_select_yield(fd + 1,
			     NULL,
			     &fdset_write_in,
			     NULL,
			     NULL);
  if (rc != 1) {
    return (rc);
  }

  /* do the write: */
  return (write(fd, data, count));
}

/* this exits a thread: */
void
tme_sjlj_exit(void)
{
  struct tme_sjlj_thread *thread;

  /* remove this thread from the list: */
  thread = tme_sjlj_thread_active;
  tme_sjlj_thread_active = thread->next;
  *thread->prev = thread->next;

  /* free this thread: */
  tme_free(thread);

  /* jump back to the dispatcher: */
  longjmp(tme_sjlj_dispatcher, 1);
}

#ifndef TME_NO_DEBUG_LOCKS

/* lock operations: */
int
tme_sjlj_rwlock_init(struct tme_sjlj_rwlock *lock)
{
  /* initialize the lock: */
  lock->_tme_sjlj_rwlock_locked = FALSE;
  lock->_tme_sjlj_rwlock_file = NULL;
  lock->_tme_sjlj_rwlock_line = 0;
  return (TME_OK);
}
int
tme_sjlj_rwlock_lock(struct tme_sjlj_rwlock *lock, _tme_const char *file, unsigned long line, int try)
{
  
  /* if this lock is already locked: */
  if (lock->_tme_sjlj_rwlock_locked) {
    if (try) {
      return (TME_EDEADLK);
    }
    abort();
  }

  /* lock the lock: */
  lock->_tme_sjlj_rwlock_locked = TRUE;
  lock->_tme_sjlj_rwlock_file = file;
  lock->_tme_sjlj_rwlock_line = line;
  return (TME_OK);
}
int
tme_sjlj_rwlock_unlock(struct tme_sjlj_rwlock *lock, _tme_const char *file, unsigned long line)
{
  
  /* if this lock isn't locked: */
  if (!lock->_tme_sjlj_rwlock_locked) {
    abort();
  }

  /* unlock the lock: */
  lock->_tme_sjlj_rwlock_locked = FALSE;
  lock->_tme_sjlj_rwlock_file = file;
  lock->_tme_sjlj_rwlock_line = line;
  return (TME_OK);
}

#endif /* !TME_NO_DEBUG_LOCKS */
