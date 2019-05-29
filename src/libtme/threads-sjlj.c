/* $Id: threads-sjlj.c,v 1.12 2005/02/17 13:23:35 fredette Exp $ */

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
_TME_RCSID("$Id: threads-sjlj.c,v 1.12 2005/02/17 13:23:35 fredette Exp $");

/* includes: */
#include <tme/threads.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <setjmp.h>

/* if we don't have GTK, fake a few definitions to keep things
   compiling: */
#ifdef HAVE_GTK
#ifndef G_ENABLE_DEBUG
#define G_ENABLE_DEBUG (0)
#endif /* !G_ENABLE_DEBUG */
#include <gtk/gtk.h>
#else  /* !HAVE_GTK */
typedef int gint;
typedef int GdkInputCondition;
typedef void *gpointer;
#define GDK_INPUT_READ		TME_BIT(0)
#define GDK_INPUT_WRITE		TME_BIT(1)
#define GDK_INPUT_EXCEPTION	TME_BIT(2)
#endif /* !HAVE_GTK */

/* thread states: */
#define TME_SJLJ_THREAD_STATE_BLOCKED		(1)
#define TME_SJLJ_THREAD_STATE_RUNNABLE		(2)
#define TME_SJLJ_THREAD_STATE_DISPATCHING	(3)

/* dispatcher longjmp values, different from the normal TRUE and FALSE
   because you can't call longjmp with a value of zero: */
#define TME_SJLJ_DISPATCHER_FALSE		(1)
#define TME_SJLJ_DISPATCHER_TRUE		(2)

/* types: */

/* a thread: */
struct tme_sjlj_thread {

  /* the all-threads list: */
  struct tme_sjlj_thread *next;
  struct tme_sjlj_thread **prev;

  /* the current state of the thread, and any state-related list that
     it is on: */
  int tme_sjlj_thread_state;
  struct tme_sjlj_thread *state_next;
  struct tme_sjlj_thread **state_prev;

  /* the thread function: */
  void *tme_sjlj_thread_func_private;
  tme_thread_t tme_sjlj_thread_func;

  /* any condition that this thread is waiting on: */
  tme_cond_t *tme_sjlj_thread_cond;

  /* the file descriptors that this thread is waiting on: */
  int tme_sjlj_thread_max_fd;
  fd_set tme_sjlj_thread_fdset_read;
  fd_set tme_sjlj_thread_fdset_write;
  fd_set tme_sjlj_thread_fdset_except;

  /* if nonzero, the amount of time that this thread is sleeping,
     followed by the time the sleep will timeout.  all threads with
     timeouts are kept on a sorted list: */
  struct timeval tme_sjlj_thread_sleep;
  struct timeval tme_sjlj_thread_timeout;
  struct tme_sjlj_thread *timeout_next;
  struct tme_sjlj_thread **timeout_prev;
#ifdef HAVE_GTK
  gint tme_sjlj_thread_timeout_tag;
#endif /* HAVE_GTK */
};

/* globals: */

/* the all-threads list: */
static struct tme_sjlj_thread *tme_sjlj_threads_all;

/* the timeout-threads list: */
static struct tme_sjlj_thread *tme_sjlj_threads_timeout;

/* the runnable-threads list: */
static struct tme_sjlj_thread *tme_sjlj_threads_runnable;

/* the dispatching-threads list: */
static struct tme_sjlj_thread *tme_sjlj_threads_dispatching;

/* the active thread: */
static struct tme_sjlj_thread *tme_sjlj_thread_active;

/* this dummy thread structure is filled before a yield to represent
   what, if anything, the active thread is blocking on when it yields: */
static struct tme_sjlj_thread tme_sjlj_thread_blocked;

/* this is set if the active thread is exiting: */
static int tme_sjlj_thread_exiting;

/* this is a jmp_buf back to the dispatcher: */
static jmp_buf tme_sjlj_dispatcher_jmp;

/* the main loop fd sets: */
static int tme_sjlj_main_max_fd;
static fd_set tme_sjlj_main_fdset_read;
static fd_set tme_sjlj_main_fdset_write;
static fd_set tme_sjlj_main_fdset_except;

/* for each file descriptor, any thread blocked on it: */
static struct tme_sjlj_thread *tme_sjlj_fd_thread[FD_SETSIZE];

/* what, if any, thread timeout caused this dispatch: */
static struct tme_sjlj_thread *tme_sjlj_thread_dispatched_timeout;

#ifdef HAVE_GTK

/* nonzero iff we're using the gtk main loop: */
static int tme_sjlj_using_gtk;

/* for each file descriptor, the GTK tag for the fd event source: */
static gint tme_sjlj_fd_tag[FD_SETSIZE];

/* this set iff the idle callback is set: */
static int tme_sjlj_idle_set;
#endif /* HAVE_GTK */

/* prototypes: */
static int tme_sjlj_dispatch _TME_P((int));

/* this initializes the threads system: */
void
tme_sjlj_threads_init(void)
{
  int fd;

#ifdef HAVE_GTK
  /* assume that we won't be using the GTK main loop: */
  tme_sjlj_using_gtk = FALSE;
  tme_sjlj_idle_set = FALSE;
#endif /* HAVE_GTK */

  /* there are no threads: */
  tme_sjlj_threads_all = NULL;
  tme_sjlj_threads_timeout = NULL;
  tme_sjlj_threads_runnable = NULL;
  tme_sjlj_threads_dispatching = NULL;
  tme_sjlj_thread_active = NULL;
  tme_sjlj_thread_exiting = FALSE;

  /* no threads are waiting on any fds: */
  tme_sjlj_main_max_fd = -1;
  FD_ZERO(&tme_sjlj_main_fdset_read);
  FD_ZERO(&tme_sjlj_main_fdset_write);
  FD_ZERO(&tme_sjlj_main_fdset_except);
  for (fd = 0; fd < FD_SETSIZE; fd++) {
    tme_sjlj_fd_thread[fd] = NULL;
  }

  /* initialize the thread-blocked structure: */
  tme_sjlj_thread_blocked.tme_sjlj_thread_cond = NULL;
  tme_sjlj_thread_blocked.tme_sjlj_thread_max_fd = -1;
  tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_sec = 0;
  tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_usec = 0;

  /* any dispatch is not a timeout dispatch: */
  tme_sjlj_thread_dispatched_timeout = NULL;
}

#ifdef HAVE_GTK
/* this initializes the threads system to use the GTK event loop: */
void
tme_sjlj_threads_gtk_init(void)
{
  char **argv;
  char *argv_buffer[3];
  int argc;

  /* if we've already initialized GTK: */
  if (tme_sjlj_using_gtk) {
    return;
  }

  /* conjure up an argv.  this is pretty bad: */
  argv = argv_buffer;
  argc = 0;
  argv[argc++] = "tmesh";
#if 1
  argv[argc++] = "--gtk-debug=signals";
#endif
  argv[argc] = NULL;
  gtk_init(&argc, &argv);
  
  /* we are now using GTK: */
  tme_sjlj_using_gtk = TRUE;
}
#endif /* HAVE_GTK */

/* this changes a thread's state: */
static void
_tme_sjlj_change_state(struct tme_sjlj_thread *thread, int state)
{
  
  /* if this thread is already the desired state, do nothing: */
  if (thread->tme_sjlj_thread_state == state) {
    return;
  }

  /* if the thread's current state is not BLOCKED: */
  if (thread->tme_sjlj_thread_state
      != TME_SJLJ_THREAD_STATE_BLOCKED) {

    /* the thread must be on a state list: */
    assert(thread->state_prev != NULL);

    /* remove it from that list: */
    *thread->state_prev = thread->state_next;
    if (thread->state_next != NULL) {
      thread->state_next->state_prev = thread->state_prev;
    }

    /* this thread is now on no list: */
    thread->state_prev = NULL;
    thread->state_next = NULL;
  }

  /* the thread must be on no state list: */
  assert(thread->state_prev == NULL);

  /* set the new state of the thread: */
  thread->tme_sjlj_thread_state = state;

  /* if the thread's desired state is not BLOCKED: */
  if (state != TME_SJLJ_THREAD_STATE_BLOCKED) {

    /* if the desired state is RUNNABLE, put this thread
       at the front of the runnable list: */
    if (state == TME_SJLJ_THREAD_STATE_RUNNABLE) {
      thread->state_prev = &tme_sjlj_threads_runnable;
    }

    /* otherwise, the desired state must be DISPATCHING: */
    else {
      assert(state == TME_SJLJ_THREAD_STATE_DISPATCHING);

      /* if there is an active thread, put this thread right after it,
	 so it will run right after the active thread yields: */
      if (tme_sjlj_thread_active != NULL) {
	thread->state_prev = &tme_sjlj_thread_active->state_next;
      }
      else {
	thread->state_prev = &tme_sjlj_threads_dispatching;
      }
    }

    /* finish linking the thread into the list: */
    thread->state_next = *thread->state_prev;
    *thread->state_prev = thread;
    if (thread->state_next != NULL) {
      thread->state_next->state_prev = &thread->state_next;
    }
  }
}

/* this dispatches a thread whose timeout has expired: */
static gint
tme_sjlj_dispatch_timeout(gpointer _thread)
{
  struct tme_sjlj_thread *thread;
  int rc;

  /* recover the thread: */
  thread = (struct tme_sjlj_thread *) _thread;

  /* set the timed-out thread indication: */
  assert(tme_sjlj_thread_dispatched_timeout == NULL);
  tme_sjlj_thread_dispatched_timeout = thread;

  /* dispatch this thread: */
  assert(tme_sjlj_threads_dispatching == NULL);
  _tme_sjlj_change_state(thread, TME_SJLJ_THREAD_STATE_DISPATCHING);
  rc = tme_sjlj_dispatch(1);
  assert(tme_sjlj_threads_dispatching == NULL);

  /* clear the timed-out thread indication: */
  tme_sjlj_thread_dispatched_timeout = NULL;

  return (rc);
}

/* this dispatches a thread whose fd condition has been met: */
static void
tme_sjlj_dispatch_fd(gpointer junk0, gint fd, GdkInputCondition junk1)
{
  struct tme_sjlj_thread *thread;

  /* recover the thread: */
  thread = tme_sjlj_fd_thread[fd];
  if (thread == NULL) {
    return;
  }

  /* nothing else should appear to be dispatching: */
  assert(tme_sjlj_thread_dispatched_timeout == NULL);

  /* dispatch this thread: */
  assert(tme_sjlj_threads_dispatching == NULL);
  _tme_sjlj_change_state(thread, TME_SJLJ_THREAD_STATE_DISPATCHING);
  tme_sjlj_dispatch(1);
  assert(tme_sjlj_threads_dispatching == NULL);
}

/* this dispatches all runnable threads during an idle: */
static int
tme_sjlj_dispatch_idle(gpointer junk0)
{
  struct tme_sjlj_thread *thread;
  
  /* if there are runnable threads, move them en masse to the
     dispatching list, and do the dispatch: */
  assert(tme_sjlj_threads_dispatching == NULL);
  thread = tme_sjlj_threads_runnable;
  if (thread != NULL) {
    thread->state_prev = &tme_sjlj_threads_dispatching;
    tme_sjlj_threads_dispatching = thread;
    tme_sjlj_threads_runnable = NULL;
    for (;
	 thread != NULL;
	 thread = thread->state_next) {
      thread->tme_sjlj_thread_state = TME_SJLJ_THREAD_STATE_DISPATCHING;
    }
    tme_sjlj_dispatch(1);
    assert(tme_sjlj_threads_dispatching == NULL);
  }

  /* if there are still runnable threads, do not remove this idle
     callback: */
#ifdef HAVE_GTK
  if (tme_sjlj_threads_runnable == NULL) {
    tme_sjlj_idle_set = FALSE;
  }
#endif /* HAVE_GTK */
  return (tme_sjlj_threads_runnable != NULL);
}

/* this dispatches all dispatching threads: */
static int
tme_sjlj_dispatch(volatile int passes)
{
  struct tme_sjlj_thread * volatile thread;
  struct tme_sjlj_thread *thread_other;
  volatile int rc;
  int rc_one;
  
  /* assume we will return TRUE, indicating that the callback that
     started this dispatching should not be removed: */
  rc = TRUE;

  /* dispatch the given number of passes over the dispatching threads: */
  for (; passes-- > 0; ) {
    for (tme_sjlj_thread_active = tme_sjlj_threads_dispatching;
	 (thread = tme_sjlj_thread_active) != NULL; ) {
      
      /* when this active thread yields, we'll return here, where we
	 will continue the inner dispatching loop: */
      rc_one = setjmp(tme_sjlj_dispatcher_jmp);
      if (rc_one) {
	if (rc_one == TME_SJLJ_DISPATCHER_FALSE) {
	  rc = FALSE;
	}
	else {
	  assert(rc_one == TME_SJLJ_DISPATCHER_TRUE);
	}
	continue;
      }

      /* run this thread.  if it happens to return, just call
         tme_sjlj_exit(): */
      (*thread->tme_sjlj_thread_func)(thread->tme_sjlj_thread_func_private);
      tme_sjlj_exit();
    }
  }

  /* if there are still dispatching threads, move them en masse to the
     runnable list: */
  thread = tme_sjlj_threads_dispatching;
  if (thread != NULL) {
    thread_other = tme_sjlj_threads_runnable;
    thread->state_prev = &tme_sjlj_threads_runnable;
    tme_sjlj_threads_runnable = thread;
    tme_sjlj_threads_dispatching = NULL;
    for (;; thread = thread->state_next) {
      thread->tme_sjlj_thread_state = TME_SJLJ_THREAD_STATE_DISPATCHING;
      if (thread->state_next == NULL) {
	thread->state_next = thread_other;
	if (thread_other != NULL) {
	  thread_other->state_prev = &thread->state_next;
	}
	break;
      }
    }
  }

#ifdef HAVE_GTK
  
  /* if threads are still runnable, but the idle callback isn't set,
     set it: */
  if (tme_sjlj_threads_runnable != NULL
      && !tme_sjlj_idle_set) {
    gtk_idle_add_priority(G_PRIORITY_DEFAULT_IDLE,
			  tme_sjlj_dispatch_idle,
			  NULL);
    tme_sjlj_idle_set = TRUE;
  }
#endif /* HAVE_GTK */

  return (rc);
}
      
/* this starts the threads dispatching: */
void
tme_sjlj_threads_run(void)
{
  struct tme_sjlj_thread *thread, *thread_other, **_prev;
  int fd;
  fd_set fdset_read_out;
  fd_set fdset_write_out;
  fd_set fdset_except_out;
  struct timeval now, *timeout, timeout_buffer;
  int rc;
  
#ifdef HAVE_GTK
  /* if we're using the GTK main loop, add a single idle callback and
     call gtk_main(): */
  if (tme_sjlj_using_gtk) {
    gtk_idle_add_priority(G_PRIORITY_DEFAULT_IDLE,
			  tme_sjlj_dispatch_idle,
			  NULL);
    tme_sjlj_idle_set = TRUE;
    gtk_main();
    return;
  }
#endif /* HAVE_GTK */

  /* otherwise, we have to use our own main loop: */
  
  /* loop while we have threads: */
  for (; tme_sjlj_threads_all != NULL; ) {

    /* get the current time: */
    gettimeofday(&now, NULL);

    /* set the timeout time for any thread timeouts that are new or
       just timed out, and insert-sort them into the timeouts list: */
    for (; (thread = tme_sjlj_threads_timeout) != NULL; ) {

      /* stop if we've run out of new timeouts to sort: */
      if (thread->tme_sjlj_thread_timeout.tv_sec != 0
	  || thread->tme_sjlj_thread_timeout.tv_usec != 0) {
	break;
      }

      /* this thread must be on the timeouts list: */
      assert(thread->timeout_prev == &tme_sjlj_threads_timeout);

      /* remove this thread from the timeouts list: */
      *thread->timeout_prev = thread->timeout_next;
      if (thread->timeout_next != NULL) {
	thread->timeout_next->timeout_prev = thread->timeout_prev;
      }

      /* set this thread's timeout: */
      thread->tme_sjlj_thread_timeout = now;
      thread->tme_sjlj_thread_timeout.tv_sec
	+= thread->tme_sjlj_thread_sleep.tv_sec;
      thread->tme_sjlj_thread_timeout.tv_usec
	+= thread->tme_sjlj_thread_sleep.tv_usec;
      for (; thread->tme_sjlj_thread_timeout.tv_usec >= 1000000; ) {
	thread->tme_sjlj_thread_timeout.tv_sec++;
	thread->tme_sjlj_thread_timeout.tv_usec -= 1000000;
      }

      /* find where to insert-sort this timeout back into the busy list: */
      for (_prev = &tme_sjlj_threads_timeout;
	   (thread_other = *_prev) != NULL;
	   _prev = &thread_other->timeout_next) {

	/* stop if this is the last timeout on the busy list,
	   or if the next timeout happens at the same time as
	   or later than the timeout we're inserting: */
	if ((thread_other->tme_sjlj_thread_timeout.tv_sec
	     > thread->tme_sjlj_thread_timeout.tv_sec)
	    || ((thread_other->tme_sjlj_thread_timeout.tv_sec
		 == thread->tme_sjlj_thread_timeout.tv_sec)
		&& (thread_other->tme_sjlj_thread_timeout.tv_usec
		    >= thread->tme_sjlj_thread_timeout.tv_usec))) {
	  break;
	}
      }

      /* put this thread back on the timeouts list: */
      if ((thread->timeout_next = *_prev) != NULL) {
	thread->timeout_next->timeout_prev = &thread->timeout_next;
      }
      thread->timeout_prev = _prev;
      *_prev = thread;
    }

    /* if the top timeout has expired: */
    thread = tme_sjlj_threads_timeout;
    if (thread != NULL) {
      if (thread->tme_sjlj_thread_timeout.tv_sec < now.tv_sec
	  || (thread->tme_sjlj_thread_timeout.tv_sec == now.tv_sec
	      && thread->tme_sjlj_thread_timeout.tv_usec <= now.tv_usec)) {

	/* reset the timeout to zero: */
	thread->tme_sjlj_thread_timeout.tv_sec = 0;
	thread->tme_sjlj_thread_timeout.tv_usec = 0;

	/* dispatch this timeout: */
	tme_sjlj_dispatch_timeout(thread);

	/* rerun the main loop: */
	continue;
      }
    }

    /* if we have file descriptors to select on, do a select: */
    if (tme_sjlj_main_max_fd >= 0) {

      /* make the fd sets: */
      fdset_read_out = tme_sjlj_main_fdset_read;
      fdset_write_out = tme_sjlj_main_fdset_write;
      fdset_except_out = tme_sjlj_main_fdset_except;

      /* make the select timeout: */

      /* if there are runnable threads, make this a poll: */
      if (tme_sjlj_threads_runnable != NULL) {
	timeout_buffer.tv_sec = 0;
	timeout_buffer.tv_usec = 0;
	timeout = &timeout_buffer;
      }

      /* otherwise, if there are threads with timeouts, timeout when
	 the next thread's timeout will expire: */
      else if (tme_sjlj_threads_timeout != NULL) {
	timeout_buffer = tme_sjlj_threads_timeout->tme_sjlj_thread_timeout;
	if (timeout_buffer.tv_usec < now.tv_usec) {
	  timeout_buffer.tv_sec--;
	  timeout_buffer.tv_usec += 1000000;
	}
	timeout_buffer.tv_sec -= now.tv_sec;
	timeout_buffer.tv_usec -= now.tv_usec;
	timeout = &timeout_buffer;
      }

      /* otherwise, block forever: */
      else {
	timeout = NULL;
      }

      /* do the select: */
      rc = select(tme_sjlj_main_max_fd + 1,
		  &fdset_read_out,
		  &fdset_write_out,
		  &fdset_except_out,
		  timeout);

      /* if some fds are ready, dispatch them: */
      if (rc > 0) {	
	for (fd = tme_sjlj_main_max_fd; fd >= 0; fd--) {
	  if (FD_ISSET(fd, &fdset_read_out)
	      || FD_ISSET(fd, &fdset_write_out)
	      || FD_ISSET(fd, &fdset_except_out)) {
	    
	    /* dispatch this file descriptor: */
	    tme_sjlj_dispatch_fd(NULL, fd, 0);

	    /* stop if there are no more file descriptors left in the
	       sets: */
	    if (--rc == 0) {
	      break;
	    }
	  }
	}
      }
    }
    
    /* dispatch an idle: */
    tme_sjlj_dispatch_idle(NULL);
  }

  /* all threads have exited: */
}

/* this creates a new thread: */
void
tme_sjlj_thread_create(tme_thread_t func, void *func_private)
{
  struct tme_sjlj_thread *thread;

  /* allocate a new thread and put it on the all-threads list: */
  thread = tme_new(struct tme_sjlj_thread, 1);
  thread->prev = &tme_sjlj_threads_all;
  thread->next = *thread->prev;
  *thread->prev = thread;
  if (thread->next != NULL) {
    thread->next->prev = &thread->next;
  }

  /* initialize the thread: */
  thread->tme_sjlj_thread_func_private = func_private;
  thread->tme_sjlj_thread_func = func;
  thread->tme_sjlj_thread_cond = NULL;
  thread->tme_sjlj_thread_max_fd = -1;
  thread->tme_sjlj_thread_sleep.tv_sec = 0;
  thread->tme_sjlj_thread_sleep.tv_usec = 0;
  thread->timeout_prev = NULL;

  /* make this thread runnable: */
  thread->tme_sjlj_thread_state = TME_SJLJ_THREAD_STATE_BLOCKED;
  thread->state_prev = NULL;
  thread->state_next = NULL;
  _tme_sjlj_change_state(thread,
			 TME_SJLJ_THREAD_STATE_RUNNABLE);
}

/* this makes a thread wait on a condition: */
void
tme_sjlj_cond_wait_yield(tme_cond_t *cond, tme_mutex_t *mutex)
{

  /* unlock the mutex: */
  tme_mutex_unlock(mutex);

  /* remember that this thread is waiting on this condition: */
  tme_sjlj_thread_blocked.tme_sjlj_thread_cond = cond;

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
    if (thread->tme_sjlj_thread_state == TME_SJLJ_THREAD_STATE_BLOCKED
	&& thread->tme_sjlj_thread_cond == cond) {
      
      /* if threads are being dispatched, move this found thread to
	 the dispatching state, else move it to the runnable state.
	 (it's possible that this function could be called from
	 outside the threads system, say, like from a GTK callback
	 handler of some sort.) */
      _tme_sjlj_change_state(thread,
			     (tme_sjlj_thread_active == NULL
			      ? TME_SJLJ_THREAD_STATE_RUNNABLE
			      : TME_SJLJ_THREAD_STATE_DISPATCHING));

      /* if we're not broadcasting this notification, stop now: */
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
  struct tme_sjlj_thread *thread;
  int blocked;
  int max_fd_old;
  int max_fd_new;
  int max_fd, fd;
  GdkInputCondition fd_condition_old;
  GdkInputCondition fd_condition_new;
#ifdef HAVE_GTK
  unsigned long usec_part;
#endif /* HAVE_GTK */
  int longjmp_value;

  /* get the active thread: */
  thread = tme_sjlj_thread_active;

  /* the thread must be dispatching: */
  assert(thread->tme_sjlj_thread_state
	 == TME_SJLJ_THREAD_STATE_DISPATCHING);

  /* assume that this thread is not blocked: */
  blocked = FALSE;

  /* assume that the dispatcher will return TRUE to keep the tag it
     was called on alive: */
  longjmp_value = TME_SJLJ_DISPATCHER_TRUE;

  /* see if this thread is blocked on a condition: */
  if (tme_sjlj_thread_blocked.tme_sjlj_thread_cond != NULL) {
    blocked = TRUE;
  }
  thread->tme_sjlj_thread_cond = tme_sjlj_thread_blocked.tme_sjlj_thread_cond;
  tme_sjlj_thread_blocked.tme_sjlj_thread_cond = NULL;

  /* see if this thread is blocked on any file descriptors: */
  max_fd_old = thread->tme_sjlj_thread_max_fd;
  max_fd_new = tme_sjlj_thread_blocked.tme_sjlj_thread_max_fd;
  max_fd = TME_MAX(max_fd_old, max_fd_new);
  for (fd = 0; fd <= max_fd; fd++) {

    /* the old and new conditions on this fd start out empty: */
    fd_condition_old = 0;
    fd_condition_new = 0;

    /* check the old fd sets: */
    if (fd <= max_fd_old) {
#define CHECK_FD_SET(fd_set, condition)	\
do {					\
  if (FD_ISSET(fd, &thread->fd_set)) {	\
    fd_condition_old |= condition;	\
  }					\
} while (/* CONSTCOND */ 0)
      CHECK_FD_SET(tme_sjlj_thread_fdset_read, GDK_INPUT_READ);
      CHECK_FD_SET(tme_sjlj_thread_fdset_write, GDK_INPUT_WRITE);
      CHECK_FD_SET(tme_sjlj_thread_fdset_except, GDK_INPUT_EXCEPTION);
#undef CHECK_FD_SET
    }

    /* check the new fd sets: */
    if (fd <= max_fd_new) {
#define CHECK_FD_SET(fd_set, condition)			\
do {							\
  if (FD_ISSET(fd, &tme_sjlj_thread_blocked.fd_set)) {	\
    fd_condition_new |= condition;			\
    FD_SET(fd, &thread->fd_set);			\
  }							\
  else {						\
    FD_CLR(fd, &thread->fd_set);			\
  }							\
} while (/* CONSTCOND */ 0)
      CHECK_FD_SET(tme_sjlj_thread_fdset_read, GDK_INPUT_READ);
      CHECK_FD_SET(tme_sjlj_thread_fdset_write, GDK_INPUT_WRITE);
      CHECK_FD_SET(tme_sjlj_thread_fdset_except, GDK_INPUT_EXCEPTION);
#undef CHECK_FD_SET
    }

    /* if this thread is blocked on this file descriptor: */
    if (fd_condition_new != 0) {
      blocked = TRUE;
    }

    /* if the conditions have changed: */
    if (fd_condition_new != fd_condition_old) {

      /* if there were old conditions, remove this fd: */
      if (fd_condition_old != 0) {

	/* there is now no thread blocking on this fd: */
	assert(tme_sjlj_fd_thread[fd] == thread);
	tme_sjlj_fd_thread[fd] = NULL;

#ifdef HAVE_GTK
	if (tme_sjlj_using_gtk) {

	  /* it should be safe to remove this fd, even if we're
	     currently in a callback for it.  if we happen to get a
	     callback for it later anyways, tme_sjlj_dispatch_fd()
	     will ignore it: */
	  gdk_input_remove(tme_sjlj_fd_tag[fd]);
	}
	else
#endif /* HAVE_GTK */
	  {

	    /* remove this fd from our main loop's fd sets: */
	    assert(fd <= tme_sjlj_main_max_fd);
	    FD_CLR(fd, &tme_sjlj_main_fdset_read);
	    FD_CLR(fd, &tme_sjlj_main_fdset_write);
	    FD_CLR(fd, &tme_sjlj_main_fdset_except);
	    if (fd == tme_sjlj_main_max_fd) {
	      for (; --tme_sjlj_main_max_fd > 0; ) {
		if (tme_sjlj_fd_thread[tme_sjlj_main_max_fd] != NULL) {
		  break;
		}
	      }
	    }
	  }
      }

      /* if there are new conditions, add this fd: */
      if (fd_condition_new != 0) {

	/* this thread is now blocking on this fd: */
	assert(tme_sjlj_fd_thread[fd] == NULL);
	tme_sjlj_fd_thread[fd] = thread;

#ifdef HAVE_GTK
	if (tme_sjlj_using_gtk) {
	  tme_sjlj_fd_tag[fd] = 
	    gdk_input_add(fd,
			  fd_condition_new,
			  tme_sjlj_dispatch_fd,
			  NULL);
	}
	else
#endif /* HAVE_GTK */
	  {

	    /* add this fd to main loop's relevant fd sets: */
	    if (fd_condition_new & GDK_INPUT_READ) {
	      FD_SET(fd, &tme_sjlj_main_fdset_read);
	    }
	    if (fd_condition_new & GDK_INPUT_WRITE) {
	      FD_SET(fd, &tme_sjlj_main_fdset_write);
	    }
	    if (fd_condition_new & GDK_INPUT_EXCEPTION) {
	      FD_SET(fd, &tme_sjlj_main_fdset_except);
	    }
	    if (fd > tme_sjlj_main_max_fd) {
	      tme_sjlj_main_max_fd = fd;
	    }
	  }
      }
    }
  }
  thread->tme_sjlj_thread_max_fd = max_fd_new;
  tme_sjlj_thread_blocked.tme_sjlj_thread_max_fd = -1;

  /* see if this thread is blocked for some amount of time: */
  if (tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_sec != 0
      || tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_usec != 0) {

#ifdef HAVE_GTK
    /* GTK timeouts only offer a resolution of one millisecond, so
       always convert the timeout into a whole number of milliseconds,
       rounding any fraction up to one: */
    if (tme_sjlj_using_gtk) {
      usec_part
	= (tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_usec
	   % 1000);
      if (usec_part) {
	tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_usec
	  += (1000 - usec_part);
      }
    }
#endif /* HAVE_GTK */

    assert(tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_usec < 1000000);
    blocked = TRUE;
  }

  /* if the sleep interval has changed, or if this thread
     didn't run because its sleep interval expired: */
  if ((tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_sec
       != thread->tme_sjlj_thread_sleep.tv_sec)
      || (tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_usec
	  != thread->tme_sjlj_thread_sleep.tv_usec)
      || ((thread->tme_sjlj_thread_sleep.tv_sec != 0
	   || thread->tme_sjlj_thread_sleep.tv_usec != 0)
	  && thread != tme_sjlj_thread_dispatched_timeout)) {

    /* free any old timeout: */
    if (thread->tme_sjlj_thread_sleep.tv_sec != 0
	|| thread->tme_sjlj_thread_sleep.tv_usec != 0) {

#ifdef HAVE_GTK
      if (tme_sjlj_using_gtk) {

	/* to be safe, if this timeout is why GTK is calling us back
	   right now, don't risk confusing GTK's main loop by removing
	   this timeout here.  instead, arrange for the dispatcher to
	   return FALSE so the GTK main loop will remove the timeout
	   automatically: */
	if (thread == tme_sjlj_thread_dispatched_timeout) {
	  longjmp_value = TME_SJLJ_DISPATCHER_FALSE;
	}

	/* otherwise, it should be safe to remove this timeout: */
	else {
	  gtk_timeout_remove(thread->tme_sjlj_thread_timeout_tag);
	}
      }
      else
#endif /* HAVE_GTK */
	{

	  /* this thread must be on the timeout list: */
	  assert(thread->timeout_prev != NULL);

	  /* remove this thread from the timeout list: */
	  *thread->timeout_prev = thread->timeout_next;
	  if (thread->timeout_next != NULL) {
	    thread->timeout_next->timeout_prev = thread->timeout_prev;
	  }

	  /* this thread is no longer on the timeout list: */
	  thread->timeout_prev = NULL;
	  thread->timeout_next = NULL;
	}
    }

    /* add any new timeout: */
    if (tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_sec != 0
	|| tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_usec != 0) {
#ifdef HAVE_GTK
      if (tme_sjlj_using_gtk) {
	thread->tme_sjlj_thread_timeout_tag =

	  /* XXX we have to call g_timeout_add_full here, because
	     there are no gtk_timeout_add_ functions that allow you to
	     specify the priority, and gtk_timeout_add() uses
	     G_PRIORITY_DEFAULT, which means our (usually very
	     frequent) timeouts always win over gtk's event handling,
	     meaning the gtk windows never update: */
	  g_timeout_add_full(G_PRIORITY_DEFAULT_IDLE,
			     ((tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_sec
			       * 1000)
			      + (tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_usec
				 / 1000)),
			     tme_sjlj_dispatch_timeout,
			     thread,
			     NULL);
      }
      else
#endif /* HAVE_GTK */
	{

	  /* this thread must not be on the timeout list: */
	  assert(thread->timeout_prev == NULL);

	  /* put this thread at the front of the timeout list with
	     zero wakeup time - the main loop will calculate the next
	     wakeup time and insert-sort the thread into its correct
	     spot: */
	  thread->tme_sjlj_thread_timeout.tv_sec = 0;
	  thread->tme_sjlj_thread_timeout.tv_usec = 0;
	  thread->timeout_next = tme_sjlj_threads_timeout;
	  thread->timeout_prev = &tme_sjlj_threads_timeout;
	  tme_sjlj_threads_timeout = thread;
	  if (thread->timeout_next != NULL) {
	    thread->timeout_next->timeout_prev = &thread->timeout_next;
	  }
	}
    }
  }
  thread->tme_sjlj_thread_sleep = tme_sjlj_thread_blocked.tme_sjlj_thread_sleep;
  tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_sec = 0;
  tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_usec = 0;

  /* if this thread is actually exiting, it must appear to be
     runnable, and it only isn't because it's exiting: */
  if (tme_sjlj_thread_exiting) {
    assert(!blocked);
    blocked = TRUE;
  }

  /* make any following thread on the runnable list the next active
     thread: */
  tme_sjlj_thread_active = thread->state_next;

  /* if this thread is blocked, move it to the blocked list: */
  if (blocked) {
    _tme_sjlj_change_state(thread, 
			   TME_SJLJ_THREAD_STATE_BLOCKED);

    /* if this thread is exiting: */
    if (tme_sjlj_thread_exiting) {

      /* remove this thread from the all-threads list: */
      *thread->prev = thread->next;
      if (thread->next != NULL) {
	thread->next->prev = thread->prev;
      }

      /* free this thread: */
      tme_free(thread);

      /* nothing is exiting any more: */
      tme_sjlj_thread_exiting = FALSE;
    }
  }

  /* jump back to the dispatcher: */
  longjmp(tme_sjlj_dispatcher_jmp, longjmp_value);
}

/* this sleeps: */
void
tme_sjlj_sleep(unsigned long sec, unsigned long usec)
{
  struct timeval then, now, timeout;
  int rc;
  
  /* get the wakeup time for the thread: */
  gettimeofday(&then, NULL);
  for (; usec >= 1000000; sec++, usec -= 1000000);
  if ((then.tv_usec += usec) >= 1000000) {
    sec++;
    then.tv_usec -= 1000000;
  }
  then.tv_sec += sec;
  
  /* select for the sleep period: */
  for (;;) {

    /* calculate the current timeout: */
    if ((now.tv_sec > then.tv_sec)
	|| (now.tv_sec == then.tv_sec
	    && now.tv_usec >= then.tv_usec)) {
      break;
    }
    timeout = then;
    if (timeout.tv_usec < now.tv_usec) {
      timeout.tv_sec--;
      timeout.tv_usec += 1000000;
    }
    timeout.tv_sec -= now.tv_sec;
    timeout.tv_usec -= now.tv_usec;

    /* do the select.  select returns 0 iff the timeout expires, so we
       can skip another gettimeofday and loop: */
    rc = select(-1, NULL, NULL, NULL, &timeout);
    if (rc == 0) {
      break;
    }

    /* loop to see if the timeout really expired: */
    gettimeofday(&now, NULL);
  }
}

/* this sleeps and yields: */
void
tme_sjlj_sleep_yield(unsigned long sec, unsigned long usec)
{

  /* set the sleep interval: */
  for (; usec >= 1000000; ) {
    sec++;
    usec -= 1000000;
  }
  tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_sec = sec;
  tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_usec = usec;

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
  struct timeval timeout_out;
  int rc;

  /* we can't deal if there are more than FD_SETSIZE fds: */
  assert(nfds <= FD_SETSIZE);

  /* in case we end up yielding, we need to save the original
     descriptor sets: */
  if (fdset_read_in != NULL) {
    tme_sjlj_thread_blocked.tme_sjlj_thread_fdset_read = *fdset_read_in;
  }
  if (fdset_write_in != NULL) {
    tme_sjlj_thread_blocked.tme_sjlj_thread_fdset_write = *fdset_write_in;
  }
  if (fdset_except_in != NULL) {
    tme_sjlj_thread_blocked.tme_sjlj_thread_fdset_except = *fdset_except_in;
  }

  /* do a polling select: */
  timeout_out.tv_sec = timeout_out.tv_usec = 0;
  rc = select(nfds, fdset_read_in, fdset_write_in, fdset_except_in, &timeout_out);
  if (rc != 0
      || (timeout_in != NULL
	  && timeout_in->tv_sec == 0
	  && timeout_in->tv_usec == 0)) {
    return (rc);
  }

  /* we are yielding.  zero any unused descriptor sets and set the
     timeout time: */
  tme_sjlj_thread_blocked.tme_sjlj_thread_max_fd = nfds - 1;
  if (fdset_read_in == NULL) {
    FD_ZERO(&tme_sjlj_thread_blocked.tme_sjlj_thread_fdset_read);
  }
  if (fdset_write_in == NULL) {
    FD_ZERO(&tme_sjlj_thread_blocked.tme_sjlj_thread_fdset_write);
  }
  if (fdset_except_in == NULL) {
    FD_ZERO(&tme_sjlj_thread_blocked.tme_sjlj_thread_fdset_except);
  }
  if (timeout_in != NULL) {
    tme_sjlj_thread_blocked.tme_sjlj_thread_sleep = *timeout_in;
    for (; tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_usec >= 1000000; ) {
      tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_sec++;
      tme_sjlj_thread_blocked.tme_sjlj_thread_sleep.tv_usec -= 1000000;
    }
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
  
  /* mark that this thread is exiting: */
  tme_sjlj_thread_exiting = TRUE;

  /* yield: */
  tme_thread_yield();
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
