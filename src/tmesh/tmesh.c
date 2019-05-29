/* $Id: tmesh.c,v 1.1 2003/05/16 21:48:16 fredette Exp $ */

/* tmesh/tmesh.c - the tme shell: */

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
_TME_RCSID("$Id: tmesh.c,v 1.1 2003/05/16 21:48:16 fredette Exp $");

/* includes: */
#include <tme/tme.h>
#include <tme/tmesh.h>
#include <stdio.h>
#include <string.h>

/* types: */

/* an input buffer: */
struct _tmesh_input {
  FILE *_tmesh_input_fp;
  char _tmesh_input_buffer[1024];
  unsigned int _tmesh_input_buffer_head;
  unsigned int _tmesh_input_buffer_tail;
  int _tmesh_input_buffer_eof;
};

/* globals: */
const char *argv0;

/* our shell instance: */
static void *_tmesh;

/* our current input: */
static struct tmesh_io *_tmesh_io;

/* nonzero if we're doing the pre-threads commands: */
static int _tmesh_doing_pre_threads;

/* our log and its mutex: */
static FILE *_tmesh_log;
static tme_mutex_t _tmesh_log_mutex;

/* this removes all consumed characters from a buffer, and shifts
   everything else down: */
static void
_tmesh_remove_consumed(struct _tmesh_input *input)
{
  input->_tmesh_input_buffer_head -= input->_tmesh_input_buffer_tail;
  memmove(input->_tmesh_input_buffer,
	  input->_tmesh_input_buffer
	  + input->_tmesh_input_buffer_tail,
	  input->_tmesh_input_buffer_head);
  input->_tmesh_input_buffer_tail = 0;
}

/* our pre-getc function: */
static int
_tmesh_pre_getc(struct tmesh_io *io)
{
  struct _tmesh_input *input;
  int c;

  /* recover our input: */
  input = io->tmesh_io_private;

  /* get the next character: */
  c = getc(input->_tmesh_input_fp);
  
  return (c == EOF ? TMESH_C_EOF : c);
}

/* our getc function: */
static int
_tmesh_getc(struct tmesh_io *io)
{
  struct _tmesh_input *input;
  int c;

  /* recover our input: */
  input = io->tmesh_io_private;

  /* if the buffer isn't empty yet, return the next character: */
  if (input->_tmesh_input_buffer_tail
      < input->_tmesh_input_buffer_head) {
    c = input->_tmesh_input_buffer[input->_tmesh_input_buffer_tail++];
    return (c);
  }

  /* if we're at EOF, return EOF: */
  if (input->_tmesh_input_buffer_eof) {
    return (TMESH_C_EOF);
  }

  /* otherwise, we must yield: */
  return (TMESH_C_YIELD);
}

/* our close function: */
static void
_tmesh_close(struct tmesh_io *io_old, struct tmesh_io *io_new)
{
  struct _tmesh_input *input;

  /* recover our input: */
  input = io_old->tmesh_io_private;

  /* close the file and free the input: */
  fclose(input->_tmesh_input_fp);
  tme_free(input);

  /* set the new, emerging input: */
  _tmesh_io = io_new;
  _tmesh_remove_consumed(io_new->tmesh_io_private);
}

/* our open function: */
static int
_tmesh_open(struct tmesh_io *io_new, struct tmesh_io *io_old, char **_output)
{
  struct _tmesh_input *input;
  int saved_errno;

  /* allocate a new input: */
  input = tme_new0(struct _tmesh_input, 1);

  /* try to open the file: */
  input->_tmesh_input_fp = fopen(io_new->tmesh_io_name, "r");
  
  /* if the open failed: */
  if (input->_tmesh_input_fp == NULL) {
    saved_errno = errno;
    tme_free(input);
    _tmesh_doing_pre_threads = FALSE;
    return (saved_errno);
  }

  /* set the new input: */
  io_new->tmesh_io_private = input;
  io_new->tmesh_io_getc = (_tmesh_doing_pre_threads
			   ? _tmesh_pre_getc
			   : _tmesh_getc);
  io_new->tmesh_io_close = _tmesh_close;
  io_new->tmesh_io_open = _tmesh_open;
  _tmesh_io = io_new;
  _tmesh_doing_pre_threads = FALSE;

  return (TME_OK);
}

/* our log output function: */
static void
_tmesh_log_output(struct tme_log_handle *handle)
{
  FILE *fp;
  
  /* lock the log mutex: */
  tme_mutex_lock(&_tmesh_log_mutex);

  /* if this is an error, report it to stderr, else it goes to the
     log: */
  fp = (handle->tme_log_handle_errno != TME_OK
	? stderr
	: _tmesh_log);

  fprintf(fp,
	  "[%s.%lu]",
	  (char *) handle->tme_log_handle_private,
	  handle->tme_log_handle_level);

  if (handle->tme_log_handle_message != NULL) {
    fprintf(fp, ": %s", handle->tme_log_handle_message);
    tme_free(handle->tme_log_handle_message);
    handle->tme_log_handle_message = NULL;
  }
  
  if (handle->tme_log_handle_errno != TME_OK) {
    fprintf(fp, ": %s", strerror(handle->tme_log_handle_errno));
  }

  fputc('\n', fp);

  /* unlock the log mutex: */
  tme_mutex_unlock(&_tmesh_log_mutex);
}

/* our log open function: */
static void 
_tmesh_log_open(struct tmesh_support *support, 
		struct tme_log_handle *handle,
		const char *pathname, 
		const char *module)
{
  handle->tme_log_handle_level_max = 0;
  handle->tme_log_handle_private = tme_strdup(pathname);
  handle->tme_log_handle_output = _tmesh_log_output;
}

/* our log close function: */
static void 
_tmesh_log_close(struct tmesh_support *support, 
		 struct tme_log_handle *handle)
{
  tme_free(handle->tme_log_handle_private);
}

/* our thread: */
static void
_tmesh_thread(void *junk)
{
  int yield, rc;
  struct tmesh_io *io;
  struct _tmesh_input *input;
  char *output;
  unsigned int consumed;

  /* loop while we have a current input buffer: */
  for (; (io = _tmesh_io) != NULL;) {
    input = io->tmesh_io_private;

    /* remove all consumed characters: */
    _tmesh_remove_consumed(input);

    /* if the current input buffer is full, a command is too long: */
    if (input->_tmesh_input_buffer_head
	== sizeof(input->_tmesh_input_buffer)) {
      fprintf(stderr, "%s: command too long\n", argv0);
      input->_tmesh_input_buffer_head = 0;
    }

    /* try to read more input: */
    rc = tme_thread_read_yield(fileno(input->_tmesh_input_fp),
			       input->_tmesh_input_buffer
			       + input->_tmesh_input_buffer_head,
			       sizeof(input->_tmesh_input_buffer)
			       - input->_tmesh_input_buffer_head);

    /* if the read failed: */
    if (rc < 0) {
      fprintf(stderr, "%s: %s\n",
	      io->tmesh_io_name,
	      strerror(errno));
      continue;
    }

    /* add characters in our current input buffer, or set EOF: */
    if (rc > 0) {
      input->_tmesh_input_buffer_head += rc;
    }
    else {
      input->_tmesh_input_buffer_eof = TRUE;
    }

    /* run commands until we have to yield: */
    for (;;) {

      /* all characters already read have been consumed: */
      consumed = input->_tmesh_input_buffer_tail;

      /* run a command: */
      rc = tmesh_eval(_tmesh, &output, &yield);

      /* if we're yielding: */
      if (yield) {

	/* if the current io has not changed, mark how many
	   characters were consumed by successful commands: */
	if (io == _tmesh_io) {
	  input->_tmesh_input_buffer_tail = consumed;
	}

	break;
      }

      /* this command may have changed the current io, so reload: */
      io = _tmesh_io;
      input = io->tmesh_io_private;

      /* display this command's output: */
      if (rc == TME_OK) {
	if (output != NULL
	    && *output != '\0') {
	  printf("%s\n", output);
	}
      }
      else {
	fprintf(stderr, "%s:%lu: ",
		io->tmesh_io_name,
		io->tmesh_io_input_line);
	if (output != NULL
	    && *output != '\0') {
	  fprintf(stderr, "%s: ", output);
	}
	fprintf(stderr, "%s\n", strerror(rc));
      }
      if (output != NULL) {
	tme_free(output);
      }

      /* put up the next prompt: */
      if (isatty(fileno(input->_tmesh_input_fp))
	  && isatty(fileno(stdout))) {
	printf("%s> ", argv0);
	fflush(stdout);
      }
    }
  }
}

int
main(int argc, char **argv)
{
  int usage;
  const char *opt;
  int arg_i;
  const char *pre_threads_filename;
  const char *log_filename;
  struct tmesh_io io;
  struct tmesh_support support;
  struct _tmesh_input *input_stdin;
  char *output;
  int yield, rc;

  /* check our command line: */
  usage = FALSE;
  pre_threads_filename = NULL;
  log_filename = "/dev/null";
  if ((argv0 = strrchr(argv[0], '/')) == NULL) argv0 = argv[0]; else argv0++;
  for (arg_i = 1;
       (arg_i < argc
	&& *argv[arg_i] == '-');
       arg_i++) {
    opt = argv[arg_i];
    if (!strcmp(opt, "--log")) {
      if (++arg_i < argc) {
	log_filename = argv[arg_i];
      }
      else {
	usage = TRUE;
	break;
      }
    }
    else {
      if (strcmp(opt, "-h")
	  && strcmp(opt, "--help")
	  && strcmp(opt, "-h")) {
	fprintf(stderr, "%s: unknown option %s\n",
		argv0, opt);
      }
      usage = TRUE;
      break;
    }
  }
  if (arg_i < argc) {
    pre_threads_filename = argv[arg_i++];
  }
  else {
    usage = TRUE;
  }
  if (usage) {
    fprintf(stderr, "\
usage: %s [OPTIONS] INITIAL-CONFIG\n\
where OPTIONS are:
  --log LOGFILE          log to LOGFILE\n\
",
	    argv0);
    exit(1);
  }

  if (!strcmp(log_filename, "-")) {
    _tmesh_log = stdout;
  }
  else {
    _tmesh_log = fopen(log_filename, "a");
    if (_tmesh_log == NULL) {
      perror(log_filename);
      exit(1);
    }
  }

  /* initialize libtme: */
  (void) tme_init();

  /* initialize libtmesh: */
  (void) tmesh_init();

  /* create our stdin input buffer, and stuff it with the command to
     source the pre-threads commands: */
  input_stdin = tme_new0(struct _tmesh_input, 1);
  input_stdin->_tmesh_input_fp = stdin;
  snprintf(input_stdin->_tmesh_input_buffer,
	   sizeof(input_stdin->_tmesh_input_buffer) - 1,
	   "source %s\n",
	   pre_threads_filename);
  input_stdin->_tmesh_input_buffer[sizeof(input_stdin->_tmesh_input_buffer) - 1] = '\0';
  input_stdin->_tmesh_input_buffer_head = strlen(input_stdin->_tmesh_input_buffer);

  /* create our stdin io: */
  io.tmesh_io_name = "*stdin*";
  io.tmesh_io_private = input_stdin;
  io.tmesh_io_input_line = 0;
  io.tmesh_io_getc = _tmesh_getc;
  io.tmesh_io_close = _tmesh_close;
  io.tmesh_io_open = _tmesh_open;

  /* the next open we do will be for the pre-threads commands: */
  _tmesh_doing_pre_threads = TRUE;

  /* create our support: */
  tme_mutex_init(&_tmesh_log_mutex);
  support.tmesh_support_log_open = _tmesh_log_open;
  support.tmesh_support_log_close = _tmesh_log_close;

  /* create our shell: */
  _tmesh = tmesh_new(&support, &io);

  /* run commands until we get a yield: */
  for (;;) {
    rc = tmesh_eval(_tmesh, &output, &yield);
    if (yield) {
      break;
    }
    if (rc == TME_OK) {
      if (output != NULL
	  && *output != '\0') {
	printf("%s\n", output);
      }
    }
    else {
      fprintf(stderr, "%s:%lu: ",
	      io.tmesh_io_name,
	      io.tmesh_io_input_line);
      if (output != NULL
	  && *output != '\0') {
	fprintf(stderr, "%s: ", output);
      }
      fprintf(stderr, "%s\n", strerror(rc));
    }
    if (output != NULL) {
      tme_free(output);
    }
  }

  /* put up our first prompt: */
  if (isatty(fileno(stdin))
      && isatty(fileno(stdout))) {
    printf("%s> ", argv0);
    fflush(stdout);
  }

  /* create our thread: */
  tme_thread_create((tme_thread_t) _tmesh_thread, NULL);

  /* run the threads: */
  tme_threads_run();

  /* done: */
  exit(0);
}
