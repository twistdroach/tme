## Process this file with automake to produce Makefile.in
# Makefile.am for The Machine Emulator libtme/:

AUTOMAKE_OPTIONS = 1.4 gnu

INCLUDES = -I$(top_srcdir) -I$(srcdir) -I. $(INCLTDL) $(GTK_CFLAGS) -D_TME_IMPL

lib_LTLIBRARIES = libtme.la
libtme_la_SOURCES = misc.c \
	threads-sjlj.c	\
	module.c \
	element.c \
	log.c \
	hash.c \
	alloc.c
libtme_la_LDFLAGS = -version-info 0:0:0
libtme_la_LIBADD = $(LIBLTDL)
EXTRA_DIST = log-prf.c

# module.c needs to know the LTDL_SHLIBPATH_VAR that libltdl
# is using:
module.lo: shlibvar.h
shlibvar.h: $(top_builddir)/libltdl/config.h
	grep LTDL_SHLIBPATH_VAR $(top_builddir)/libltdl/config.h > $@
