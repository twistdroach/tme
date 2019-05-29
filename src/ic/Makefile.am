## Process this file with automake to produce Makefile.in
# Makefile.am for The Machine Emulator ic/:

AUTOMAKE_OPTIONS = 1.4 gnu

INCLUDES = -I$(top_srcdir) -I$(srcdir) -I. -D_TME_IMPL

SUBDIRS = @TME_IC_SUBDIRS@
DIST_SUBDIRS = m68k

pkglib_LTLIBRARIES = @TME_ICS@
EXTRA_LTLIBRARIES = tme_ic_am9513.la \
	tme_ic_z8530.la \
	tme_ic_mm58167.la

tme_ic_am9513_la_SOURCES = am9513.c
tme_ic_am9513_la_LDFLAGS = -rpath $(pkglibdir) -module -version-info 0:0:0
tme_ic_am9513_la_LIBADD = $(top_builddir)/libtme/libtme.la \
	$(top_builddir)/generic/libtme-generic.la 

tme_ic_z8530_la_SOURCES = z8530.c z8530reg.h
tme_ic_z8530_la_LDFLAGS = -rpath $(pkglibdir) -module -version-info 0:0:0
tme_ic_z8530_la_LIBADD = $(top_builddir)/libtme/libtme.la \
	$(top_builddir)/generic/libtme-generic.la

tme_ic_mm58167_la_SOURCES = mm58167.c
tme_ic_mm58167_la_LDFLAGS = -rpath $(pkglibdir) -module -version-info 0:0:0
tme_ic_mm58167_la_LIBADD = $(top_builddir)/libtme/libtme.la \
	$(top_builddir)/generic/libtme-generic.la

include $(top_srcdir)/modules
