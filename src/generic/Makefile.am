## Process this file with automake to produce Makefile.in
# Makefile.am for The Machine Emulator generic/:

AUTOMAKE_OPTIONS = 1.4 gnu

INCLUDES = -I$(top_srcdir) -I$(srcdir) -I. -D_TME_IMPL

lib_LTLIBRARIES = libtme-generic.la
libtme_generic_la_SOURCES = bus.c bus-device.c serial.c ethernet.c
libtme_generic_la_LDFLAGS = -version-info 0:0:0

pkglib_LTLIBRARIES = tme_generic.la
tme_generic_la_SOURCES = bus-el.c
tme_generic_la_LDFLAGS = -module -version-info 0:0:0
tme_generic_la_LIBADD = libtme-generic.la

include $(top_srcdir)/modules
