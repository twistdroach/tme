## Process this file with automake to produce Makefile.in
# Makefile.am for The Machine Emulator machine/sun2:

AUTOMAKE_OPTIONS = 1.4 gnu

INCLUDES = -I$(top_srcdir) -I$(srcdir) -I. -D_TME_IMPL

pkglib_LTLIBRARIES = tme_machine_sun2.la
tme_machine_sun2_la_SOURCES = sun2-mainbus.c sun2-mmu.c sun2-control.c sun2-impl.h
tme_machine_sun2_la_LDFLAGS = -module -version-info 0:0:0
tme_machine_sun2_la_LIBADD = $(top_builddir)/libtme/libtme.la \
	../sun/libtme-machine-sun.la

examplesdir = $(datadir)/examples/tme
examples_DATA = SUN2-MULTIBUS

EXTRA_DIST = $(examples_DATA)

include $(top_srcdir)/modules

#noinst_PROGRAMS = sun2-test
#sun2_test_SOURCES = sun2-test.c
#sun2_test_LDADD = libtme-sun2.la \
#	../sun/libtme-sun.la \
#	../../host/posix/libtme-posix.la \
#	../../host/bsd/libtme-bsd.la \
#	../../ic/m68k/libtme-m68k.la \
#        $(top_srcdir)/../vMac/CPU/libCPU.a \
#	../../ic/ic-vol0/libtme-ic-vol0.la \
#	../../bus/multibus/libtme-multibus.la \
#	../../generic/libtme-generic.la \
#	../../threads/libtme-threads.la

# target to make remote things:
#../sun/libtme-machine-sun.la \
#../../host/posix/libtme-posix.la \
#../../host/bsd/libtme-bsd.la \
#../../ic/m68k/libtme-m68k.la \
#../../ic/ic-vol0/libtme-ic-vol0.la \
#../../bus/multibus/libtme-multibus.la \
#../../generic/libtme-generic.la \
#../../threads/libtme-threads.la: force
#	cd `dirname $@` && $(MAKE) `basename $@`
#force: ;
