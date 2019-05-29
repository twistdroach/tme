## Process this file with automake to produce Makefile.in
# Makefile.am for The Machine Emulator machine/sun:

AUTOMAKE_OPTIONS = 1.4 gnu

INCLUDES = -I$(top_srcdir) -I$(srcdir) -I. -D_TME_IMPL

pkglib_LTLIBRARIES = libtme-machine-sun.la
libtme_machine_sun_la_SOURCES = sun-mmu.c
libtme_machine_sun_la_LDFLAGS = -version-info 0:0:0
libtme_machine_sun_la_LIBADD = $(top_builddir)/libtme/libtme.la \
	$(top_builddir)/generic/libtme-generic.la

bin_SCRIPTS = tme-sun-idprom

EXTRA_DIST = tme-sun-idprom.pl

# target to make tme-sun-idprom:
tme-sun-idprom: tme-sun-idprom.pl
	sed -e s%/usr/pkg/bin/perl%$(PERL)% < $(srcdir)/tme-sun-idprom.pl > $@
