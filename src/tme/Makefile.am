## Process this file with automake to produce Makefile.in
# Makefile.am for The Machine Emulator tme/:

AUTOMAKE_OPTIONS = 1.4 gnu

SUBDIRS = generic ic machine

pkginclude_HEADERS = atomics.h \
	common.h \
	connection.h \
	element.h \
	threads.h \
	module.h \
	log.h \
	tme.h \
	tmesh.h

install-exec-local:
	sort -u < tme-plugins.txt > tme-plugins.txt.final
	mv tme-plugins.txt.final tme-plugins.txt
	$(mkinstalldirs) $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) tme-plugins.txt $(DESTDIR)$(pkglibdir)
