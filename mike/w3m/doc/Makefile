.SUFFIXES: .texi .info .pdf

INSTALL      = /usr/local/bin/ginstall -c
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_INFO = install-info
prefix       = /usr/local/Cellar/emacs/26.1_1/Emacs.app/Contents
datarootdir  = ${prefix}/share
infodir      = ${datarootdir}/info
srcdir       = .
subdir       = doc
top_srcdir   = ..
ADDITIONAL_LOAD_PATH = NONE
GZIP_PROG    = /usr/bin/gzip
COMPRESS_INSTALL = yes

SHELL        = /bin/sh


MAKEINFO     = makeinfo
TEXI2PDF     = texi2pdf
EMACS        = /usr/local/bin/emacs
VANILLA_FLAG = -q -no-site-file
FLAGS        = $(VANILLA_FLAG) -batch -l ./w3mhack.el '$(ADDITIONAL_LOAD_PATH)'

## This is used to set the environment variable XEMACSDEBUG for
## XEmacs 21.5 in order to suppress warnings for Lisp shadows
## when XEmacs 21.5 starts.
XEMACSDEBUG = 

EMACSINFO    = cd $(top_srcdir) && $(XEMACSDEBUG) '$(EMACS)' $(FLAGS) -f w3mhack-makeinfo

INFO_DEPS_EN = emacs-w3m.info
INFO_DEPS_JA = emacs-w3m-ja.info
INFO_DEPS    = $(INFO_DEPS_EN) $(INFO_DEPS_JA)

# Use xetex if it is your taste.
PDFTEX=luatex

default: $(INFO_DEPS)

en: $(INFO_DEPS_EN)

ja: $(INFO_DEPS_JA)

version.texi: $(top_srcdir)/w3m.el
	@version=`cd $(top_srcdir)\
		&& $(XEMACSDEBUG) '$(EMACS)' $(FLAGS) -f w3mhack-version 2>/dev/null`;\
	echo "@set VERSION $$version"> version.texi

emacs-w3m.info: version.texi emacs-w3m.texi

emacs-w3m-ja.info: version.texi emacs-w3m-ja.texi

# We can use makeinfo to format Japanese info files by replacing
#	$(EMACSINFO) $$texi
# with
#	$(MAKEINFO) --no-split -o $@ $$texi
.texi.info:
	@texi=$<;\
	case "$$texi" in \
	  *-ja.texi) \
	    rm -f $@ ;\
	    $(EMACSINFO) $$texi ;;\
	  *) \
	    if test "$(MAKEINFO)" = "no"; then \
	      rm -f $@ ;\
	      $(EMACSINFO) $$texi ;\
	    else \
	      echo "Formatting $$texi using $(MAKEINFO)..." ;\
	      LC_ALL=C $(MAKEINFO) --no-split -o $@ $$texi ;\
	      echo "Formatting $$texi using $(MAKEINFO)...done" ;\
	    fi ;;\
	esac

install: $(INFO_DEPS)
	$(SHELL) $(top_srcdir)/mkinstalldirs "$(DESTDIR)$(infodir)"
	@for file in $(INFO_DEPS); do\
	  if test -f $$file; then\
	    rm -f "$(DESTDIR)$(infodir)"/$$file{,-[0-9],.info,.info-[0-9]};\
	    rm -f "$(DESTDIR)$(infodir)"/$$file{,-[0-9],.info,.info-[0-9]}.gz;\
	    echo "$(INSTALL_DATA) $$file \"$(DESTDIR)$(infodir)/$$file\"";\
	    $(INSTALL_DATA) $$file "$(DESTDIR)$(infodir)/$$file";\
	    if test $(COMPRESS_INSTALL) = yes -a -n "$(GZIP_PROG)"; then\
	      "$(GZIP_PROG)" -9n "$(DESTDIR)$(infodir)/$$file";\
	    fi;\
	  else : ; fi;\
	done
	@if $(SHELL) -c $(INSTALL_INFO)' --version| sed 1q\
		| fgrep -s -v -i debian'>/dev/null 2>&1; then\
	  for file in $(INFO_DEPS); do\
	    if test -f "$(DESTDIR)$(infodir)/$$file"\
	      -o -f "$(DESTDIR)$(infodir)/$$file.gz"; then\
	      echo "$(INSTALL_INFO) --info-dir=\"$(DESTDIR)$(infodir)\" \"$(DESTDIR)$(infodir)/$$file\"";\
	      $(INSTALL_INFO) --info-dir="$(DESTDIR)$(infodir)" "$(DESTDIR)$(infodir)/$$file" || :;\
	    else : ; fi\
	  done;\
	else : ; fi

install-en: $(INFO_DEPS_EN)
	$(MAKE) INFO_DEPS='$(INFO_DEPS_EN)' $(XEMACSDEBUG) EMACS="$(EMACS)"\
		infodir="$(infodir)" install

install-ja: $(INFO_DEPS_JA)
	$(MAKE) INFO_DEPS='$(INFO_DEPS_JA)' $(XEMACSDEBUG) EMACS="$(EMACS)"\
		infodir="$(infodir)" install

pdf: pdf-en pdf-ja

pdf-en: emacs-w3m.pdf

pdf-ja: emacs-w3m-ja.pdf

emacs-w3m.pdf: version.texi emacs-w3m.texi

emacs-w3m-ja.pdf: version.texi emacs-w3m-ja.texi

.texi.pdf:
	sed '/^@dircategory/d' $< > w3mtmp.texi
	env PDFTEX=$(PDFTEX) $(TEXI2PDF) -o $@ w3mtmp.texi

clean:
	-rm -f *.info *.pdf *~ version.texi w3mtmp.*

distclean: clean
	-rm -f Makefile
