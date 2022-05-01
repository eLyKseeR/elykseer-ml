
NAMEIT := LXR

.SUFFIXES:

.PHONY: default config clean clean-dep clean-all clean-doc tags doc

MAKECOQ := +$(MAKE) -r -f Makefile.coq

VFILES := $(shell find . -name \*.v | grep -v .\# | sed -e 's|^./||g')

default: Makefile.coq
	$(MAKECOQ)

time: Makefile.coq
	COQC=./time_coqc make

config Makefile.coq:
	echo -R src $(NAMEIT) $(VFILES) > _CoqProject
	coq_makefile -f _CoqProject -o Makefile.coq

clean:
	rm -f `find . -name \*~`
	-$(MAKECOQ) clean
	rm -rf `find . -name .coq-native -o -name .\*.aux -o -name \*.cache`

clean-dep:
	rm -f `find . -name \*.v.d`

clean-all: clean clean-doc clean-dep
	rm -f _CoqProject Makefile.coq Makefile.coq.conf stat_time.log `find . -name \*.time`

clean-doc:
	rm -f doc/$(NAMEIT).*.html doc/index.html doc/main.html doc/coqdoc.css

tags:
	coqtags $(VFILES)

doc:
	coqdoc --html -g -d doc -R . $(NAMEIT)

%.vo: %.v
	$(MAKECOQ) $@

%:
	$(MAKECOQ) $@
