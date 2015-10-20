BISECT_DIR=$(shell ocamlfind query bisect)

.PHONY: clean build install uninstall default doc

default:
	@echo "available targets:"
	@echo "  build        compile sosa"
	@echo "  test         compile sosa_tests, a test suite"
	@echo "  coverage     compile sosa with instrumented bisect_ppx coverage"
	@echo "  cov_report   create a coverage report from the latest coverage run"
	@echo "  clean        remove build directory"
	@echo "  install      install via ocamlfind"
	@echo "  uninstall    unintall via ocamlfind"
	@echo "  merlinize    create .merlin file"
	@echo "  doc          create documentation"

build:
	ocamlbuild -use-ocamlfind -cflag -safe-string -I lib/src sosa.cmx sosa.cma sosa.cmxa sosa.cmxs

coverage:
	ocamlbuild -use-ocamlfind -package bisect_ppx -cflag -safe-string -I lib/src sosa.cmx sosa.cma sosa.cmxa sosa.cmxs

test:
	ocamlbuild -use-ocamlfind -package nonstd -package unix -package bigarray -cflag -safe-string -I lib/src -I lib/test main.native  && \
	rm -f main.native  && \
	mv _build/lib/test/main.native sosa_tests


clean:
	ocamlbuild -clean && \
	rm -f main.native

install:
	ocamlfind install sosa META \
		_build/lib/src/sosa.cmi \
		_build/lib/src/sosa.cmo \
		_build/lib/src/sosa.cmx \
		_build/lib/src/sosa.a \
		_build/lib/src/sosa.o \
		_build/lib/src/sosa.cma \
		_build/lib/src/sosa.cmxa \
		_build/lib/src/sosa.cmxs

uninstall:
	ocamlfind remove sosa

merlinize:
	echo 'S .' > .merlin
	echo 'B _build' >> .merlin

doc:
	cp lib/src/sosa.mlpack sosa.odocl && \
	ocamlbuild -I lib/src/ sosa.docdir/index.html && \
	rm sosa.docdir && \
	ln -s _build/sosa.docdir/ doc && \
	rm sosa.odocl


##ocamlfind ocamldoc -charset UTF-8 -keep-code -colorize-code -html lib/src/sosa.odocl -d doc/

cov_report:
	cp _build/sosa.cmp . && \
	bisect-ppx-report -html report_dir $(shell ls -t bisect*.out | head -1) && \
	rm sosa.cmp
