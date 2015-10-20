BISECT_DIR=$(shell ocamlfind query bisect)

.PHONY: clean build install uninstall default doc

default:
	@echo "available targets:"
	@echo "  build        compile sosa"
	@echo "  test         compile sosa_tests, a test suite"
	@echo "  coverage     compile sosa_test with instrumented bisect_ppx coverage"
	@echo "  cov_report   create a coverage report from the latest coverage run"
	@echo "  clean        remove build directory"
	@echo "  install      install via ocamlfind"
	@echo "  uninstall    unintall via ocamlfind"
	@echo "  merlinize    create .merlin file"
	@echo "  doc          create documentation"

build:
	ocamlbuild -use-ocamlfind -cflag -safe-string -I src/lib sosa.cmx sosa.cma sosa.cmxa sosa.cmxs

test:
	ocamlbuild -use-ocamlfind -package nonstd -package unix -package bigarray -cflag -safe-string -I src/lib -I src/test main.native  && \
	rm -f main.native  && \
	mv _build/src/test/main.native sosa_tests

coverage:
	ocamlbuild -use-ocamlfind -pkgs nonstd,unix,bigarray,bisect_ppx.fast -cflag -safe-string -I src/lib -I src/test main.native  && \
	rm -f main.native  && \
	mv _build/src/test/main.native sosa_tests

clean:
	ocamlbuild -clean && \
	rm -f main.native

install:
	ocamlfind install sosa META \
		_build/src/lib/sosa.cmi \
		_build/src/lib/sosa.cmo \
		_build/src/lib/sosa.cmx \
		_build/src/lib/sosa.a \
		_build/src/lib/sosa.o \
		_build/src/lib/sosa.cma \
		_build/src/lib/sosa.cmxa \
		_build/src/lib/sosa.cmxs

uninstall:
	ocamlfind remove sosa

merlinize:
	echo 'S .' > .merlin
	echo 'B _build' >> .merlin

doc:
	cp src/lib/sosa.mlpack sosa.odocl && \
	ocamlbuild -I src/lib/ sosa.docdir/index.html && \
	rm sosa.docdir && \
	ln -s _build/sosa.docdir/ doc && \
	rm sosa.odocl

##ocamlfind ocamldoc -charset UTF-8 -keep-code -colorize-code -html src/lib/sosa.odocl -d doc/

cov_report:
	cd _build && \
	bisect-ppx-report -html ../report_dir ../$(shell ls -t bisect*.out | head -1) && \
	cd -
