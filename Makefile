BISECT_DIR=$(shell ocamlfind query bisect)

.PHONY: clean build install uninstall default

default:
	@echo "available targets:"
	@echo "  build        compile sosa"
	@echo "  coverage     compile sosa with instrumented bisect_ppx coverage"
	@echo "  cov_report   create a coverage report from the latest coverage run"
	@echo "  clean        remove build directory"
	@echo "  install      install via ocamlfind"
	@echo "  uninstall    unintall via ocamlfind"
	@echo "  merlinize    create .merlin file"
	@echo "  doc          create documentation"

build:
	ocamlbuild -use-ocamlfind -I lib/src sosa.cmx sosa.cma sosa.cmxa sosa.cmxs

coverage:
	ocamlbuild -use-ocamlfind -package bisect_ppx sosa.cmo sosa.cmx sosa.cma sosa.cmxa sosa.cmxs

clean:
	ocamlbuild -clean

install:
	ocamlfind install sosa META \
		_build/sosa.cmi \
		_build/sosa.cmo \
		_build/sosa.cmx \
		_build/sosa.a \
		_build/sosa.o \
		_build/sosa.cma \
		_build/sosa.cmxa \
		_build/sosa.cmxs

uninstall:
	ocamlfind remove sosa

merlinize:
	echo 'S .' > .merlin
	echo 'B _build' >> .merlin

doc:
	mkdir -p doc
	ocamlfind ocamldoc  -charset UTF-8 -keep-code -colorize-code -html sosa.ml -d doc/

cov_report:
	cp _build/sosa.cmp . && \
	bisect-ppx-report -html report_dir $(shell ls -t bisect*.out | head -1) && \
	rm sosa.cmp
