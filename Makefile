
.PHONY: clean build install uninstall default

default:
	@echo "available targets:"
	@echo "  build        compile sosa"
	@echo "  clean        remove build directory"
	@echo "  install      install via ocamlfind"
	@echo "  uninstall    unintall via ocamlfind"
	@echo "  merlinize    unintall via ocamlfind"
	@echo "  doc          create documentation"

build:
	ocamlbuild sosa.cmo sosa.cmx sosa.cma sosa.cmxa sosa.cmxs

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

