.SILENT: clean tests build
.PHONY: clean tests build

clean: 
	rm -rf _build
	find . -type f \( -iname \*.lexed \) -delete

build:
	dune build

tests:
	make clean
	make build
	dune test


