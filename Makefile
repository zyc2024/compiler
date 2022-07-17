.SILENT: clean tests build doc
.PHONY: clean tests build doc

clean: 
	rm -rf _build
	find . -type f \( -iname \*.lexed \) -delete

build:
	dune build

tests:
	make clean
	make build
	dune test

doc:
	dune build @doc
	dune build @doc-private

