.SILENT: clean test build doc
.PHONY: clean test build doc

clean: 
	# rm -rf _build
	find . -type f \( -iname \*.lexed \) -delete

build:
	dune build

test:
	clear
	make clean
	make build
	dune exec test-lex

doc:
	dune build @doc
	dune build @doc-private

