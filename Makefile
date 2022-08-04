.SILENT: clean test build doc
.PHONY: clean test build doc

clean: 
	# rm -rf _build
	find . -type f \( -iname \*.lexed -o -iname \*.parsed \) -delete

build:
	dune build

test:
	clear
	make clean
	make build
	echo "Lexing ========================================="
	dune exec test-lex
	echo "================================================"

doc:
	dune build @doc
	dune build @doc-private

menhir:
	cd ./src/parser && menhir --dump parser.mly

