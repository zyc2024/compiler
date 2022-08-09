.SILENT: clean test build doc bisect coverage
.PHONY: clean test build doc

clean: 
	# rm -rf _build
	find . -type f \( -iname \*.lexed -o -iname \*.parsed -o -iname \
		\*.coverage \) -delete

build:
	dune build

test:
	clear
	make clean
	make build
	echo "==================Lexing========================"
	dune exec test-lex
	echo "================================================"
	echo "==================Parsing======================="
	dune exec test-parse
	echo "================================================"

doc:
	dune build @doc
	dune build @doc-private

menhir:
	cd ./src/parser && menhir --dump parser.mly

bisect:
	make clean
	dune exec --instrument-with bisect_ppx test-sexp
	dune exec --instrument-with bisect_ppx test-lexer
	
coverage:
	bisect-ppx-report html
	# open _coverage/index.html to see coverage info

