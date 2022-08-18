# Evo

## What is this?

Evo is a Java and C inspired language. A quick reference can be found [here](https://docs.google.com/document/d/1yYv6ZvSxqd7DV0c8jdOiRyiQUuv3o7miFSQGA9BvQdo/edit?usp=sharing).  This repository contains a compiler intended to compile from Evo to x86 (??)

<!-- Language syntax and design can be found [here](https://docs.google.com/document/d/12_BwkMjr5jHUhqg6X14LQBCUyMM4eOKd05uj9ksdHGA/edit?usp=sharing). -->

### Status

This project is under heavy development. Reaching the first stable build will entail having the following:
 - A lexer
 - A parser
 - An AST
 - A typechecker
 - An IR generator
 - A target code generator

As of writing this, this project has (written and tested) a lexer, parser, and AST. 

### Future Plans

Upon reaching a stable build, the following features will be implemented:

 - IR optimizations
 - Targetting LLVM IR
 - Parametric polymorphism
 - More functional aspects

These features have been agreed on as being 

## Building the project

1. opam switch create evo ocaml-base-compiler.4.13.0

2. opam install dune merlin ocaml-isp-server ocamlformat menhir odoc ANSITerminal sedlex ounit bisect_ppx

3. opam user-setup install


## How to use
The compiler can be run with the following flags:
  - -D `<dir>`
    *  Specify where to place generated diagnostic files
  - -d `<dir>`
    * Specify where to place generated assembly/executable output files
  - --debug
    * enable debugging mode for developers
  - --help, -help, -h
    * displays this help message and exits
  - --lex, -l
    * Generate output from lexical analysis
  - --parse, -p
    * Generate output from syntatic analysis
  - --sourcepath `<path>`, -sp `<path>`, -source `<path>`
    * Specify where to look for input files


