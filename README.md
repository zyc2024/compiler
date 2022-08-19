# Evo

## What is this?

Evo is a Java and C inspired language. A quick reference can be found [here](./specifications/evo_spec.pdf).  This repository contains a compiler named Evoke intended to compile from Evo to x86-64 and various other architectures. 

### Status

This project is under heavy development. Reaching the first stable build will entail having the following:
 - A lexer
 - A parser
 - An AST
 - A typechecker
 - An IR generator
 - Converting IR into SSA form
 - Converting to LLVM IR and using LLVM to produce executables

As of writing this, this project has (written and tested) a lexer, parser, and AST. 

### Next Steps

To typecheck a given input program represented in a valid Abstract Syntax Tree (AST),
we need to store the typing context of imports to resolve unbounded values. 

Implementation Idea: When imports are evaluated, the compiler first searches for an interface (.evi). 
If no such interface is found, the compiler looks for the implementation file (.evo). 
The existence of an .evi implies that there must be a .evo and a .evo must exists
for another module to include as an import. 

If there's an interface, the interface's typing context is returned to the import caller.
The interface must have a corresponding implementation file and to typecheck, 
the module should implement everything specified in the interface. During the
typechecking process, we check for whether there are cyclic dependencies between
modules. The interface and implementation together forms a single node in the
dependency graph and when a cycle is created from imports, the compiler fails to
typecheck.  

To simplify the task of code generation, each occurrence of a variable will be
renamed to indicate the actual referenced variable. This is necessary in siutations
where the same variable name is used in declarations across different scopes. 

### Future Plans

Upon reaching a stable build, the following features will be considered:

 - Parametric polymorphism (generics)
 - More functional aspects (first class functions)
 - Automatic type inference

These features have been agreed on as being 

## Bulding the project

1. install opam [see instructions online](https://ocaml.org/docs/up-and-running)

2. opam switch create evo ocaml-base-compiler.4.13.0

3. opam install dune merlin ocaml-isp-server ocamlformat menhir odoc ANSITerminal sedlex ounit bisect_ppx

4. opam user-setup install

5. download all source code, cd into compiler directory and run: dune build

6. ./evoke-build # you may have to give permission with chmod +x evoke-build

7. Now you're all set to use evoke from any directory locally.

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
  - -verbose
    * Output messages about what the compiler is doing

This message is printed by default when the compiler is called without any command line arguments.
