# 3110-Final
OCamTex - Creating a more intuitive TeX language and raising the current standards.

![Ocamtex](ocamtex.png)

## Usage

1. Install required packages for latex compilation - this is required even if just running the precompiled executable.

    `sudo apt-get install texlive-latex-recommended`
1. Run the executable to get corresponding `.tex` and `.pdf` outputs.

    `./main.byte [filename].otex`

## Compilation

1. Install Ocaml per instructions on [this page](https://opam.ocaml.org/doc/Install.html).
1. Install required Opam dependencies - we use extlib for file IO.

    `opam install extlib`
1. `make main` makes the `main.byte` executable.
1. `make test` runs the oUnit test suite.

Sublime Syntax plugin: https://github.com/Div99/OCamTeX
