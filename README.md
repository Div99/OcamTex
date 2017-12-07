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

    `opam install extlib ocamllex menhir`
1. `make main` makes the `main.byte` executable.
1. `make test` runs the oUnit test suite.

Sublime Syntax plugin: https://github.com/Div99/OCamTeX

## OCamTex Syntax

- Use hard tabs - this affects the parsing of tables, lists, matrices and listed elements.

### Optional Head Section
```
|HEAD
	|title THIS IS OCAMLTEX
	|author Divyansh
	|date 11/22/33
	|fontsize 14
	|font tgtermes
	|margins 1
	|landscape
```
- The supported font sizes are 8pt, 9pt, 10pt, 11pt, 12pt, 14pt, 17pt, 20pt.
- The supported fonts are tgtermes, mathptmx, txfonts.
- Margins are measured in inches.
- Not specifying font or fontsize will resort to the default LaTeX font and fontsize.
- Not specifying the date will resort to the current day.

### Variables
Written as :<var name>. An example is :inf, which would produce the infinity symbol. They are meant to act as shortcuts for frequently used Latex symbols. A list of supported variables are:
```
:arrow		=> 	➡
:larrow		=>	⬅
:lrarrow	=>	↔
:Arrow		=>	⇒
:Larrow     =>	⇐
:Lrarrow	=>	⇔
:or		=>	⋁
:and		=>	⋀
:union		=>	∪
:intersect	=>	∩
:nats		=>	ℕ
:ints		=>	ℤ
:reals		=>	ℝ
:rats		=>	ℚ
:del, :grad	=>	∇
else,
:[command] => \[command]
```

## Comments
```
// This is a comment
/* This is also a comment
…
*/
```

## Matrices
```
|matrix -> []
    A	B
    C	D
```
- Supported styles: [], {}, ||, ||||

## Tables
```
|table -> 2 //number of columns
    A	B
    C	D
```
