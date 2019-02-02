# OCamTex
Creating a more intuitive LaTeX language and raising it to modern standards. This project is geared toward simplicity to make editing in LaTex faster and less cubersome, especially for math-related writing for research papers or HWs. 

We want to make writing LaTeX code a pleasant task and eliminate the need to work with LaTeX's obscure syntax.
![Ocamtex](ocamtex.png)

## Features
   * Inituitive and efficient user interface, designed for **fast editing**.
   * **Automatic Math mode**: Smart interpreter automatically puts math-like content in Math mode. [Eliminates '$']
   * **Indentation Styling**: Use indentation for nesting commands.
   * **Preserves newline**: Just press "enter" to create new-lines, instead of ugly "\\\\" syntax.
   * **[SublimeText Plugin](https://github.com/Div99/OCamTeX_sublime)**: Provides syntax higlighting, auto-compilation commands.

## Pre-requisites

1. Install required packages for latex compilation - this is required even if just running the precompiled executable.
    - For Mac: Install [MacTex](https://www.tug.org/mactex)
    - For Linux: Install Tex Live, do `sudo apt-get install texlive-latex-recommended`
    - For Windows: Install [MikTex](https://miktex.org/download) or [Tex Live](http://www.tug.org/texlive/acquire-netinstall.html)
   
2. Install **Ocaml** per instructions on [this page](https://opam.ocaml.org/doc/Install.html).
3. Install required Opam dependencies - we use extlib for file IO.

    `opam install extlib ocamllex menhir`

## Installation

Clone the repo, compile `main.byte`, and place it in system's path.

## Compilation

1. `make` or `make test` will run the oUnit test suite.
1. `make main` will build the `main.byte` executable.
1. `make tex` will build the `main.byte` and attempts to run it on the default file `test.otex`.
1. `make clean` will delete generated files produced by the make and build systems, as well as any `.aux`, `.log`, `.tex`, and `.pdf` files.

## Usage

Run the executable to get corresponding `.tex` and `.pdf` outputs.

```./main.byte [filename].otex```
    
**OR**

Use SublimeText Plugin to automatically build *.otex files using its build system.
  
## OCamTeX Syntax:

### Head section and document formatting
This is an optional head section that can take in various parameters set for the whole document, represented by the head command.

There are multiple commands that can be used to specify format options. These include title, author, date, margins, font, fontsize, landscape. An example use case is shown below.

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

The supported font sizes are 8pt, 9pt, 10pt, 11pt, 12pt, 14pt, 17pt, 20pt.

The supported fonts are tgtermes, mathptmx, txfonts.

Margins are measured in inches.

Not specifying font or fontsize will resort to the default LaTeX font and fontsize.

Not specifying the date will resort to the current day.

### Body section and relevant syntax:

Commands include tables, lists, pictures, matrices. They are initialized by `|<cmd>`. Command names are written in all lowercase. All commands end by a new line and can be nested. Everything that is not a command will be either parsed automatically either as text or math mode.  We use `->` to pass in parameters for command style, allowing the user to control formatting.

Variables: are written as `:<var name>`. An example is `:inf`, which would produce the infinity symbol. They are meant to act as shortcuts for frequently used Latex symbols. A list of supported variables are:

```
:arrow		=> 	➡
:larrow		=>	⬅
:lrarrow	=>	↔
:Arrow		=>	⇒
:Larrow	=>	⇐
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
```

By default, `:[command] => \[command]`

#### Comments
There are two ways to use comments. They are both based on Java commenting syntax.

The first method is for single line comments: double slashes followed by the comment string.

```
// <comment text>
```

The second method is for multi-line block comments, which use the backslash star combo.

```
/*
<comment text>
*/
```

#### Matrices

Uses matrix command and specifies formatting of borders from one of the four displayed choices. It defaults to square brackets if a border type is not specified. We then separate each entry in a row by a tab/spaces and separate rows by the new line character, `\n`.

The entries must be hard tabbed.

```
|matrix -> [],{},||,||||
    A	B
    C	D
```

#### Tables
Uses the table command. Rows are separated by new line character and columns by tab characters. The style parameter passed in specifies the number of columns.

The entries must be hard tabbed.

```
|table -> 2
    A	B
    C	D
```

#### Lists
We represent the list command as shown below where the first line after initialization sets up the list level configuration where choices are lowercase letters (`alph`), uppercase letters (`Alph`), numbers (`Arabic`), lowercase Roman letters (`roman`), and uppercase Roman letters (`Roman`).

Dashes signify items within a list.

Lists are nestable. The entries must be hard tabbed. The number of tabs indicates what list an entry should appear in.

```
|list -> (a), (1), (i), (A), (I)
    - 1       
    |list
        - 1.5
        - 1.7
    - 2
    - 3
```

#### Pictures
For pictures, rather than having to use the graphicx package and `\includegraphics`, we signal the importation of a picture with

```
|image -> <picture path>, width = <in terms of page e.g 0.5>
```
Example: `|image -> camel.jpg, .2`

Images must be located in an `/images/` folder in the same directory.

#### Body Format
There are two ways to format the page.

`|newpage` will start a new page and write onto it.

`|<sub, subsub>section <section title>` will allow for three layers of depth of sections. The string that follows after the section command labels and titles the section.

#### Verbose
Verbose mode is started and ended by the use of three backticks as shown in the example. Any text written inside the backticks will be interpreted and output  as straight Latex, allowing a user to include more Latex functionality that we did not implement yet.

````
```
<pure LaTeX>
```
````

### Math Mode

Parse spaces within math mode so that a user does not need to escape a space.
For switching  to math mode, the user can use the cmd   

`|equation, eqn = <math text>`

For inline math mode, it can get a bit tricky to distinguish whether strings should be parsed in math or text mode. We try to use an intuitive scheme to simplify things:

1. Any string consisting of just letters and with length > 1, will be interpreted by default in text mode. We do this as it is unlikely that a user meant to use such a string as a math equation or a variable (operators in the language are not pure letters and  distinguishable).
1. A single letter string with the exception of `a`, `I` will be parsed in math mode as it is likely that such a string was meant to be used as a variable
1. We will deal with some other corner cases that arises with this choice, like handling text enclosed in parentheses, etc. with as natural choice as possible.
1. We give the user option to manually specify whether a string should be parsed in text mode by commands `|m [  …. string … ]` for math and `|t [ … string …]` for text.
