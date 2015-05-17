SOURCES = expr.mli expr.ml parser.mly lexer.ml main.ml
RESULT = fobl
PACKS = sedlex menhirLib

OCAMLYACC = menhir

all : byte-code native-code

include OCamlMakefile
