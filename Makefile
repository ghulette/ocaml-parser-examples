SOURCES = expr.mli expr.ml tokenizer.ml parser.mly main.ml
RESULT = fobl
PACKS = sedlex

OCAMLYACC = menhir

all : native-code

include OCamlMakefile
