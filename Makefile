SOURCES = expr.ml tokenizer.ml parser.mly
RESULT = test
PACKS = sedlex

OCAMLYACC = menhir

all : native-code

include OCamlMakefile
