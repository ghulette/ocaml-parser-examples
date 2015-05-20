SOURCES = expr_ast.ml expr_parser.mly expr_lexer.ml expr.mli expr.ml main.ml
RESULT = fobl
PACKS = sedlex menhirLib

OCAMLYACC = menhir

all : byte-code native-code htdoc

include OCamlMakefile
