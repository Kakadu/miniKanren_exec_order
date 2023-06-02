.PHONY: test test_kotlin test_ocaml test_caml test_racket
PROMOTE ?=

test_kotlin:
	dune b $(PROMOTE) \
		@appendo1.kotlin \
		@appendo2.kotlin \
		@reverso.kotlin

test_racket:
	dune b $(PROMOTE) \
		@appendo1.rkt \
		@appendo2.rkt \

test_ocaml: test_caml
test_caml:
	dune b $(PROMOTE) \
		@appendo1.ml \
		@appendo2.ml \
		@quines.ml \
		
test: test_kotlin test_racket test_kaml
