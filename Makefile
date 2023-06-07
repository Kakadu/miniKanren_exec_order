.PHONY: test test_kotlin test_ocaml test_caml test_racket
PROMOTE ?=

.DEFAULT_GOAL := test

test_kotlin:
	dune b $(PROMOTE) \
		@appendo1.kotlin \
		@appendo2.kotlin \
		@reverso.kotlin \
		@expo1.kotlin \
		@logo1.kotlin \

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
		@twines.ml \
		@thrines.ml \

test: test_racket test_caml test_kotlin


clean:
	dune clean
	$(RM) -r klogic/build

