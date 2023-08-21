all: quines_bench_racket.sexp numero_bench_racket.sexp
CPUPOWER := $(shell command -v cpupower 2> /dev/null)

numero_bench.exe: numero_bench.rkt
	@$(RM) $@
	raco exe -v --cs -o $@ $^

quines_bench.exe: quines_bench.rkt
	@$(RM) $@
	raco exe -v --cs -o $@ $^

mul255x255.exe: mul255x255.rkt
	@$(RM) $@
	raco exe -v --cs -o $@ $^

quines_demo.exe: quines_demo.rkt
	@$(RM) $@
	raco exe -v --cs -o $@ $^

.PHONY: numero_bench_racket.sexp
numero_bench_racket.sexp: numero_bench.exe
	@$(RM) $@   # %U means seconds
ifdef CPUPOWER
	sudo cpupower frequency-set --governor performance
endif
	taskset -c 0 ./$^

.PHONY: quines_bench_racket.sexp
quines_bench_racket.sexp: quines_bench.exe
	@ $(RM) $@
ifdef CPUPOWER
	sudo cpupower frequency-set --governor performance
endif
	taskset -c 0 ./$^

clean:
	$(RM) numero_bench_racket.sexp* numero_bench_racket.sexp* quines_bench.exe numero_bench.exe
