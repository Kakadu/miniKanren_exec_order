# TODO: heap specification
# Also
# sudo cpupower frequency-set --governor performance
# taskset -c 0 ../../_build/default/ocaml/bench/twines_bench.exe -raw

CPUPOWER := $(shell command -v cpupower 2> /dev/null)
REPEAT?=40
all:
	dune b ./all_benchmarks.exe --profile=release
ifdef CPUPOWER
	sudo cpupower frequency-set --governor performance
endif
	OCAMLRUNPARAM='s=500M,h=500M' taskset -c 0 \
		../../_build/default/ocaml/bench/all_benchmarks.exe -raw -r $(REPEAT)
