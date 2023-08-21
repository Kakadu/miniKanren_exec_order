#### Collecting traces

After `dune test` they would be found in `/unif_count` and `unif_traces` directories. The counts and traces should be the same (miniKanren's should execute similarly)


#### Tests

  * Getting OCanren benchmarks: `opam exec -- make -C ocaml/bench`
  * Getting Racket benchmarks: `make -C racket/bench`
  * Getting Kotlin benchmarks is complicated. We annotated some Kotlin internals with if-statement that checks environment variable and seems that this check is not being optimized by JIT. We....
  * `mkae -C racket/bench` to calculate Racket benchmarks
  * `make -C ocaml/bench` to calculate OCaml benchmarks
  * `dune test unif_count` to calculate count of unifications ...
  * `dune test unif_trace` ... and get unification traces

#### System and Racket dependencies

  sudo apt install racket openjdk-17-jdk opam linux-tools-common -y
  raco pkg install benchmark pretty-format

The `linux-tools-common`` may require extra kernel-specific packages...

#### OCaml dependencies

  opam init --bare
  opam switch create 4.14.1+flambda --packages=ocaml-variants.4.14.1+options,ocaml-option-flambda
  opam install OCanren OCanren-ppx --deps-only -y
  opam install benchmark -y
