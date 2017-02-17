all: verify verify_tests

verify: verify.ml
	ocamlbuild -use-ocamlfind verify.byte

verify_tests: verify_tests.ml
	ocamlbuild -use-ocamlfind verify_tests.byte

clean:
	rm -rf _build *.byte