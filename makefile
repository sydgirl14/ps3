all: ps3 bignum_tests rsa

ps3: bignum.ml
	ocamlbuild -use-ocamlfind bignum.byte

rsa: rsa_puzzle.ml
	ocamlbuild -use-ocamlfind rsa_puzzle.byte

bignum_tests: bignum_tests.ml
	ocamlbuild -use-ocamlfind bignum_tests.byte

clean:
	rm -rf _build *.byte
