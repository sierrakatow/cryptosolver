all: Cryptogramsolver

# These must be in the right order--no forward refs
FILES = Crypto.ml load.ml Dictionary.ml to_scheme.ml \
    decide.ml \
	main.ml

Cryptogramsolver: $(FILES)
	ocamlc -g -o Cryptogramsolver unix.cma str.cma $(FILES)

clean: 
	rm -f Cryptogramsolver *.cmi *.cmo
