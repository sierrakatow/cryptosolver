all: Cryptogramsolver

# These must be in the right order--no forward refs
FILES = types.ml load.ml Dictionary.ml decide.ml to_scheme.ml \
	main.ml

Cryptogramsolver: $(FILES)
	ocamlc -g -o Cryptogramsolver unix.cma str.cma $(FILES)

clean: 
	rm -f Cryptogramsolver *.cmi *.cmo
