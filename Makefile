CC=ocaml
CFLAGS=-I `eval \`opam env\` && ocamlfind query graphics` graphics.cma

install-ocaml:
	sudo apt install -y -- 'ocaml' 'opam'
	opam init -y --dot-profile ~/'.bashrc' --shell-setup --
	opam install -y -- 'graphics' 'ocamlfind'
	eval "$(opam env)"

#Grille cubique, résolution automatique
run-rect : rect_main_auto.ml
	$(CC) $(CFLAGS) $^

#Grille cubique, résolution manuelle
run-rect-inter : rect_main_inter.ml
	$(CC) $(CFLAGS) $^

#Grille hexagonale, résolution automatique
run-hexa : hexa_main_auto.ml
	$(CC) $(CFLAGS) $^

#Grille hexagonale, résolution manuelle
run-hexa-inter : hexa_main_inter.ml
	$(CC) $(CFLAGS) $^