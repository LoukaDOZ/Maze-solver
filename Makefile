CC=ocaml
CFLAGS=graphics.cma

install:
	sudo apt install -y -- 'opam'
	opam init -y --dot-profile ~/'.bashrc' --shell-setup --
	opam switch create 4.07.0
	opam switch 4.07.0
	opam install -y -- 'graphics'
	eval "$(opam env)"

uninstall:
	opam uninstall -y -- 'graphics'
	opam switch remove -y -- 4.07.0
	sudo apt remove -y -- 'opam'

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
