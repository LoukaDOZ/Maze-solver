CC=ocaml
CFLAGS=graphics.cma

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