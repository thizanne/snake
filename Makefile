all: byte
	js_of_ocaml snake.byte

byte:
	ocamlbuild -use-ocamlfind -pkgs js_of_ocaml-lwt,js_of_ocaml.ppx,lwt.ppx snake.byte
