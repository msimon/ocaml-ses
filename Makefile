all:
	ocaml setup.ml -configure
	ocaml setup.ml -build

clean:
	ocaml setup.ml -clean

install:
	ocaml setup.ml -install

reinstall:
	ocaml setup.ml -reinstall

uninstall:
	ocaml setup.ml -uninstall
