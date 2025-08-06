FLAGS=-out:nami
SRC=./src

.PHONY: build
build:
	odin build $(SRC) $(FLAGS)

.PHONY: test
test:
	odin test $(SRC) $(FLAGS)

builtins.o: builtins.odin
	odin build builtins.odin -file -build-mode:obj -no-entry-point -use-single-module
