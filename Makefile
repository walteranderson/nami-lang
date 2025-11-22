# FLAGS=-out:nami -define:EXPERIMENTAL_IR=true
BASE_FLAGS=-out:nami
EXTRA_FLAGS=
FLAGS=$(BASE_FLAGS) $(EXTRA_FLAGS)
SRC=./src

.PHONY: build
build:
	odin build $(SRC) $(FLAGS)

.PHONY: test
test:
	odin test $(SRC) $(FLAGS)

builtins.o: builtins.odin
	odin build builtins.odin -file -build-mode:obj -no-entry-point -use-single-module
