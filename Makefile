BASE_FLAGS=-out:nami
EXTRA_FLAGS=
FLAGS=$(BASE_FLAGS) $(EXTRA_FLAGS)
SRC=./src

.PHONY: build
build:
	odin build $(SRC) $(FLAGS)

# Use the new IR generation
ir: EXTRA_FLAGS=-define:EXPERIMENTAL_IR=true
ir: build

.PHONY: test
test:
	odin test $(SRC) $(FLAGS)

builtins.o: builtins.odin
	odin build builtins.odin -file -build-mode:obj -no-entry-point -use-single-module
