FLAGS=-out:nami
SRC=./src

.PHONY: build
build:
	odin build $(SRC) $(FLAGS)

.PHONY: test
test:
	odin test $(SRC) $(FLAGS)
