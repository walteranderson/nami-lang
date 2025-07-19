FLAGS=-out:nami

.PHONY: build
build:
	odin build . $(FLAGS)

.PHONY: test
test:
	odin test . $(FLAGS)
