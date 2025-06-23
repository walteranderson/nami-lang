.PHONY: build
build:
	odin build . -out:nami

.PHONY: test
test:
	odin test . -out:nami
