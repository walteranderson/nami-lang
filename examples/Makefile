COMPILER=../nami
SRC=$(wildcard *.nami)
TARGETS=$(patsubst %.nami,%,$(SRC))

all: $(TARGETS)

%: %.nami
	$(COMPILER) $<

clean:
	rm -f $(TARGETS) *.ssa *.s

