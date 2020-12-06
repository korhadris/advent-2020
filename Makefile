SOURCE=$(sort $(wildcard 2020-*.rkt))
TARGETS=$(patsubst %.rkt,compiled/%_rkt.zo,$(SOURCE))
RUNS=$(patsubst 2020-%.rkt,%,$(SOURCE))

.PHONY: all
all: $(TARGETS)

compiled/%_rkt.zo: %.rkt
	raco make $<

.PHONY: run
run: $(RUNS)

.PHONY: $(RUNS)
$(RUNS):
	racket 2020-$@.rkt
	@echo
