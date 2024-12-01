DUNE=opam exec -- dune
days=$(patsubst bin/%.ml,%,$(wildcard bin/dec*))
day_targets=$(sort $(filter dec%,$(MAKECMDGOALS)) $(days))

.PHONY: all
all: $(days)

.PHONY: $(day_targets)
$(day_targets): dec%: inputs/dec%.txt
	$(info Computing solutions for $@)
	$(DUNE) build
	$(DUNE) exec $@ -- $<

inputs/dec%.txt:
	echo $@ | grep -oP '\d\d' | xargs ./get_input.sh > /dev/null

.PHONY: clean
clean:
	rm -rf inputs/*

.NOTPARALLEL:
.SILENT:
