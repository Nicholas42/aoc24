DUNE=opam exec -- dune
days=$(patsubst bin/%.ml,%,$(wildcard bin/dec*))
day_targets=$(sort $(filter dec%,$(MAKECMDGOALS)) $(days))
time_targets=$(patsubst dec%,time_dec%,$(days))
is_silent=$(findstring s,$(firstword -$(MAKEFLAGS)))

.PHONY: today
today:
	$(MAKE) $(shell date +"dec%d")

.PHONY: all
all: $(days)

.PHONY: $(day_targets)
$(day_targets): dec%: inputs/dec%.txt
ifeq ($(is_silent),)
	$(info Computing solutions for $@)
endif
	$(DUNE) exec $@ -- $<

.PHONY: check
check:
	./run_check.sh

.PHONY: time
time: build
	$(MAKE) $(time_targets)

.PHONY: $(time_targets)
$(time_targets): time_dec%: build
	time $(MAKE) $(patsubst time_dec%,dec%,$@)


.PHONY: build
build:
	$(DUNE) build

inputs/dec%.txt:
	echo $@ | grep -oP '\d\d' | xargs ./get_input.sh > /dev/null

.PHONY: clean
clean:
	rm -rf inputs/*

.NOTPARALLEL:
.SILENT:
