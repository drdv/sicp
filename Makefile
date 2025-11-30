OUT_DIR := out
RACO_TEST := raco test

.PHONY: help test-1 test-2 docs clean

help: URL := github.com/drdv/makefile-doc/releases/latest/download/makefile-doc.awk
help: DIR := $(HOME)/.local/share/makefile-doc
help: SCR := $(DIR)/makefile-doc.awk
help: VFLAG := -v SUB='$$(OPEN_TARGETS):open-:$(NOTES)'
help: ## Show this help
	@test -f $(SCR) || wget -q -P $(DIR) $(URL)
	@awk $(VFLAG) -f $(SCR) $(MAKEFILE_LIST)

## Generate latex note
notes.tex:
	tectonic notes.tex

## Run tests for exercises in Chapter 1
test-1:
	$(RACO_TEST) sicp1.rkt

## Run tests for exercises in Chapter 2
test-2: clean | $(OUT_DIR)
	$(RACO_TEST) sicp2_part1.rkt
	$(RACO_TEST) sicp2_part2.rkt
	$(RACO_TEST) sicp2_part3.rkt
	$(RACO_TEST) sicp2_part4.rkt

## Generate docs
docs:
	cd docs && scribble mouse.scrbl

## Clean generated output
clean:
	rm -f out/*.png
	rm -f out/*.svg

$(OUT_DIR):
	@mkdir -p $(OUT_DIR)
