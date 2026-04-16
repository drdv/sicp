RACO_TEST := raco test
OUT_DIR := out
DOCS_DIR := docs

.PHONY: help test-1 test-2 docs clean

help: URL := github.com/drdv/makefile-doc/releases/latest/download/makefile-doc.awk
help: DIR := $(HOME)/.local/share/makefile-doc
help: SCR := $(DIR)/makefile-doc.awk
help: VFLAG := -v SUB='$$(OUT_DIR)/meetup-logo.png:$(OUT_DIR)/meetup-logo.png:'
help: ## Show this help
	@test -f $(SCR) || wget -q -P $(DIR) $(URL)
	@awk $(VFLAG) -f $(SCR) $(MAKEFILE_LIST)

## Generate latex note
notes.tex:
	@tectonic notes.tex

## Run a selected EXERCISE from a selected SECTION
# There doesn't seem to be a way to run the tests in a specific submodule using raco test
# `raco test -s Exercise/1.10 sicp1.rkt` runs `Exercise/1.10` but not its nested test submodule
test: SECTION := sicp1
test: EXERCISE := 1.1
test:
	racket -e '(require (submod "$(SECTION).rkt" Exercise/$(EXERCISE) test))'

## Run tests for exercises in Chapter 1
test-1:
	$(RACO_TEST) sicp1.rkt

## Run tests for exercises in Chapter 2
test-2: clean | $(OUT_DIR)
	$(RACO_TEST) sicp2_part1.rkt
	$(RACO_TEST) sicp2_part2.rkt
	$(RACO_TEST) sicp2_part3.rkt
	$(RACO_TEST) sicp2_part4.rkt

##% Generate docs (WIP)
docs: DOCS_NAME := sicp-drdv
docs: | $(OUT_DIR)
	@scribble --htmls \
		+m --redirect-main https://docs.racket-lang.org/ \
		--dest $(OUT_DIR) \
		$(DOCS_DIR)/$(DOCS_NAME).scrbl

## Generate logo for meetup
$(OUT_DIR)/meetup-logo.png: | $(OUT_DIR)
	@magick img/front.png -set page 2600x1467+100+0 -background '#0055aa' -flatten \
		-font DejaVu-Sans -pointsize 130 -fill white \
		-gravity center -annotate +600-300 "Let's study\nthe Wizard Book\ntogether" \
		-gravity center -pointsize 80 -annotate +600+400 "exercise by exercise ..." \
	$(OUT_DIR)/meetup-logo.png

## Clean generated output
clean:
	@rm -f $(OUT_DIR)/*.png
	@rm -f $(OUT_DIR)/*.svg

$(OUT_DIR):
	@mkdir -p $(OUT_DIR)
