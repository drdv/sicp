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
docs:
	@mkdir -p $(DOCS_DIR)
	@cd $(DOCS_DIR) && scribble mouse.scrbl

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
