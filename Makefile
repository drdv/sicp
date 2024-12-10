## Show this help
help:
	-@awk -f makefile-help-target.awk $(MAKEFILE_LIST)

## Generate latex note
tex:
	tectonic notes.tex

## Run tests for exercises in Chapter 1
test-1:
	raco test sicp1.rkt

## Run tests for exercises in Chapter 2
test-2: clean
	mkdir -p out #  FIXME: create from the code
	raco test sicp2_part1.rkt
	raco test sicp2_part2.rkt
	raco test sicp2_part3.rkt
	raco test sicp2_part4.rkt

## Generate docs
docs:
	cd docs && scribble mouse.scrbl

## Clean generated output
clean:
	rm -rf out/*.png
	rm -rf out/*.svg
