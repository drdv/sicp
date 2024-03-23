_BLUE=\033[34m
_END=\033[0m

# canned recipe
define show =
echo -e "${_BLUE}============================================================${_END}" && \
echo -e "${_BLUE}[$@] ${1}${_END}" && \
echo -e "${_BLUE}============================================================${_END}"
endef

.PHONY: help
help: ## Show this help
	@grep -h '\s##\s' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "${_BLUE}%-15s${_END} %s\n", $$1, $$2}'

tex: ## Generate latex note
	tectonic notes.tex

test-1: ## Run tests for exercises in Chapter 1
	raco test sicp1.rkt

test-2: clean ## Run tests for exercises in Chapter 2
	# raco test sicp2_part1.rkt
	# raco test sicp2_part2.rkt
	raco test sicp2_part3.rkt

.PHONY: docs
docs: ## Generate docs
	cd docs && scribble mouse.scrbl

.PHONY: clean
clean:
	rm -rf out/*.png
	rm -rf out/*.svg
