# SPDX-License-Identifier: GPL-2.0-or-later
# This Makefile is for convenience only; it is not needed for building the package.

EMACS_BIN ?= emacs

.PHONY: all test view clean

all: test

# Run all tests, or only those matching SELECTOR, e.g. "make test SELECTOR=preset-".
# Set HTML_DIR to write each test's result there as HTML, to check it by eye,
# open the "index.html" it writes alongside them.
test:
	EMACS_BIN="$(EMACS_BIN)" \
	$(if $(HTML_DIR),HL_PROG_EXTRA_TEST_HTML_DIR="$(HTML_DIR)") \
	python ./tests/hl-prog-extra_tests.py \
		$(if $(SELECTOR),--selector "$(SELECTOR)")

# Export FILE to HTML, to check the highlighting by eye.
view:
	@test -n "$(FILE)" || { echo "Usage: make view FILE=path/to/file"; exit 1; }
	EMACS_BIN="$(EMACS_BIN)" python ./tests/hl-prog-extra_tests.py --view "$(FILE)"

clean:
	rm -f *.elc tests/*.elc
