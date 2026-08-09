# SPDX-License-Identifier: GPL-2.0-or-later
# This Makefile is for convenience only; it is not needed for building the package.

EMACS_BIN ?= emacs
# Prefer "python3" since many distributions don't install a plain "python",
# fall back for systems that only provide "python" (Windows for e.g.).
PYTHON_BIN ?= $(if $(shell command -v python3),python3,python)

.PHONY: all test view clean

all: test

# Run all tests, or only those matching SELECTOR, e.g. "make test SELECTOR=preset-".
# Set HTML_DIR to write each test's result there as HTML, to check it by eye,
# open the "index.html" it writes alongside them.
test:
	EMACS_BIN="$(EMACS_BIN)" \
	$(if $(HTML_DIR),HL_PROG_EXTRA_TEST_HTML_DIR="$(HTML_DIR)") \
	$(PYTHON_BIN) ./tests/hl-prog-extra_tests.py \
		$(if $(SELECTOR),--selector "$(SELECTOR)")

# Export FILE to HTML, to check the highlighting by eye.
view:
	@test -n "$(FILE)" || { echo "Usage: make view FILE=path/to/file"; exit 1; }
	EMACS_BIN="$(EMACS_BIN)" $(PYTHON_BIN) ./tests/hl-prog-extra_tests.py --view "$(FILE)"

clean:
	rm -f *.elc tests/*.elc
