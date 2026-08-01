# SPDX-License-Identifier: GPL-2.0-or-later

import argparse
import subprocess
import json
import os
import sys


THIS_DIR = os.path.normpath(os.path.abspath(os.path.join(os.path.dirname(__file__))))
BASE_DIR = os.path.normpath(os.path.join(THIS_DIR, ".."))

EMACS_BIN = os.environ.get("EMACS_BIN") or "emacs"

# Ensure a stale `*.elc` is never used.
EMACS_ARGS = [EMACS_BIN, "-batch", "--eval", "(setq load-prefer-newer t)"]


def elisp_string(text: str) -> str:
    """Return TEXT as an Emacs Lisp string literal."""
    # Both languages use the same escapes for quotes, back-slashes and \\uXXXX.
    # NOTE: `ensure_ascii` must be disabled, JSON writes a character outside the
    # BMP as a surrogate pair, which Emacs reads as two separate characters.
    return json.dumps(text, ensure_ascii=False)


def subprocess_call(cmd: list) -> int:
    """Run command, streaming its output."""
    process = subprocess.Popen(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    for line in process.stdout:
        sys.stdout.write(line)
        sys.stdout.flush()

    return process.wait()


def run_hl_prog_extra_tests(selector: str | None) -> int:
    # Without a selector every test runs.
    run_fn = "(ert-run-tests-batch-and-exit%s)" % (
        " " + elisp_string(selector) if selector else ""
    )
    cmd = EMACS_ARGS + [
        "-l", os.path.join(THIS_DIR, "hl-prog-extra_tests.el"),
        "--eval", run_fn,
    ]
    return subprocess_call(cmd)


def run_hl_prog_extra_view(file_in: str, file_out: str) -> int:
    cmd = EMACS_ARGS + [
        "--eval", "(add-to-list 'load-path %s)" % elisp_string(BASE_DIR),
        "-l", os.path.join(THIS_DIR, "hl-prog-extra_html.el"),
        "--eval", "(hl-prog-extra-html-export-file %s %s)" % (
            elisp_string(file_in), elisp_string(file_out),
        ),
    ]
    return subprocess_call(cmd)


def argparse_create() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Run the tests, or export a file to HTML to check it by eye.",
    )
    parser.add_argument(
        "--selector",
        dest="selector",
        metavar="REGEX",
        default=None,
        help="Only run tests whose name matches this regular expression.",
    )
    parser.add_argument(
        "--view",
        dest="view",
        metavar="FILE",
        default=None,
        help="Export FILE to HTML instead of running the tests.",
    )
    parser.add_argument(
        "--output",
        dest="output",
        metavar="FILE",
        default=None,
        help="Write --view output here, defaults to the input with an html extension.",
    )
    return parser


def main() -> int:
    args = argparse_create().parse_args()

    if args.view is not None:
        if not os.path.exists(args.view):
            sys.stderr.write("File not found: %s\n" % args.view)
            return 1
        file_out = args.output or (os.path.splitext(args.view)[0] + ".html")
        # Viewing a HTML file writes the export over its own input.
        if os.path.abspath(file_out) == os.path.abspath(args.view):
            sys.stderr.write("Output would overwrite the input: %s\n" % file_out)
            return 1
        exit_code = run_hl_prog_extra_view(args.view, file_out)
        if exit_code == 0:
            sys.stdout.write("Written: %s\n" % file_out)
        return exit_code

    return run_hl_prog_extra_tests(args.selector)


if __name__ == "__main__":
    sys.exit(main())
