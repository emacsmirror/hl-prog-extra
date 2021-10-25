############################
Highlight Programming Extras
############################

This package supports highlighting regular expressions into a setting that can be adjusted.

Where highlighting may be set local to a buffer, directory ... project etc.

Highlighting definitions is applied separately to comments, strings or other code.

While this can be used to highlight arbitrary expressions, common use cases include:

- URL's and emails in comments.
- Issue tracker ticket numbers in comments.
- Literal text using quotes or back-ticks.
- Tags such as ``TODO``, ``FIXME`` etc.

Since projects have their own conventions for adding structured text in comments
this package aims to support customized highlighting in a few lines.


Motivation
==========

This package was written to support easily highlighting arbitrary terms in source code,
without the need to rely on multiple packages or writing custom highlighting code for each use.


Usage
=====

This package exposes the following functions.

- ``hl-prog-extra-mode`` (enable the buffer local mode).
- ``hl-prog-extra-refresh`` (run this so customization is updated while the mode is active).
- ``global-hl-prog-extra-mode`` (enable the mode globally).


Customization
-------------

``hl-prog-extra-list``
   The main variable used to set highlighting,
   this is a list of lists representing ``(regex regex-subexpr context face)``

   :regex:
      The regular expression to match.
   :regex-subexpr:
      Group to use when highlighting the expression (zero for the entire expression).
   :context:
      A symbol in: ``'comment``, ``'string`` or ``nil``
      This limits the highlighting to only these parts of the text,
      where ``nil`` is used for anything that doesn't match a comment or string.
   :face:
      The face to apply as a symbol, a string or a face property list, for example:
      ``'(:background "#666600" :foreground "#FFFFFF")``.

   This defaults to matching URL's and email addresses.

``hl-prog-extra-global-ignore-modes`` nil
   A list of modes that won't enable highlighting from ``global-hl-prog-extra-mode``.

``hl-prog-extra-global-ignore-buffer`` nil
   When not ``nil``, the buffer won't enable highlighting from ``global-hl-prog-extra-mode``.

   This may also be a function that takes a single buffer argument,
   where returning ``nil`` will enable highlighting anything else will not.


Example
-------

Without any configuration, enabling the mode globally will highlight URL's and email addresses within comments:

.. code-block:: elisp

   (use-package hl-prog-extra)
   (global-hl-prog-extra-mode)


This is a more involved example that defines it's own matches, only loading on some modes:

.. code-block:: elisp

   (use-package hl-prog-extra
     :commands (hl-prog-extra-mode)
     :config
     (setq hl-prog-extra-list
       (list
         ;; Match `some.text` as a constant.
         '("\\_<[^`\n]+\\_>" 0 comment font-lock-doc-face)
         ;; Match http://xyz (URL).
         '("\\<https?://[^[:blank:]]*" 0 comment font-lock-constant-face)
         ;; Match <email@name.foo> email address.
         '("<\\([[:alnum:]\\._-]+@[[:alnum:]\\._-]+\\)>" 1 comment font-lock-constant-face))))

   ;; Enable for modes.
   (add-hook 'python-mode-hook
     (lambda () (hl-prog-extra-mode))

   (add-hook 'sh-mode-hook
     (lambda () (hl-prog-extra-mode))


Details
=======

- Highlighting is performed in a single pass, to avoid the overhead of re-scanning the buffer for each search term.
- The use of ``comment`` and ``string`` contexts is a result of emacs internal syntactic analysis,
  which exposes this information before highlighting.


Limitations
===========

- Only a single face is supported per-match.
- Categories are limited to ``'comment``, ``'string`` and ``nil``.


Installation
============

The package is `available in melpa <https://melpa.org/#/hl-prog-extra>`__ as ``hl-prog-extra``.

This is currently installable via straight.

.. code-block:: elisp

   (use-package hl-prog-extra
     :commands (hl-prog-extra-mode))


Further Work
============

- Elements could optionally be made into links,
  allowing project specific but-tracker tickets to open URL's when clicked on for e.g.

- A predicate function could be (optionally) defined to perform additional checks before highlighting,
  this would allow checking additional context when considering matches.
