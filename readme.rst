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

``hl-prog-extra-mode``
   Enable the buffer local mode.
``hl-prog-extra-refresh``
   Run this so customization is updated while the mode is active.
``hl-prog-extra-preset``
   Load a preset associated with the current major-mode (when available), see: the presets section below.
``global-hl-prog-extra-mode``
   Enable the mode globally.


Customization
-------------

``hl-prog-extra-list``
   The main variable used to set highlighting,
   this is a list of lists representing ``(regex regex-subexpr context face)``

   ``regex``
      The regular expression to match.
   ``regex-subexpr``
      Group to use when highlighting the expression (zero for the entire expression).

      May also be a list of groups, see the `Multi-Group Matching`_ example.
   ``context``
      A symbol (or list of symbols) in:

      .. list-table::
         :header-rows: 1

         - - Symbol
           - Description
         - - ``'comment``
           - All comments (both ``comment-only`` & ``comment-doc``).
         - - ``'comment-only``
           -  Only non-documentation comments.
         - - ``'comment-doc``
           - Only documentation comments.
         - - ``'string``
           - All strings (both ``string-only`` & ``string-doc``).
         - - ``'string-only``
           - Only non-documentation strings.
         - - ``'string-doc``
           - Documentation strings.
         - - ``nil``
           - Non comments or strings (other source-code).

      This limits the highlighting to only these parts of the text,
      where ``nil`` is used for anything that doesn't match a comment or string.

      A list of these symbols is also supported (allowing an entry to match multiple contexts).
   ``face``
      The face to apply as a symbol, a string or a face property list, for example:
      ``'(:background "#666600" :foreground "#FFFFFF")``.

      May also be a list of faces, see the `Multi-Group Matching`_ example.

   This defaults to matching URL's and email addresses.

``hl-prog-extra-preset`` nil
   When non-nil, include presets for the current major-mode (when available).

``hl-prog-extra-global-ignore-modes`` nil
   A list of modes that won't enable highlighting from ``global-hl-prog-extra-mode``.

``hl-prog-extra-global-ignore-buffer`` nil
   When not ``nil``, the buffer won't enable highlighting from ``global-hl-prog-extra-mode``.

   This may also be a function that takes a single buffer argument,
   where returning ``nil`` will enable highlighting anything else will not.


Presets
-------

Optionally you may use presets included with this package.
This is done to provide useful highlighting associated with specific languages.

If you wish to use presets, when the variable ``hl-prog-extra-preset`` is non-nil
they are included when available.

Otherwise you may call a function which returns presets that can be used to build ``hl-prog-extra-list`` yourself.

The following function returns a list of values associated with a major mode.

``hl-prog-extra-preset`` with arguments ``(major-mode quiet .. args)``

   Optional arguments:

   ``major-mode``
      The major mode to load a preset for (as a string), this defaults to the current major mode.
   ``quiet``
      No message when there is no preset for the ``major-mode``.

   ``args``
      The remaining arguments can be used to control the presets.
      Keyword value style arguments are to be used with common argument (for example) ``:no-string-escape``.

      See the preset function's doc-string for details.

Currently presets are included for:

- ``c-mode``
- ``lua-mode``
- ``python-mode``

Presets for other modes are welcome.


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
   (add-hook 'python-mode-hook #'hl-prog-extra-mode)
   (add-hook 'sh-mode-hook #'hl-prog-extra-mode)


Multi-Group Matching
^^^^^^^^^^^^^^^^^^^^

Withing a single expression match you may wish to assign multiple faces.

This is supported by using lists for ``regex-subexpr`` & ``face`` settings.

Both lists need to be the same length where each sub-expression matches the corresponding face.

This example configuration shows how multiple matches can be used with tags in comments such as
``NOTE(my name)`` can show text within the parenthesis with a different color:

.. code-block:: elisp

   (setq hl-prog-extra-list
     (list
       "\\<\\(NOTE\\)\\((\\([^)+]+\\))\\)?" '(0 3) 'comment
       (list
         '(:background "#006000" :foreground "#FFFFFF")
         '(:background "#006000" :foreground "#BBBBBB")))

     (list
       "\\<\\(TODO\\|WORKAROUND\\)\\((\\([^)+]+\\))\\)?" '(0 3) 'comment
       (list
         '(:background "#707000" :foreground "#FFFFFF")
         '(:background "#707000" :foreground "#BBBBBB")))

     (list
       "\\<\\(FIXME\\|XXX\\|WARNING\\)\\((\\([^)+]+\\))\\)?" '(0 3) 'comment
       (list
         '(:background "#800000" :foreground "#FFFFFF")
         '(:background "#800000" :foreground "#BBBBBB"))))


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
