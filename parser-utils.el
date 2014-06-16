
;; Utils

(defun ppar-get-local-definitions (setting)
  "Return the local value for SETTING.

SETTING is a list with the \"global/major-mode\" structure (see info ...)."
  (let ((r (--filter (keywordp (car it)) setting))
        (mm-setting (assq major-mode setting)))
    (when mm-setting
      ;; TODO/EASY: this does *not* remove the global values, merely
      ;; prepend the local ones, so assoc finds them first.  We should
      ;; probably delete the old ones to be sure
      (setq r (-concat (cdr mm-setting) r)))
    r))


;; Context tests
;; REVIEW/FROMSP: this function is copied from SP
(defun ppar-in-string? (&optional p)
  "Return non-nil if point is inside string or documentation string.

If optional argument P is present test this instead of point."
  (ignore-errors
    (save-excursion
      (nth 3 (syntax-ppss p)))))

;; REVIEW/FROMSP: this function is copied from SP
(defun ppar-in-comment? (&optional p)
  "Return non-nil if point is inside comment.

If optional argument P is present test this instead off point."
  (setq p (or p (point)))
  (ignore-errors
    (save-excursion
      (or (nth 4 (syntax-ppss p))
          ;; this also test opening and closing comment delimiters... we
          ;; need to chack that it is not newline, which is in "comment
          ;; ender" class in elisp-mode, but we just want it to be
          ;; treated as whitespace
          (and (< p (point-max))
               (memq (char-syntax (char-after p)) '(?< ?>))
               (not (eq (char-after p) ?\n)))))))

;; TODO: call some cache updater after each update
(defcustom ppar-context-types '((:string . ppar-in-string?)
                                (:comment . ppar-in-comment?))
  "List specifying context types and predicates testing these contexts.

A context type should be identified by a unique keyword (a symbol
satisfying `keywordp')

The predicate should take two *optional* arguments.  The first
argument specifies buffer point where the test should happen---if
none is specified, test at current point.  The second argument
specifies the direction in which the context is tested: non-nil
means backward.  It should return any non-nil value if the
current context is the one tested by the predicate.

NB: The test in \"backward\" direction is equivalent to moving
point one character backward.  However, we feel this interface
makes it more natural, considering the oddities of emacs's
display system (cursor is *always* between two positions, even if
it doesn't look so.)

The predicates should be independent of the order in which they
are run, and none should return non-nil in more than one context,
otherwise, the results are unspecified.

The entires are either cons pairs of the form

  (:context-type . predicate)

or a list whose `car' is major-mode and whose `cdr' is a list of
cons pairs described above.  The \"naked\" pairs are used as
global definitions.

Some functions return default context of :code is if no test
succeded, so this should not be specified here." )

;; TODO: add function to test for specific context
(defun ppar-get-context (&optional point back)
  "Get the context at point.

If optional argument POINT is non-nil, test context at that
buffer position.

If optional argument BACK is non-nil, return the context of the
point *before* the current position.

Contexts are tested according to the value of `ppar-context-types'.

If no predicate matched the current context, the context of :code
is returned."
  (save-excursion
    (when point (goto-char point))
    (when back (backward-char 1))
    (let ((lc (ppar-get-local-definitions ppar-context-types)))
      (--each-while lc (not (funcall (cdr it))) (!cdr lc))
      (or (caar lc) :code))))
