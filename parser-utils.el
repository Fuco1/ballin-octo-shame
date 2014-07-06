(require 'dash)


;; Utils

;; TODO: it should also be possible to have a local value based on
;; pair, not only major-mode
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
(defun ppar-in-string? (&optional p)
  "Return non-nil if point is inside string or documentation string.

If optional argument P is present test this instead of point."
  (ignore-errors
    (save-excursion
      (nth 3 (syntax-ppss p)))))

(defun ppar-bounds-string ()
  "Return the bounds of the string around point."
  (let* ((parse-data (syntax-ppss))
         (open (nth 8 parse-data))
         (close (save-excursion
                  (parse-partial-sexp (point) (point-max) nil nil parse-data 'syntax-table)
                  (point))))
    (cons open close)))

(defun ppar-skip-to-string (&optional back)
  "Move point before the next string.

If optional argument BACK is non-nil, skip after the previous
string."
  (let ((syntax (syntax-ppss)))
    (if back
        (progn
          (while (and (not (bobp))
                      (not (nth 3 (setq syntax (syntax-ppss)))))
            (backward-char))
          (unless (bobp) (forward-char) syntax))
      (while (and (not (eobp))
                  (not (nth 3 syntax)))
        (setq syntax (parse-partial-sexp (point) (point-max) nil nil syntax 'syntax-table)))
      (unless (eobp) (backward-char) syntax))))

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

(defun ppar-bounds-comment ()
  "If the point is inside a comment, return its bounds."
  (when (or (sp-point-in-comment)
            (looking-at "[[:space:]]+\\s<"))
    (let ((open (save-excursion
                  (while (and (not (bobp))
                              (or (sp-point-in-comment)
                                  (save-excursion
                                    (backward-char 1)
                                    (looking-at "[[:space:]]+\\s<"))))
                    (backward-char 1))
                  (when (not (or (bobp)
                                 (or (sp-point-in-comment)
                                     (save-excursion
                                       (backward-char 1)
                                       (looking-at "[[:space:]]+\\s<")))))
                    (forward-char))
                  (point)))
          (close (save-excursion
                   (while (and (not (eobp))
                               (or (sp-point-in-comment)
                                   (looking-at "[[:space:]]+\\s<")))
                     (forward-char 1))
                   (when (not (or (eobp)
                                  (or (sp-point-in-comment)
                                      (looking-at "[[:space:]]+\\s<"))))
                     (backward-char 1))
                   (point))))
      (cons open close))))

(defun ppar-skip-to-comment (&optional back)
  "Move point before the next comment.

If optional argument BACK is non-nil, skip after the previous
comment.")

;; TODO: call some cache updater after each update
;; - add type and group
(defcustom ppar-context-types '((:string ppar-in-string?
                                 ppar-bounds-string ppar-skip-to-string)
                                (:comment ppar-in-comment?
                                 ppar-bounds-comment ppar-skip-to-string))
  "List specifying context types.

A context type should be identified by a unique keyword (a symbol
satisfying `keywordp').  Each context has associated a predicate,
a function that returns the bounds of the context and a function
that skips forward or backward all content that is not inside the
specified context.

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

The bounds function should take one *optional* argument.  If this
is specified, it is the point from which we look forward and
backward for the bounds of the context at point.  When not
specified, current point is used.

The return value is a cons pair (BEG . END), where BEG and END
are buffer positions representing the beginning and the end of
the context block respectivelly.  Calling the associate predicate
at either of these positions should return nil.

The skipper function should take one *optional* argument.  If
this is specified, the search goes backward from point, otherwise
forward.  After the function returns, the point should be before
the context block, such that calling the predicate would return
nil, but calling the predicate for the following position would
return non-nil.  Similarly for the backward search.

The entires are either lists of the form

  (:context-type predicate bounds skipper)

or a list whose `car' is major-mode and whose `cdr' is a list of
lists described above, these are then local definitions for the
major-mode.  The \"naked\" pairs are used as global definitions.

Notes: Some functions return default context of :code is if no
predicate succeded, so this should not be specified here." )

(defmacro ppar-context-get (data form)
  "Get meta-information about context.

DATA is the data item describing the context.  It should be an
element of `ppar-context-types'.

FORM can be a form of these types:

* :id - return the id of the context.
* :pred - return the predicate testing if point is at this context.
* :bounds - return the function returning bounds of current context.
* :skip - return the function skipping all the intermediate content."
  (cond
   ((eq form :id)
    `(caar ,data))
   ((eq form :pred)
    `(cadr ,data))
   ((eq form :bounds)
    `(nth 2 ,data))
   ((eq form :skip)
    `(nth 3 ,data))))

(defun ppar-get-context (&optional back point)
  "Get the context at point.

If optional argument BACK is non-nil, return the context of the
point *before* the current position.

If optional argument POINT is non-nil, test context at that
buffer position.

Contexts are tested according to the value of `ppar-context-types'.

If no predicate matched the current context, the context of :code
is returned."
  (save-excursion
    (when point (goto-char point))
    (when back (backward-char 1))
    (let ((lc (ppar-get-local-definitions ppar-context-types)))
      (--each-while lc (not (funcall (ppar-context-get it :pred))) (!cdr lc))
      (or (ppar-context-get lc :id) :code))))

(defun ppar-in-context? (context &optional point)
  "Return non-nil if point is in CONTEXT.

If optional argument POINT is non-nil, test context at that
buffer position."
  (-when-let (lc (assoc context (ppar-get-local-definitions ppar-context-types)))
    (funcall (ppar-context-get lc :pred) point)))

(defun ppar-get-context-info (&optional point)
  "Get the context info at POINT.

If optional argument POINT is non-nil, test context at that
buffer position.

Context info is a plist with these items:

* :type - type of the context
* :beg  - beginning of the context block
* :end  - end of the context block"
  (when point (goto-char point))
  (let ((context (ppar-get-context)))
    (-when-let (lc (assoc context (ppar-get-local-definitions ppar-context-types)))
      (let ((bounds (funcall (ppar-context-get lc :bounds))))
        (list :id context
              :beg (car bounds)
              :end (cdr bounds))))))

(defmacro ppar-cinfo-get (cinfo form)
  "Get information about context.

CINFO is the list describing the context.  It should be in format
format returned by `ppar-get-context-info'.

FORM can be a form of these types:

* :id - return the type of the context.
* :beg - return the beginning of the context block.
* :end - return the end of the context block."
  (cond
   ((eq form :id)
    `(plist-get ,cinfo :id))
   ((eq form :beg)
    `(plist-get ,cinfo :beg))
   ((eq form :end)
    `(plist-get ,cinfo :end))))
(defun ppar-skip-up-context (&optional back)
  "Skip out of the current context."
  (-when-let (cinfo (ppar-get-context-info))
    (goto-char
     (if back
         (ppar-cinfo-get cinfo :beg)
       (ppar-cinfo-get cinfo :end)))
    cinfo))

(defun ppar-skip-to-context (context &optional back)
  "Skip all content between point and start of the next block of CONTEXT.

If BACK is non-nil, skip backwards."
  (-when-let (lc (assoc context (ppar-get-local-definitions ppar-context-types)))
    (funcall (ppar-context-get lc :skip) back)))

(font-lock-add-keywords 'emacs-lisp-mode `((,(concat "("
                                                     (regexp-opt '("ppar-cinfo-get"
                                                                   ) t)
                                                     "\\>")
                                            (1 font-lock-keyword-face))))

;; TODO: bounding should be done with narrowing instead of passing an
;; argument, it is pretty fast and simplifies lots of things.
