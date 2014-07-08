(require 'dash)

;; TODO: do we even need this? Maybe it's better to pass the pairs
;; list around as an argument.
;; TODO: add scope-limiters for pairs like //, **, "" etc
;; TODO: add prefix/suffix
;;       - syntax class
;;       - regexp
;;       - function
(defconst ppar-pairs ()
  "Pair definitions.

Note: this variable is unused and exists only for documentation
purposes.  A data structure of this format is passed as an
argument to all the parsing routines.

It is a list of plists with the structure described below.

Pair delimiters can be defined in two ways: with strings or
regular expressions (regular expressions and a PDA parser should
suffice in most of the common programming languages).

If the pair is defined using strings, these are interpreted
literally (= not as a regular expression) when matching against
them in the buffer.  The opening and closing pair can have no
mutual dependence.

If the pairs have mutual dependences or are more complex, a
\"regular expression parsing\" (regexp parsing) is supported.  In
this case, each delimiter is defined by three strings.

We will use the following terminology.  ``A replacement string''
is a string with the same format as the replacement in
`replace-regexp', that is, it can contain backrefenreces to
capture groups of previous search.

- opening regexp: a regular expression, possibly with capture
  groups, matching the opening delimiter

- forward opening delimiter: a replacement string matching the
  opening delimiter *after* the backreferences are resolved using
  the capture groups from the opening regexp.

- backward opening delimiter: a replacement string matching the
  opening delimiter *after* the backreferences are resolved using
  the capture groups from the closing regexp.

- closing regexp: a regular expression, possibly with capture
  groups, matching the closing delimiter

- forward closing delimiter: a replacement string matching the
  closing delimiter *after* the backreferences are resolved using
  the capture groups from the opening regexp.

- backward closing delimiter: a replacement string matching the
  closing delimiter *after* the backreferences are resolved using
  the capture groups from the closing regexp.

Here, the forward parsing system is explained, it works
analogically in the other direction.  First, we try to find a
match using the opening or closing regexp.

If a match was found for the opening regexp, the forward opening
and closing delimiters are evaluated as replacement strings,
replacing all the back-references with the capture groups in the
opening regexp.

If a match was found for the closing regexp, the backward opening
and closing delimiters are evaluated as replacement strings,
replacing all the back-references with the capture groups in the
closing regexp.

After this replacement, these are interpreted as *regular
expressions*.  We then use the forward (or backward) opening and
closing delimiters as regular expressions and try to find the
balanced expression.

Example.

In HTML, the definitions can look something like
this (warning, the backslashes are not escaped!):

- opening regexp: <\\(.*?\\)\\(?:\\s-.*?\\)?>
- forward opening delimiter: <\\1\\(?:\\s-.*?\\)?>
- backward opening delimiter: <\\1\\(?:\\s-.*?\\)?>
- closing regexp: </\\(.*?\\)>
- forward closing delimiter: </\\1>
- backward closing delimiter: </\\1>

Pair definition structure
=========================

Each pair has the following required attributes:

- open: Opening delimiter.  It can be either a string, a vector
  or a list with one or two elements.
  - If a string, it is interpreted literally.
  - If a vector, it can contain a string or a symbol which is
    interpreted as a word, that is, it can not be matched inside a
    symbol.
  - If a list, it should have three elements and these should be
    strings defining the opening regexp, forward opening delimiter and
    backward opening delimiter.


- close: Closing delimiter.  It is defined in the same way as
  the opening one.

Additionally, each pair has the following optional attributes:

- skip-match-function: In case the pair is not simply PDA
  parsable, you can specify a predicate which is called on every
  match and decides if it should be considered or not.  The
  predicate takes three arguments: the current match, the
  beginning of the match and the end of the match.  We only allow
  one predicate instead of a list to keep things simple, so if
  you want to test for more properties, `or' them into a single
  predicate.")

;; TODO: this will be removed in the future
(setq ppar-pairs '((:open "(" :close ")")
                   (:open "[" :close ")")
                   (:open "(" :close "]")
                   (:open ("<\\(.*?\\)\\(?:\\s-.*?\\)?>" "<\\1\\(\\s-.*?\\)?>" "<\\1\\(\\s-.*?\\)?>")
                    :close ("</\\(.*?\\)>" "</\\1>" "</\\1>"))
                   (:open [begin] :close [end])))

;; the regexp matchers will be simply stringed one after another.  To
;; get the correct match groups, we will remove all the nils from the
;; `match-data', those correspond to the non-matched previous
;; alternatives.

(defun ppar--wrap-noncapture (string)
  "Wrap STRING with non-capturing regexp group."
  (concat "\\(?:" string "\\)"))

(defun ppar--get-word-delim (delim)
  "Return a string describing word delimiter DELIM."
  (let ((x (elt delim 0)))
    (if (symbolp x) (symbol-name x) x)))

(defun ppar--get-delim (pair which)
  "Return the string representation of PAIR's delimiter.

WHICH is either :open or :close."
  (let ((delim (plist-get pair which)))
    (cond
     ((vectorp delim) (ppar--get-word-delim delim))
     ((listp delim) (car delim))
     (t delim))))

(defun ppar-string-match-entire-p (regexp string)
  "Return non-nil if REGEXP matches STRING entirely.

This works like `string-match-p' but wrapped with \\` and \\'."
  (string-match-p (concat "\\`" regexp "\\'") string))

;; N.B.: the decision whether we want to scan a pair or not should be
;; made on the client level by supplying appropriately filtered PAIRS
(defun ppar--get-matcher (pairs &optional what)
  "Return a regular expression matching all the delimiters of PAIRS

PAIRS is a list of pairs in the format of `ppar-pairs' we want to
scan.

If the optional argument WHAT is non-nil, it should be a
keyword :open or :close.  If :open, return a regexp only matching
opening delimiters.  If :close, return a regexp only matching
closing delimiters."
  (let* ((delims (cond
                  ((eq what :open) (--map (plist-get it :open) pairs))
                  ((eq what :close) (--map (plist-get it :close) pairs))
                  (t (-concat
                      (--map (plist-get it :open) pairs)
                      (--map (plist-get it :close) pairs)))))
         (delim-groups (--group-by
                        (cond
                         ((vectorp it) :word)
                         ((listp it) :regexp)
                         (t :punct))
                        delims))
         (punct (cdr (assoc :punct delim-groups)))
         (word (-map 'ppar--get-word-delim (cdr (assoc :word delim-groups))))
         (regexp (cdr (assoc :regexp delim-groups)))
         (delim-types (list
                       :punct (and punct (regexp-opt punct))
                       :word (and word (regexp-opt word 'words))
                       :regexp (and regexp (mapconcat
                                            (lambda (it) (ppar--wrap-noncapture (car it)))
                                            regexp
                                            "\\|"))
                       :punct-group punct
                       :word-group word
                       :regexp-group regexp)))
    delim-types))

(defun ppar-search-forward (pairs &optional matcher)
  "Search forward for a delimiter matching some pair of PAIRS.

If the optional argument MATCHER is non-nil, it should be a
matcher object for PAIRS as returned by `ppar--get-matcher'.
This argument exists for performance reasons."
  (setq matcher (or matcher (ppar--get-matcher pairs)))
  (let* ((punct (plist-get matcher :punct))
         (word (plist-get matcher :word))
         (regexp (plist-get matcher :regexp))
         (needles (list punct word regexp))
         (matches (-remove 'not
                           (--map (save-excursion
                                    (when (re-search-forward it nil t)
                                      (match-data)))
                                  (-remove 'not needles))))
         (mdata (when matches (--min-by (> (car it) (car other)) matches))))
    (when mdata
      (set-match-data mdata)
      (goto-char (match-end 0))
      mdata)))

(defun ppar-search-backward (pairs &optional matcher)
  "Search backward for a delimiter matching some pair of PAIRS.

If the optional argument MATCHER is non-nil, it should be a
matcher object for PAIRS as returned by `ppar--get-matcher'.
This argument exists for performance reasons."
  (setq matcher (or matcher (ppar--get-matcher pairs)))
  (let* ((punct (plist-get matcher :punct))
         (word (plist-get matcher :word))
         (regexp (plist-get matcher :regexp))
         (matches (-remove 'not
                           (list
                            (and punct (let ((limit (-max (-map 'length (plist-get matcher :punct-group)))))
                                         (save-excursion
                                           (when (ppar-search-backward-regexp punct limit)
                                             (match-data)))))
                            (and word (let ((limit (-max (-map 'length (plist-get matcher :word-group)))))
                                        (save-excursion
                                          (when (ppar-search-backward-regexp word limit)
                                            (match-data)))))
                            (and regexp (save-excursion
                                          (when (re-search-backward regexp nil t)
                                            (match-data)))))))
         (mdata (when matches (--max-by (> (cadr it) (cadr other)) matches))))
    (when mdata
      (set-match-data mdata)
      (goto-char (match-beginning 0))
      mdata)))

(defun ppar--match-pair (pair match)
  "Return non-nil if PAIR's opening or closing delimiter matches MATCH."
  (let ((op (plist-get pair :open))
        (cl (plist-get pair :close)))
    (cond
     ((vectorp op)
      (or (ppar-string-match-entire-p (regexp-quote (ppar--get-word-delim op)) match)
          (ppar-string-match-entire-p (regexp-quote (ppar--get-word-delim cl)) match)))
     ((listp op)
      (or (ppar-string-match-entire-p (car op) match)
          (ppar-string-match-entire-p (car cl) match)))
     (t
      (or (ppar-string-match-entire-p (regexp-quote op) match)
          (ppar-string-match-entire-p (regexp-quote cl) match))))))
(defun ppar--skip-to-first-pair (pairs &optional back)
  "Skip to the first pair.

Return a cons whose car is a list of opening delimiters and whose
cdr is a list of closing delimiters which contribute to the
\"depth\" of the found pair."
  (let ((cinfo (ppar-get-context-info))
        (matcher (ppar--get-matcher pairs)))
    ;; If we are in a context that is not :code, we will only search
    ;; for a pair inside its boundaries.  Otherwise, we ignore all
    ;; other contexts.
    (let ((match (if cinfo
                     (progn
                       (ppar-cinfo-get cinfo
                         (narrow-to-region :beg :end))
                       (when (re-search-forward matcher nil t)
                         (match-string 0)))
                   (let (m)
                     (while (and (setq m (re-search-forward matcher nil t))
                                 (not (eq (ppar-get-context back) :code))))
                     (when m
                       (match-string 0))))))
      ;; work out the delimiter groups here based on the match
      match)))

;; 1. get the context
;; 2. find the closed opening or closing pair in this context
;; 3. get the pair
(defun ppar-get-pair (pairs &optional back)
  "Get first paired expression after the point.

If the optional argument BACK is non-nil, search backwards
instead."
  (let* ((pairs (ppar--skip-to-first-pair))
         (opening (car pairs))
         (closing (cdr pairs)))

    ))
