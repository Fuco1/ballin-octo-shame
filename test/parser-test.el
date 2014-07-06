(require 'ert)
(require 'dash)

(defun ppar-test-extract-pair (example)
  (let* ((case-fold-search nil)
         (example (replace-regexp-in-string "[FB]" "" example)))
    (with-temp-buffer
      (insert example)
      (goto-char (point-min))
      ;; subtract one more because strings are indexed from zero
      (let ((prefix (- (re-search-forward "X" nil t) 2))
            (beg (- (re-search-forward "X" nil t) 3))
            (begi (- (re-search-forward "X" nil t) 4))
            (endi (- (re-search-forward "X" nil t) 5))
            (end (- (re-search-forward "X" nil t) 6))
            (suffix (- (re-search-forward "X" nil t) 7))
            (example (replace-regexp-in-string "X" "" example)))
        ;; and here we add one again because buffers are indexed from one
        ;; fucking emacs!!!
        (list :beg (1+ beg)
              :end (1+ end)
              :op (substring example beg begi)
              :cl (substring example endi end)
              :prefix (substring example prefix beg)
              :suffix (substring example end suffix))))))

(defun ppar-get-pair (&optional back)
  (sp-get-paired-expression back))

(defmacro ppar-test-pair (pair-defs setup &rest examples)
  (declare (indent 1))
  `(let ((ppar-pair-list ',pair-defs))
     (cl-dolist (example ',examples)
       (with-temp-buffer
         (fundamental-mode)
         ,setup
         (let ((pair-data (ppar-test-extract-pair example))
               (case-fold-search nil))
           (insert example)
           (goto-char (point-min))
           (replace-regexp "X" "")
           (goto-char (point-min))
           (cond
            ((re-search-forward "F" nil t)
             (delete-char -1)
             (should (equal (ppar-get-pair) pair-data)))
            ((re-search-forward "B" nil t)
             (delete-char -1)
             (should (equal (ppar-get-pair t) pair-data)))
            (t
             (goto-char (point-min))
             (should (equal (ppar-get-pair) pair-data))
             (goto-char (point-max))
             (should (equal (ppar-get-pair t) pair-data)))))))))

;; X's mark successively:
;; - start of the prefix
;; - outer start of pair
;; - inner start of pair
;; - inner end of pair
;; - outer end of pair
;; - end of the suffix.

;; - F marks the spot where point should be for forward test, B for
;; backward, otherwise it goes to (point-min) or (point-max)
;; - A marks the place where point *should be* after the function is
;; called

;; TODO: somehow add more settings, like major-mode etc
(ert-deftest ppar-test-parens-bidir ()
  (ppar-test-pair (("(" ")"))
    nil
    "XX(XfooX)XX"
    "foo XX(XfooX)XX"
    "XX(XfooX)XX bar"

    "foo XX(Xbar (baz (quux))X)XX baz"
    "foo XX(Xbar ((baz) quux)X)XX baz"
    "foo XX(X((bar) baz) quuxX)XX baz"
    ))

(ert-deftest ppar-test-parens-bidir-elisp ()
  (ppar-test-pair (("(" ")"))
    (emacs-lisp-mode)
    "X'X(Xfoo bar bazX)XX"
    "X`X(Xfoo bar bazX)XX"
    "X,X(Xfoo bar bazX)XX"
    "X,@X(Xfoo bar bazX)XX"

    "X'X(XfooX)XX"
    "foo X'X(XfooX)XX"
    "X'X(XfooX)XX bar"

    "foo X'X(Xfoo bar bazX)XX xuq"
    "foo X,@X(Xfoo bar bazX)XX qux"))

(ert-deftest ppar-test-parens-forward ()
  (ppar-test-pair (("(" ")"))
    nil
    "foo (bar (baz (quux))) Fbaz XX(XbarX)XX"
    ))

(ert-deftest ppar-test-parens-forward-comment ()
  (ppar-test-pair (("(" ")"))
    (emacs-lisp-mode)
    ;; make sure the comments at the end are fine even if touching
    "XX(Xfoo (bar) (baz) ((quux) (quo)) FquaX)XX;;asdasdasd"
    "XX(Xfoo (bar) (baz) ((quux) (quo)) FquaX)XX ;;asdasdasd"))

(ert-deftest ppar-test-parens-forward-elisp ()
  (ppar-test-pair (("(" ")"))
    (emacs-lisp-mode)
    "foo (bar Fbaz X,@X(Xxuuq fluxX)XX asd asd) asdas"
    "foo '(bar Fbaz X'X(Xxuuq fluxX)XX asd asd) asdas"))


(ert-deftest ppar-test-parens-backward ()
  (ppar-test-pair (("(" ")"))
    nil
    "foo XX(Xbar (baz (quux))X)XX bazB (bar)"
    ))

(ert-deftest ppar-test-parens-backward-comment ()
  (ppar-test-pair (("(" ")"))
    (emacs-lisp-mode)
    ;; make sure the comments at the end are fine even if touching
    "XX(XfooB (bar) (baz) ((quux) (quo)) quaX)XX;;asdasdasd"
    "XX(XfooB (bar) (baz) ((quux) (quo)) quaX)XX ;;asdasdasd"
    "XX(Xfoo (bar) (baz) ((quux) (quo)) quaX)XXB;;asdasdasd"
    "XX(Xfoo (bar) (baz) ((quux) (quo)) quaX)XX B;;asdasdasd"
    "XX(Xfoo (bar) (baz) ((quux) (quo)) quaX)XXB ;;asdasdasd"))

(ert-deftest ppar-test-parens-backward-elisp ()
  (ppar-test-pair (("(" ")"))
    (emacs-lisp-mode)
    "foo (bar baz X,@X(Xxuuq fluxX)XX asd Basd) asdas"
    "foo '(bar baz X'X(Xxuuq fluxX)XX asd Basd) asdas"))

(provide 'parser-test)

;; Local Variables:
;; eval: (push (file-name-directory (buffer-file-name)) load-path)
;; End:
