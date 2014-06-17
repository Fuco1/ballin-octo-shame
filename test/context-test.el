(require 'ert)
(require 'dash)

(defun ppar-test-extract-context (example)
  (let* ((case-fold-search nil)
         (example (replace-regexp-in-string "[FB]" "" example)))
    (with-temp-buffer
      (insert example)
      (goto-char (point-min))
      ;; subtract one more because strings are indexed from zero
      (let ((beg (- (re-search-forward "X" nil t) 2))
            (end (- (re-search-forward "X" nil t) 3))
            (example (replace-regexp-in-string "X" "" example)))
        ;; and here we add one again because buffers are indexed from one
        ;; fucking emacs!!!
        (list :beg (1+ beg)
              :end (1+ end))))))

(defmacro ppar-test-context-get-info (context setup &rest examples)
  (declare (indent 2))
  `(cl-dolist (example ',examples)
     (with-temp-buffer
       (fundamental-mode)
       ,setup
       (let ((context-data (-concat (list :id ,context)
                                    (ppar-test-extract-context example)))
             (case-fold-search nil))
         (insert example)
         (goto-char (point-min))
         (replace-regexp "X" "")
         (goto-char (point-min))
         (cond
          ((re-search-forward "F" nil t)
           (delete-char -1)
           (should (equal (ppar-get-context-info) context-data))))))))

(ert-deftest ppar-test-context-get-info-string ()
  (ppar-test-context-get-info :string
    (emacs-lisp-mode)
    "X\"foo F bar baz\"X"
    "asdasd X\"foo F bar baz\"X asdadasd"
    "asdasd X\"foo F bar baz\"X ;; asdasd
asdasd sd"
    "X\"foo F bar baz\"X;; asdasd"))

(ert-deftest ppar-test-context-get-info-comment ()
  (ppar-test-context-get-info :comment
    (emacs-lisp-mode)
    "asdasd X;; asd \"foo F bar baz\" asdadasdX"
    "asdasd X;; sdjfh sdfjhsdfFX
asdad asdkjads"
    ))

(defmacro ppar-test-context-id (context setup &rest examples)
  (declare (indent 2))
  `(cl-dolist (example ',examples)
     (with-temp-buffer
       (fundamental-mode)
       ,setup
       (let ((case-fold-search nil)
             (should-fail (listp example)))
         (insert (or (car-safe example) example))
         (goto-char (point-min))
         (cond
          ((re-search-forward "F" nil t)
           (delete-char -1)
           (if should-fail
               (should (not (equal (ppar-get-context) ,context)))
             (should (equal (ppar-get-context) ,context))))
          ((re-search-forward "B" nil t)
           (delete-char -1)
           (if should-fail
               (should (not (equal (ppar-get-context t) ,context)))
             (should (equal (ppar-get-context t) ,context)))))))))

(ert-deftest ppar-test-context-id-string ()
  (ppar-test-context-id :string
    (emacs-lisp-mode)
    "foo \"bar Fbaz\" qux"
    "\"bar Fbaz\" qux"
    "\"bar Fbaz\""

    "foo \"bar Fbaz ;; boo\" qux"
    "foo \"bar baz ;; boo F baz\" qux"

    "foo \"bar baz ;; boo\"B qux"
    ))

(ert-deftest ppar-test-context-id-code ()
  (ppar-test-context-id :code
    (emacs-lisp-mode)
    "Ffoo \"bar baz\" qux"
    "Ffoo \"bar baz\" Fqux"
    "F\"bar baz\" qux"
    "F\"bar baz\""
    "\"bar baz\"F"

    "foo \"bar baz ;; boo\" qFux"
    "fFoo \"bar baz ;; boo  baz\" qux"

    "foFo \"bar baz ;; boo\" qux"
    ))

(ert-deftest ppar-test-context-id-comment()
  (ppar-test-context-id :comment
    (emacs-lisp-mode)
    "foo bar ;; Fbaz qux"
    "foo bar F;; baz qux"
    ("foo bar B;; baz qux")
    "foo bar ;; baz Fqux
asdasd asda"
    "   ;; baz Fqux"
    ))

(provide 'context-test)
