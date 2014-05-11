(require 'dash)

(defun ppar-test-command-setup ()
  t)

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

(defun ppar-get-pair (example)
  (ppar-test-extract-pair example))

(defmacro ppar-test-pair (pair-defs &rest examples)
  (declare (indent 1))
  `(let ((ppar-pair-list ',pair-defs)
         (case-fold-search nil))
     (cl-dolist (example ',examples)
       (with-temp-buffer
         (let ((pair-data (ppar-test-extract-pair example)))
           (sp-test-command-setup)
           (insert example)
           (goto-char (point-min))
           (cond
            ((re-search-forward "F" nil t))
            ((re-search-forward "B" nil t))
            (t
             (replace-regexp "X" "")
             (goto-char (point-min))
             (should (equal (ppar-get-pair) pair-data))
             (goto-char (point-max))
             (should (equal (ppar-get-pair t) pair-data)))))))))
