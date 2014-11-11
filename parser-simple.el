;; different, hopefully more reasonable approach here

;; We always search in the current context as that is by far the most
;; common thing.  When anything else is required, we can manually jump
;; around contexts to achieve different behaviour.

(defun ppar-get-context (&optional point)
  "Get the context at point.

If optional argument POINT is non-nil, go to that buffer position
first."
  (save-match-data
    (save-excursion
      (when point (goto-char point))
      (cond
       ((sp-point-in-string) 'string)
       ((sp-point-in-comment) 'comment)
       ((and (eq major-mode 'org-mode) (eq (car (org-element-at-point)) 'src-block)) 'org-src-block)
       (t 'code)))))

;; TODO: optimize the bounds computation
(defun ppar-get-context-info (&optional point context)
  "Get the context info at point.

If optional argument POINT is non-nil, go to that buffer position
first.

If CONTEXT is specified, assume we are in the given contex."
  (save-match-data
    (save-excursion
      (when point (goto-char point))
      (let* ((context-type (or context (ppar-get-context)))
             (bounds (cl-case context-type
                       (string (let* ((syntax (syntax-ppss))
                                      (delim (char-to-string (nth 3 syntax)))
                                      (beg (nth 8 syntax))
                                      (not-closed nil)
                                      (end (progn
                                             (while (and
                                                     (setq closed (re-search-forward delim nil t))
                                                     (nth 3 (syntax-ppss))))
                                             (point))))
                                 (cons beg (or (and closed end) (point-max)))))
                       (comment (sp-get-comment-bounds))
                       (org-src-block (let ((org-elem (cdr (org-element-at-point))))
                                        (cons (plist-get org-elem :begin)
                                              (plist-get org-elem :end))))
                       (code (cons (point-min) (point-max))))))
        (list :type context-type :beg (car bounds) :end (cdr bounds))))))

;; TODO: add (predicate) to skip pair matches for each pair
(defun ppar-skip-to-paired-expression (pair-list &optional back)
  "Skip to the first paired expression defined by a pair in PAIR-LIST.

If BACK is non-nil, search backwards."
  (save-match-data
    (-let* ((needle (sp--strict-regexp-opt (-mapcat '-cons-to-list pair-list)))
            (context-info (ppar-get-context-info))
            ((&plist :type type :beg beg :end end) context-info)
            (p (point)))
      (while (and (re-search-forward needle end t)
                  (save-excursion
                    (goto-char (match-beginning 0))
                    (not (eq (ppar-get-context) type)))))
      ;; we have a match
      (when (/= p (point))
        (goto-char (match-beginning 0))
        context-info))))

(defun ppar-get-sexp (pair-list &optional back)
  (save-match-data
    (save-excursion
      (-when-let (context-info (ppar-skip-to-paired-expression pair-list back))
        ;; TODO: avoid computing needle twice
        (-let* ((needle (sp--strict-regexp-opt (-mapcat '-cons-to-list pair-list)))
                ((&plist :type type :beg beg :end end) context-info)
                ;; this must succeed, because context-info =/= null
                (op (progn
                      (re-search-forward needle)
                      (goto-char (match-beginning 0))
                      (match-string 0)))
                (pair (-first (-lambda ((o)) (equal op o)) pair-list))
                ((op . cl) pair))
          (cond
           ((and (= (length op) 1)
                 (= (char-syntax (aref op 0)) ?\"))
            (cond
             ((eq type 'string)
              (ppar-get-syntactic-string back))
             ;; decide here on some pair property if we want to call
             ;; strict or fast version
             (t (ppar-get-stringlike-weak (list pair) back))))
           ((equal op cl)
            ;; decide here on some pair property if we want to call
            ;; strict or fast version
            (ppar-get-stringlike-weak (list pair) back))
           (t (ppar-get-paired-expression (list pair) back))))))))

(defun ppar-get-paired-expression (pair-list &optional back)
  ;; ignores the pair-list for the mean time
  (sp-get-paired-expression back))

(defun ppar-get-syntactic-string (&optional back)
  (unless (eq (ppar-get-context) 'string) (if back (backward-char) (forward-char)))
  (-let (((&plist :beg beg :end end) (ppar-get-context-info))
         (delim (string (nth 3 (syntax-ppss)))))
    (list :ber beg :end end :op delim :cl delim :prefix "" :suffix "")))

(defun ppar-get-stringlike-strict (pair-list &optional back))

(defun ppar-get-stringlike-fast (pair-list &optional back))
