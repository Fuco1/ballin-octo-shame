;;; pair-parser.el --- New pair parser to replace the one in smartparens

;; Copyright (C) 2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: matching, languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; New pair parser to replace the one in smartparens.

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'dash-pl)

(font-lock-add-keywords 'emacs-lisp-mode `((,(concat "("
                                                     (regexp-opt '("ppar-get") t)
                                                     "\\>")
                                            (1 font-lock-keyword-face))))

(cl-eval-when (compile eval load)
  (defun ppar--get-substitute (struct list)
    "Only ever call this from `ppar-get'!  This function do the
replacement of all the keywords with actual calls to `ppar-get'."
    (if (listp list)
        (mapcar (lambda (x) (sp--get-substitute struct x)) list)
      (if (keywordp list)
          (ppar--get-replace-keyword struct list)
        list)))

  (defun ppar--get-replace-keyword (struct keyword)
    (cl-case keyword
      (:beg `(plist-get ,struct :beg))
      (:end `(plist-get ,struct :end))
      (:match `(plist-get ,struct :match))
      (t keyword))))

(defmacro ppar-get (struct &rest forms)
  "Get a property from a structure."
  (declare (indent 1)
           (debug (form body)))
  (let ((st (make-symbol "struct")))
    (ppar--get-substitute st `(let ((,st ,struct)) ,@forms))))

;; TODO: add the various skips and checks here.
;; TODO: keep track of context here?
(defun ppar-re-search (regexp &optional backward bound)
  "Search for REGEXP.

If BACKWARD is non-nil, search backward.

If BOUND is non-nil, it should be a number (position) or marker
behind which the search should not extend."
  (re-search-forward regexp bound t (if backward -1 1)))

(defun ppar-get-all-matches (pair-set &optional backward bound)
  "Get all matches of patterns from PAIR-SET."
  (--keep
   (save-excursion
     (when (ppar-re-search it backward bound)
       (list :beg (match-beginning 0)
             :end (match-end 0)
             :match (match-string 0))))
   pair-set))

(defun ppar-get-match (pair-set &optional backward bound)
  "Get the closest match of pattern from PAIR-SET to point."
  (-when-let (re (ppar-get-all-matches pair-set backward bound))
    (-min-by (-on '> (-cut -pl-get <> :beg)) re)))

(defun ppar-goto-after-match (pair-set &optional backward bound)
  (--when-let (ppar-get-match pair-set backward bound)
    (goto-char (ppar-get it (if backward :beg :end)))))

(defun ppar-goto-before-match (pair-set &optional backward bound)
  (--when-let (ppar-get-match pair-set backward bound)
    (goto-char (ppar-get it (if backward :end :beg)))))

(defun ppar-get-op-limit )

;; examples:
;; ruby: (ppar-parse-pair-forward '("module" "class" "def" "begin" "if" "do" "while") '("end"))
;; lisp: (ppar-parse-pair-forward '("(") '(")"))

;; search the first open and close pair, then search for open/close
;; inbetween, adjusting depth.  The choice is made by the comparison
;; of open/close distance from the point (e.g. if open > close, there
;; can only be close delimiters between point and open)

;; TODO: open-set and close-set will need to be more elaborate DS with
;; predicates for skipping
;; TODO: write a backward version
(defun ppar-parse-pair-forward (open-set close-set)
  (-when-let* ((op (ppar-get-match open-set))
               (cl (ppar-get-match close-set)))
    (let ((depth 0) (beg (ppar-get op :beg)) re)
      (while (and (>= depth 0))
        (cond
         ((and op (not cl))
          (setq re (> depth 0))
          (setq depth -1))
         ((and (not op) cl)
          (goto-char (ppar-get cl :end))
          (cl-decf depth)
          (while (and (> depth 0) (ppar-goto-after-match close-set))
            (cl-decf depth))
          (setq re (= depth 0))
          (setq depth -1))
         ((> (ppar-get op :beg) (ppar-get cl :beg))
          (goto-char (ppar-get cl :end))
          (cl-decf depth)
          (while (and (> depth 0)
                      (ppar-goto-after-match close-set nil (ppar-get op :beg)))
            (cl-decf depth))
          (if (= depth 0)
              (progn
                (setq re t)
                (setq depth -1))
            (goto-char (ppar-get op :end))
            (cl-incf depth)
            (setq cl (ppar-get-match close-set))
            (setq op (ppar-get-match open-set))))
         (t ;; op <= cl
          (goto-char (ppar-get op :end))
          (cl-incf depth)
          (while (and (> depth 0)
                      (ppar-goto-after-match open-set nil (ppar-get cl :beg)))
            (cl-incf depth))
          (goto-char (ppar-get cl :end))
          (cl-decf depth)
          (if (= depth 0)
              (progn
                (setq re t)
                (setq depth -1))
            (setq cl (ppar-get-match close-set))
            (setq op (ppar-get-match open-set))))))
      (when re (list beg (point))))))

(provide 'pair-parser)
;;; pair-parser.el ends here
