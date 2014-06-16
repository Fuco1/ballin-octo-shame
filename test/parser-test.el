(require 'ert)
(require 'dash)

(require 'test-helper)

;; X's mark successively:
;; - start of the prefix
;; - outer start of pair
;; - inner start of pair
;; - inner end of pair
;; - outer end of pair
;; - end of the suffix.

;; F marks the spot where point should be for forward test, B for
;; backward, otherwise it goes to (point-min) or (point-max)
;; TODO: somehow add more settings, like major-mode etc
(ert-deftest ppar-test-parens-bidir ()
  (ppar-test-pair (("(" ")"))
    "XX(XfooX)XX"
    "foo XX(XfooX)XX"
    "XX(XfooX)XX bar"

    "X'X(XfooX)XX"
    "foo X'X(XfooX)XX"
    "X'X(XfooX)XX bar"

    "foo XX(Xbar (baz (quux))X)XX baz"
    "foo XX(Xbar ((baz) quux)X)XX baz"
    "foo XX(X((bar) baz) quuxX)XX baz"

    ;; make sure the comments at the end are fine even if touching
    "XX(Xfoo (bar) (baz) ((quux) (quo)) quaX)XX;;asdasdasd"
    "XX(Xfoo (bar) (baz) ((quux) (quo)) quaX)XX ;;asdasdasd"
    ))

(ert-deftest ppar-test-parens-forward ()
  (ppar-test-pair (("(" ")"))
    ;; make sure the comments at the end are fine even if touching
    "XX(Xfoo (bar) (baz) ((quux) (quo)) FquaX)XX;;asdasdasd"
    "XX(Xfoo (bar) (baz) ((quux) (quo)) FquaX)XX ;;asdasdasd"
    ))

(ert-deftest ppar-test-parens-backward ()
  (ppar-test-pair (("(" ")"))
    ;; make sure the comments at the end are fine even if touching
    "XX(XfooB (bar) (baz) ((quux) (quo)) quaX)XX;;asdasdasd"
    "XX(XfooB (bar) (baz) ((quux) (quo)) quaX)XX ;;asdasdasd"
    ))



;; "foo XX(Xbar (baz (quux))X)XX baz (bar)"
;; "foo (bar (baz (quux)))F baz XX(XbarX)XX"


;; Local Variables:
;; eval: (push (file-name-directory (buffer-file-name)) load-path)
;; End:
