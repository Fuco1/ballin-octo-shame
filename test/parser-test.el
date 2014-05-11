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
    ))

(ert-deftest ppar-test-parens-forward ()
  (ppar-test-pair (("(" ")"))))

(ert-deftest ppar-test-parens-backward ()
  (ppar-test-pair (("(" ")"))))



;; "foo XX(Xbar (baz (quux))X)XX baz (bar)"
;; "foo (bar (baz (quux)))F baz XX(XbarX)XX"
