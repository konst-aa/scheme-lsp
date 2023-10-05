(import analysis
        (chicken base)
        (chicken string)
        (chicken io)
        (chicken port)
        (chicken format)
        cst
        srfi-1)

(display "hi")

(define test-text (with-input-from-file "test-file.scm" read-string))
(define cst (make-cst test-text display))
(define analysis-cst
  (first-pass cst "file:///home/konst/code/scheme-lsp/test.scm" display))
(analysis-cst)
