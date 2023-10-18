(import analysis
        (chicken base)
        (chicken string)
        (chicken io)
        (chicken port)
        (chicken format)
        cst
        srfi-1
        srfi-69)


(define test-text (with-input-from-file "test-file.scm" read-string))
(define cst (make-cst test-text display))
(define analysis-cst
  (first-pass cst "file:///home/konst/code/scheme-lsp/test.scm" display))
(define second-pass 
  (second-pass analysis-cst (make-hash-table) "file:///home/konst/code/scheme-lsp/test.scm" display))
(display (hash-table->alist (cdr second-pass)))
(display (cadddr (reverse (car second-pass))))
