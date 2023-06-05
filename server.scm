(import json
        uuid
        (chicken io)
        (chicken port)
        (chicken string)
        requests
        responses)

; https://gist.github.com/dhess/52681
(define (json-read-fixup jro)
  (cond ((null? jro) jro)
        ((vector? jro) (json-read-fixup (vector->list jro)))
        ((pair? jro) (cons (json-read-fixup (car jro))
                           (json-read-fixup (cdr jro))))
        (else jro)))

(define in-file (open-output-file "/home/konst/code/scheme-lsp/in.txt"))
(define out-file (open-output-file "/home/konst/code/scheme-lsp/out.txt"))

(define (log-out val)
  (display val out-file)
  (newline out-file)
  (flush-output out-file))

(define (log-in val)
  (display val in-file)
  (newline in-file)
  (flush-output in-file))

(define (read-msg)
  (let*
    ((content-length (string->number (car (string-split (read-line) "Content-Length: "))))
     (req (read-string (+ 2 content-length))))
    (json-read-fixup (call-with-input-string req json-read))))


(define (display/flush msg)
  (display msg)
  (flush-output))

(define (retrieve key table)
  (alist-ref key table equal?))

(define (dispatch! msg)
  (let* ((id (retrieve "id" msg))
         (method (retrieve "method" msg))
         (params (retrieve "params" msg)))
        (log-in msg)
        (case (string->symbol method)
          ((initialize)
           (display/flush (initialize id params)))
          (else
            (log-out "unknown method")))))

(define (loop)
  (dispatch! (read-msg))
  (loop))

(loop)
