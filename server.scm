(import json
        uuid
        (chicken random)
        (chicken io)
        (chicken format)
        (chicken port)
        (chicken string))

; https://gist.github.com/dhess/52681
(define (json-read-fixup jro)
  (cond ((null? jro) jro)
        ((vector? jro) (json-read-fixup (vector->list jro)))
        ((pair? jro) (cons (json-read-fixup (car jro))
                           (json-read-fixup (cdr jro))))
        (else jro)))

(define (json-read-fixup2 jro)
  (cond ((null? jro) jro)
        ((vector? jro)
         (alist->hash-table (json-read-fixup2 (vector->list jro))))
        ((pair? jro)
         (cons (json-read-fixup2 (car jro))
               (json-read-fixup2 (cdr jro))))
        (else jro)))

(define logfile (open-output-file "/home/konst/code/scheme-lsp/log"))


(define (read-msg)
  (let*
    ((content-length (string->number (car (string-split (read-line) "Content-Length: "))))
     (req (read-string (+ 2 content-length))))
    (json-read-fixup (call-with-input-string req json-read))))

(define (serialize alist)
  (let*
    ((stapled (list->vector (append alist (list (cons "jsonrpc" "2.0")
                                                (cons "id" (uuid))))))
     (json-payload (call-with-output-string
                     (lambda (port)
                       (json-write stapled port))))
     (len (string-length json-payload)))
    (sprintf "Content-Length: ~A\r\n\r\n~A" len json-payload)))



(define (dispatch! msg)
  (display msg logfile))

(define (loop)
  (dispatch! (read-msg)))

(define initialize-struct (read-msg))

; do initialization here

(display (serialize (list (cons "method" "initialized"))))

(loop)
