(module utils
  (serialize read-msg retrieve display/flush range vector-dict
             make-logger! make-mailbox make-and-start-thread! thread-receive thread-send
             parse-params
             make-filled-mutex)

  (import scheme
          json
          srfi-69
          srfi-133
          srfi-18
          list-utils
          (chicken io)
          (chicken base)
          (chicken string)
          (chicken port)
          (chicken format))

  (define (make-logger! prefix)
    (define log-file (open-output-file (string-append "/home/konst/code/scheme-lsp/" prefix ".txt")))
    (lambda (val)
      (display val log-file)
      (newline log-file)
      (flush-output log-file)))

  ; https://gist.github.com/dhess/52681
  (define (json-read-fixup jro)
    (cond ((null? jro) jro)
          ((vector? jro) (json-read-fixup (vector->list jro)))
          ((pair? jro) (cons (json-read-fixup (car jro))
                             (json-read-fixup (cdr jro))))
          (else jro)))


  (define (parse-params msg)
    (let ((id (retrieve "id" msg 0))
          (method (string->symbol (retrieve "method" msg "fail here")))
          (params (retrieve "params" msg '())))
      (values id method params)))

  ; (define (json-read-fixup jro)
  ;   (cond ((null? jro) jro)
  ;         ((vector? jro)
  ;          (alist->hash-table (json-read-fixup (vector->list jro))))
  ;         ((pair? jro)
  ;          (cons (json-read-fixup (car jro))
  ;                (json-read-fixup (cdr jro))))
  ;         (else jro)))

  (define (make-mailbox thunk name)
    (let* ((mailbox (make-mutex))
           (asdf (mutex-specific-set! mailbox '()))
           (thread (make-thread thunk name)))
      (thread-specific-set! thread mailbox)
      thread))

  (define (make-and-start-thread! thunk name)
    (let ((thread (make-thread thunk name)))
      (thread-start! thread)
      thread))

  (define (thread-receive)
    (let* ((m (thread-specific (current-thread)))
           (locked (mutex-lock! m))
           (mailbox (mutex-specific m)))
      (if (null? mailbox)
        (begin (mutex-unlock! m)
               (thread-receive))
        (let ((msg (car mailbox)))
          (mutex-specific-set! m (cdr mailbox))
          (mutex-unlock! m)
          msg))))

  (define (thread-send thread message)
    (let* ((m (thread-specific thread))
           (locked (mutex-lock! m))
           (mailbox (mutex-specific m)))
      (mutex-specific-set! m (append mailbox (list message)))
      (mutex-unlock! m)))

  (define (make-filled-mutex contents name)
    (let ((m (make-mutex name)))
      (mutex-specific-set! m contents)
      m))

  (define (display/flush msg)
    (display msg)
    (flush-output))

  (define (retrieve key table default)
    (cond
      ((alist? table)
       (alist-ref key table equal? default))
      ((hash-table? table)
       (hash-table-ref key table))
      (else (error "retrieve: not a table"))))

  (define (serialize vec)
    (let* ((json-payload (call-with-output-string (lambda (port) (json-write vec port))))
           (len (string-length json-payload)))
      (sprintf "Content-Length: ~A\r\n\r\n~A" len json-payload)))

  (define (range start end)
    (vector-dict 'start (vector-dict 'line (car start)
                                     'character (cdr start))
                 'end (vector-dict 'line (car end)
                                   'character (cdr end))))

  (define (read-msg logger)
    (let* ((header (read-line))
           (content-length (string->number (car (string-split header "Content-Length: "))))
           (req (read-string (+ 2 content-length))))
      (logger (json-read-fixup (call-with-input-string req json-read)))
      (json-read-fixup (call-with-input-string req json-read))))


  (define (vector-dict . args)
    (apply vector
           (map (lambda (v) (cons (car v) (cadr v)))
                (section args 2 2))))
  )

