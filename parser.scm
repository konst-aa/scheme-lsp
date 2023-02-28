(import (chicken process-context)
        (chicken port)
        (chicken io)
        (json)
        srfi-1
        args)

(define opts
  (list (args:make-option (h help) #:none "Display this text" (usage))))

(define (usage)
  (with-output-to-port (current-error-port)
                       (lambda ()
                         (print "Usage: " (car (argv)) " [options...] [files...]")
                         (newline)
                         (print (args:usage opts))))
  (exit 1))

(args:parse (command-line-arguments) opts)

(define (contains val alist)
  (any (lambda (t) (equal? t val)) alist))

(define whitespace (string->list " \n"))

(define (next-row loc)
  (list (+ 1 (car loc))
        0))

(define (inc-col loc)
  (list (car loc)
        (+ 1 (cadr loc))))

(define (make-value start end type value)
  (list (cons "start" start)
        (cons "end" end)
        (list "type" type)
        (list "value" value)))

(define (until-non-whitespace text loc)
  (cond
    ((equal? text '()) (values text loc)) ; this should return a special value
    ((contains (car text) whitespace)
     (until-non-whitespace
       (cdr text)
       (if (equal? (car text) #\newline)
         (next-row loc)
         (inc-col loc))))
    (else (values text loc))))

(define (parse-number text loc)
  (define stops (cons #\) whitespace))
  (define (helper text loc acc)
    (cond
      ((equal? text '()) (values text loc acc))
      ((contains (car text) stops) (values text loc acc))
      (else (helper (cdr text)
                    (inc-col loc)
                    (cons (car text) acc)))))
  (let*-values
    (((negative) (equal? (car text) #\-))
     ((text end-loc res) (helper (if negative (car text) text)
                                 (if negative (inc-col loc) loc)
                                 '()))
     ((abs-number) (string->number (list->string (reverse res))))
     ((number) (if negative (* -1 abs-number) abs-number)))
    (values text end-loc (make-value loc end-loc "number" number))))

(define (parse-list text loc)
  (define (helper text loc acc)
    (let-values
      (((text loc) (until-non-whitespace text loc)))
      (cond
        ((equal? text '()) (values text loc acc))
        ((equal? (car text) #\)) (values text loc acc))
        (else
          (let-values (((text loc res) (parse-datum text loc)))
                      (helper text loc (cons res acc)))))))
  (let-values
    (((text end-loc res) (helper (cdr text) (inc-col loc) '())))
    (values text end-loc (make-value loc end-loc "list" (reverse res)))))

(define (parse-datum text loc)
  (let*-values
    (((text loc) (until-non-whitespace text loc)))
    (cond
      ((equal? text '()) (cons text "aaaaa"))
      ((equal? (car text) #\() (parse-list text loc))
      ((contains (car text) (string->list "1234567890-"))
       (parse-number text loc)))))

(define in (open-input-file "test.scm"))

(define-values (text end-loc res) (parse-datum (string->list (read-string #f in)) (list 0 0)))

(display (alist-ref "start" res equal?))

(json-write res)
