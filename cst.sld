(module cst
  (make-cst
    make-value
    value?
    value-type
    value-contents
    value-start
    value-end
    value-err?
    make-pos
    pos?
    pos-text
    pos-line
    pos-column
    parse-test
    value->range
    )

  (import scheme
          (chicken io)
          (chicken port)
          (chicken base)
          (chicken format)
          utils)

  ;; possible values:
  ;; - number
  ;; - string
  ;; - symbol
  ;; - boolean (not supported yet)
  ;; - list
  ;; - comment
  ;; - char (not supported yet)
  ;; - vector (not supported yet)

  (define-record-type value
    (make-value type contents start end err)
    value?
    (type value-type (setter value-type))
    (contents value-contents (setter value-contents))
    (start value-start (setter value-start))
    (end value-end (setter value-end))
    (err value-err?)
    )

  (define-record-printer (value v out)
    (fprintf out "#,(value type: ~s contents: ~s start: ~s end: ~s err: ~s)"
             (value-type v) (value-contents v) (value-start v) (value-end v) (value-err? v)))

  (define-record-type pos
    (make-pos text line column)
    pos?
    (text pos-text (setter pos-text))
    (line pos-line (setter pos-line))
    (column pos-column (setter pos-column))
    )

  (define-record-printer (pos p out)
    (fprintf out "#,(pos line: ~s column: ~s bound character: ~s)"
             (pos-line p) (pos-column p) (car (pos-text p))))

  (define (value->range v)
    (let ((start (value-start v))
          (end (value-end v)))
      ; ranges are exclusive
      (range (cons (pos-line start) (pos-column start)) 
             (cons (pos-line end) (+ (pos-column end) 1)))))

  (define (pos-increment pos)
    (let* ((text (pos-text pos))
           (line (pos-line pos))
           (column (pos-column pos))
           (c (car text)))
      (cond
        ((equal? c #\newline)
         (make-pos (cdr text) (+ line 1) 0))
        (else
          (make-pos (cdr text) line (+ column 1))))))

  (define (until-nonwhitespace pos)
    (let ((c (car (pos-text pos))))
      (if (contains '(#\space #\newline #\tab) c)
        (until-nonwhitespace (pos-increment pos))
        pos)))

  (define (parse-string start-pos)
    (define (loop acc pos)
      (let ((c (car (pos-text pos))))
        (cond
          ((equal? c #\")
           (values (make-value 'string (list->string (reverse acc)) start-pos pos #f) (pos-increment pos)))
          ((eof-object? c)
           (values (make-value 'string (list->string (reverse acc)) start-pos pos #t) pos))
          (else (loop (cons c acc) (pos-increment pos))))))
    (loop '() (pos-increment start-pos)))

  (define (parse-symbol start-pos)
    (define (rev-list->symbol lst)
      (string->symbol (list->string (reverse lst))))

    (define (loop acc pos)
      (let ((c (cadr (pos-text pos))))
        (cond
          ((contains '(#\space #\newline #\tab #\) #\() c)
           (values (make-value 'symbol (rev-list->symbol acc) start-pos pos #f) (pos-increment pos)))
          ((eof-object? c)
           (values (make-value 'string (rev-list->symbol acc) start-pos pos #f) (pos-increment pos)))
          (else (loop (cons c acc) (pos-increment pos))))))

    (loop (list (car (pos-text start-pos))) start-pos))

  (define (parse-list start-pos)
    (define (loop acc pos)
      (let* ((pos (until-nonwhitespace pos))
             (c (car (pos-text pos))))
        (cond
          ((equal? c #\))
           (values (make-value 'list (reverse acc) start-pos pos #f) (pos-increment pos)))
          ((eof-object? c)
           (values (make-value 'list (reverse acc) start-pos pos #t) pos))
          (else
            (let-values (((value new-pos) (parse-value pos)))
              (loop (cons value acc) new-pos))))))

    (loop '() (pos-increment start-pos)))

  (define (parse-number start-pos)
    (define (rev-list->number lst)
      (string->number (list->string (reverse lst))))

    (define (loop acc pos)
      (let ((c (cadr (pos-text pos))))
        (cond
          ((contains '(#\space #\newline #\tab #\) #\() c)
           (values (make-value 'number (rev-list->number acc) start-pos pos #f) (pos-increment pos)))
          ((eof-object? c)
           (values (make-value 'number (rev-list->number acc) start-pos pos #f) (pos-increment pos)))
          (else (loop (cons c acc) (pos-increment pos))))))

    (loop (list (car (pos-text start-pos))) start-pos))

  (define (parse-comment start-pos)

    (define (loop acc pos)
      (let ((c (car (pos-text pos))))
        (cond
          ((equal? c #\newline)
           (values (make-value 'comment (list->string (reverse acc)) start-pos pos #f) (pos-increment pos)))
          ((eof-object? c)
           (values (make-value 'comment (list->string (reverse acc)) start-pos pos #f) (pos-increment pos)))
          (else (loop (cons c acc) (pos-increment pos))))))

    (loop '() (pos-increment start-pos)))

  (define (parse-value pos)
    (let* ((new-pos (until-nonwhitespace pos))
           (c (car (pos-text new-pos))))
      (cond
        ((equal? c #\")
         (parse-string new-pos))
        ; ((equal? c "#")
        ;  (parse-boolean new-pos))
        ((equal? c #\()
         (parse-list new-pos))
        ((contains '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) c)
         (parse-number new-pos))
        ((equal? c #\;)
         (parse-comment new-pos))
        ; ((equal? c "'")
        ;  (parse-quote new-pos))
        ; ((equal? c #!eof)
        ;  (make-value 'eof #f pos pos #t))
        ((eof-object? c)
         ; not actually included
         (values (make-value 'eof #f pos pos #t) new-pos))
        (else
          (parse-symbol new-pos)))))

  (define (make-cst text logger)
    (define (loop acc pos)
      (let-values (((value new-pos) (parse-value pos)))
        ; (logger "trying over here")
        (cond
          ((eof-object? (car (pos-text new-pos)))
           (reverse acc))
          (else
            (loop (cons value acc) new-pos)))))

    (loop '() (make-pos (append (string->list text) (list #!eof)) 0 0)))

  (define (parse-test)
    (define text (with-input-from-file "test.scm" read-string))
    (make-cst text (lambda (x) (display x)))
    )
  )
