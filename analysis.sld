(module analysis
  (first-pass)
  (import scheme
          (chicken base)
          (chicken string)
          (chicken io)
          (chicken port)
          (chicken format)
          cst
          srfi-1
          )

  (define-record-type info
    (make-info type
               contents
               uri
               doc
               loc)
    info?
    (type info-type (setter info-type))
    (contents info-contents)
    (uri info-uri)
    (doc info-doc (setter info-doc))
    (loc info-loc))


  (define-record-printer (info i out)
    (fprintf out "#,(info type: ~s contents: ~s uri: ~s doc: ~s loc: ~s)"
             (info-type i) (info-contents i) (info-uri i) (info-doc i) (info-loc i)))

  (define (make-loc value)
    (cons (value-start value) (value-end value)))

  (define (newline-on-full str)
    (if (equal? str "")
      ""
      "\n"))

  ;; used for documentation

  (define (join-comments lst)

    (define (get-comment-body comment offset)
      (list->string (drop (string->list (value-contents comment)) offset)))

    (define (parse-type comment-body)
      (with-input-from-string comment-body read))

    ;;d appends the comment to the previous s-expr/symbol
    (define (appended-doc? comment-text)
      (substring=? ";;" comment-text 0 0 2))

    ;;d appends the comment to the previous s-expr/symbol
    (define (prepended-doc? comment-text)
      (substring=? ";d" comment-text 0 0 2))

    (define (appended-type? comment-text)
      (substring=? "<" comment-text 0 0 1))

    (define (prepended-type? comment-text)
      (substring=? ";>" comment-text 0 0 2))

    (define (apply-changes item changes)
      (define (fold-fn acc change)
        (case (car change)
          ((type)
           (set! (info-type acc) (cdr change)))
          ((doc)
           (set! (info-doc acc)
             (string-append
               (info-doc acc)
               (newline-on-full (info-doc acc))
               (cdr change))))
          (else 'noop))
        acc)
      (foldl fold-fn item (reverse changes))

      )
    (define (fold-fn acc item)
      (let ((items (car acc))
            (changes (cdr acc)))
        (cond
          ((and (value? item) (eq? (value-type item) 'comment))
           (cond
             ((and (not (null? items)) (appended-doc? (value-contents item)))
              (set! (info-doc (car items))
                (string-append
                  (info-doc (car items))
                  (newline-on-full (info-doc (car items)))
                  (get-comment-body item 3))) ; to account for the space
              )
             ((and (not (null? items)) (appended-type? (value-contents item)))
              (set! (info-type (car items)) (parse-type (get-comment-body item 2)))
              )
             ((prepended-doc? (value-contents item))
              (set! changes (cons (cons 'doc (get-comment-body item 3)) changes))
              )
             ((prepended-type? (value-contents item))
              (set! changes (cons (cons 'type (parse-type (get-comment-body item 3))) changes))
              )
             (else 'noop))
           (cons items changes))
          (else (cons (cons (apply-changes item changes) items) '())))
        )
      )

    (reverse (car (foldl fold-fn '(() . ()) lst))))

  ;; a scope is a list of symbols

  (define (first-pass cst uri logger)
    ;;; Does comment expansion
    ;;; converts the cst to a list of symbol-infos and s-expr-infos


    (define (traversal value)
      (case (value-type value)
        ((comment)
         (list value)) ; comments will be joined later
        ((symbol)
         (list (make-info 'symbol (cons (value-contents value) 'unbound) uri "" (make-loc value))))
        ((number string)
         (list (make-info (value-type value) (value-contents value) uri "" (make-loc value))))
        ((list)
         (let ((prepared-list
                 (join-comments (apply append (map traversal (value-contents value))))))
           (list (make-info 'unknown
                            prepared-list
                            uri
                            ""
                            (make-loc value))))
         )))

    (join-comments (apply append (map traversal cst)))
    )
  )
