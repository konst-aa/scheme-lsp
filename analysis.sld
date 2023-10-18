(module analysis
  (first-pass
    second-pass)
  (import scheme
          (chicken base)
          (chicken string)
          (chicken io)
          (chicken port)
          (chicken format)
          srfi-69
          cst
          srfi-1
          utils
          )

  (define-record-type info
    (make-info type
               inferred-type
               contents
               uri
               doc
               loc)
    info?
    (type info-type (setter info-type))
    (inferred-type info-inferred-type (setter info-inferred-type))
    (contents info-contents)
    (uri info-uri)
    (doc info-doc (setter info-doc))
    (loc info-loc))


  (define-record-printer (info i out)
    (fprintf out "#,(info type: ~s inferred-type: ~s contents: ~s uri: ~s doc: ~s loc: )"
             (info-type i) (info-inferred-type i) (info-contents i) (info-uri i) (info-doc i)
             ;(info-loc i)
             ))

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
      (define t (with-input-from-string comment-body read))
      t)

    ;;d ;;; adds the comment to the previous s-expr
    (define (appended-doc? comment-text)
      (substring=? ";;" comment-text 0 0 2))

    ;;d ;;d prepends the comment to the next s-expr
    (define (prepended-doc? comment-text)
      (substring=? ";d" comment-text 0 0 2))

    ;;d ;t  adds type signature to the previous s-expr
    (define (appended-type? comment-text)
      (substring=? "t" comment-text 0 0 1))

    ;;d ;;t adds type signature to the next s-expr
    (define (prepended-type? comment-text)
      (substring=? ";t" comment-text 0 0 2))


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

    (reverse (car (foldl fold-fn '(() . ()) lst)))

    )

  ;; a scope is a list of symbols
  (define-record-type scope-entry
    (make-scope-entry name type exec loc uri)
    scope-entry?
    (name scope-entry-name)
    (type scope-entry-type)
    (exec scope-entry-exec)
    (loc scope-entry-loc)
    (uri scope-entry-uri))


  (define (first-pass cst uri logger)
    ;;; Does comment expansion
    ;;; converts the cst to a list of info records, removes comments

    (define (traversal value)
      (case (value-type value)
        ((comment)
         (list value)) ; comments will be joined later
        ((symbol)
         (list (make-info 'symbol 'unknown (cons (value-contents value) 'unbound) uri "" (make-loc value))))
        ((number string)
         (list (make-info (value-type value) 'unknown (value-contents value) uri "" (make-loc value))))
        ((list)
         (let ((prepared-list
                 (join-comments (apply append (map traversal (value-contents value))))))
           (list (make-info 'list 'unknown prepared-list uri "" (make-loc value))))
         )))

    (define t (map traversal cst))

    (join-comments (apply append t))
    )

  (define (nil-op anst scope uri logger)
    (let* ((proc-list (cdr (info-contents anst)))
           (resolved-list (map (lambda (anst) (resolve anst scope))
                               proc-list)))
      )
    (cons (make-info ) scope))

  (define (retrieve-symbol sym-info)
    (car (info-contents sym-info)))

  (define (define-op anst scope uri logger)
    (let ((proc-list (cdr (info-contents anst))))
      (if (and (eq? 'symbol (info-type (car proc-list))) (not (null? (cdr proc-list))))
        (begin
          (set! (hash-table-ref scope (retrieve-symbol (car proc-list)))
            (info-contents (cadr proc-list)))
          (cons anst scope))
        (cons anst scope))))

  (define dummy-loc (cons dummy-pos dummy-pos))

  (define (second-pass anst scope uri logger)
    ;;; Does type inference and scope resolution
    (define scope
      (alist->hash-table
        (list (cons '+
                    (make-scope-entry
                      '+
                      '(number number -> number)
                      nil-op
                      dummy-loc
                      uri))
              (cons 'define
                    (make-scope-entry
                      'define
                      '(symbol any -> undefined)
                      define-op
                      dummy-loc
                      uri))
              )))

    (define (resolve anst scope)
      (case (info-type anst)
        ((list)
         (let ((resolved-first (resolve (car (info-contents anst)) scope)))
           ((scope-entry-exec resolved-first) anst scope uri logger)))
        ((symbol)
         (let ((retrieved (hash-table-ref scope (retrieve-symbol anst))))
           (if (not (null? retrieved))
             retrieved
             (error "Unbound symbol" (info-contents anst)))))
        (else (cons anst scope)))
      )
    (foldl (lambda (acc item)
             (let ((resolved (resolve item (cdr acc))))
               (cons (cons (car resolved) (car acc)) (cdr resolved))))
           (cons '() scope) anst)
    )
  )
