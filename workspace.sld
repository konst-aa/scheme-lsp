(module workspace
  (make-workspace)

  (import (chicken base)
          (chicken format)
          scheme
          utils
          cst
          responses
          srfi-18)

  ; text-document/did-open
  ; text-document/did-change
  ; text-document/did-close
  ; text-document/did-save

  (define-record-type state
    (make-state config init documents symbols logger)
    state?
    (config state-config (setter state-config))
    (init state-init (setter state-init))
    (documents state-documents (setter state-documents))
    (symbols state-symbols (setter state-symbols))
    (logger state-logger)
    )

  (define-record-type doc-state
    (make-doc-state text ast symbols logger)
    doc-state?
    (text doc-state-text (setter doc-state-text))
    (ast doc-state-cst (setter doc-state-cst))
    (symbols doc-state-symbols (setter doc-state-symbols))
    (logger doc-state-logger)
    )

  (define (make-diagnostic range message severity)
    (vector-dict 'range range 'message message 'severity severity))

  (define (mark-numbers cst logger)
    (define (traverse value)
      (case (value-type value)
        ((list) (apply append (map traverse (value-contents value))))
        ((number)
         (logger (sprintf "marking number: ~A" value))
         (list (make-diagnostic (value->range value) "number" 2)))
        ((comment)
         (logger (sprintf "marking comment: ~A" value))
         (list (make-diagnostic (value->range value) "comment" 2)))
        (else '())))
    (apply append (map traverse cst)))

  (define (mark-incomplete cst logger)
    (define (traverse value)
      (case (value-type value)
        ((list)
         (if (value-err? value)
           (list (make-diagnostic (value->range value) "Incomplete list" 1))
           '()))
        ((string)
         (if (value-err? value)
           (list (make-diagnostic (value->range value) "Incomplete string" 1))
           '()))
        (else '())))

    (apply append (map traverse cst)))


  (define (text-document/did-open! state id params)
    (define text-document (retrieve "textDocument" params (list)))
    (define text (retrieve "text" text-document (list)))
    (define logger (doc-state-logger state))

    (set! (doc-state-text state) text)
    (logger
      (sprintf "~A, new text: ~A" (thread-name (current-thread)) (doc-state-text state)))

    (logger "making a cst")
    (set! (doc-state-cst state) (make-cst text logger))

    (define msg (diagnostics-message (symbol->string (thread-name (current-thread)))
                                     (append (mark-numbers (doc-state-cst state) logger)
                                             (mark-incomplete (doc-state-cst state) logger))))

    (logger "marking numbers")
    (logger (mark-numbers (doc-state-cst state) logger))

    (logger (serialize msg))
    (display/flush
      (serialize msg))
    )

  (define (text-document/did-change! state id params)
    "")

  (define (text-document/did-close! state id params)
    "")

  (define (text-document/did-save! state id params)
    "")

  ;; creates a thread responsible for a single document
  ;; returns the thread and its mutex
  (define (make-doc name logger)
    (logger (sprintf "starting doc: ~A" name))

    (define (doc-loop mutex-state)
      (mutex-lock! mutex-state)
      (let*-values (((msg) (thread-receive))
                    ((state) (mutex-specific mutex-state))
                    ((id method params) (parse-params msg)))
        (logger (sprintf "~A received: ~A" (thread-name (current-thread)) msg))
        (case method
          ((textDocument/didOpen)
           (text-document/did-open! state id params))

          ((textDocument/didChange)
           (text-document/did-change! state id params))

          ((textDocument/didClose)
           (text-document/did-close! state id params))

          ((textDocument/didSave)
           (text-document/did-save! state id params))

          (else
            (logger (sprintf "unknown method: ~A" method))
            state))
        )
      (mutex-unlock! mutex-state)
      )

    (define mutex-state
      (make-filled-mutex (make-doc-state "" '() '() logger) name))

    (values (make-mailbox (lambda () (doc-loop mutex-state)) name) mutex-state))

  ;; creates a thread responsible for a workspace that manages documents
  ;; returns the thread and its mutex
  (define (make-workspace init logger name)
    (logger "starting workspace")
    (define (workspace-loop mutex-state)
      (mutex-lock! mutex-state)

      (let*-values (((msg) (thread-receive))
                    ((id method params) (parse-params msg))
                    ((uri)
                     (string->symbol (retrieve "uri" (retrieve "textDocument" params (list)) "fail here"))))
        (logger (sprintf "workspace received: ~A" msg))

        (case method
          ((textDocument/didOpen)
           (let*-values (((docs) (state-documents (mutex-specific mutex-state)))
                         ((doc-thread doc-mutex) (make-doc uri logger)))
             (logger "starting up doc thread")
             (alist-update! uri (list uri doc-thread doc-mutex) docs)
             (thread-start! doc-thread)
             (thread-send doc-thread msg)))

          ((textDocument/didChange textDocument/didClose textDocument/didSave)
           (logger (sprintf "sending to ~A" uri))
           (thread-send (alist-ref uri (state-documents (mutex-specific mutex-state))) msg)
           )))

      (mutex-unlock! mutex-state)
      (workspace-loop mutex-state))

    (define mutex-state
      (make-filled-mutex (make-state '() init '() '() logger) 'workspace-state))

    (values (make-mailbox (lambda () (workspace-loop mutex-state)) name)
            mutex-state))
  )
