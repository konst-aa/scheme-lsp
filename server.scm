(import json
        (chicken io)
        (chicken port)
        (chicken string)
        (chicken format)
        srfi-1
        srfi-18
        srfi-69
        error-codes
        responses
        utils
        text-document
        workspace)

(define TextDocSync-None 0)
(define TextDocSync-Full 1)
(define TextDocSync-Incremental 2)

(define sync-options
  (vector-dict 'openClose #t
               'change TextDocSync-Full
               'willSave #f
               'willSaveWaitUntil #f))

; https://github.com/jeapostrophe/racket-langserver/blob/570b19570db40647c5ae69c73a67d3a05466a465/methods.rkt#L17
(define server-capabilities
  (vector-dict 'textDocumentSync sync-options
               'hoverProvider #t
               'codeActionProvider #t
               'definitionProvider #t
               'referencesProvider #t
               'completionProvider (vector-dict 'triggerCharacters (list "("))
               'signatureHelpProvider (vector-dict 'triggerCharacters (list " " ")" "]"))
               'inlayHintProvider #t
               'renameProvider #t
               'documentHighlightProvider #t
               'documentSymbolProvider #t
               'documentFormattingProvider #t
               'documentRangeFormattingProvider #t
               'documentOnTypeFormattingProvider
               (vector-dict 'firstTriggerCharacter ")"
                            'moreTriggerCharacter (list "\n" "]"))))

(define (initialize id params)
  (success-response
    id
    (vector-dict 'capabilities server-capabilities
                 'serverInfo (vector-dict 'name "Chicken Scheme LSP"
                                          'version "0.1.0"))))

(define (shutdown id)
  (success-response id #f))

(define log-input (make-logger! "in"))
(define log-output (make-logger! "out"))

(define (worker-loop queue workspace-thread workspace-mutex)
  (define (dispatch! msg)
    (define-values (id method params) (parse-params msg))

    (log-output (sprintf "~A: handled\n ~A" (thread-name (current-thread)) method))

    (case method
      ((textDocument/didOpen textDocument/didChange textDocument/didClose textDocument/didSave)
       (log-output "sent to workspace thread")
       (thread-send workspace-thread msg))

      ; ((textdocument/hover)
      ;  (text-document/hover id params))

      ; ((textdocument/codeaction)
      ;  (text-document/code-action id params))

      ; ((textdocument/completion)
      ;  (text-document/completion id params))

      ; ((textdocument/signaturehelp)
      ;  (text-document/signaturehelp id params))

      ; ((textdocument/definition)
      ;  (text-document/definition id params))

      ; ((textdocument/documenthighlight)
      ;  (text-document/document-highlight id params))

      ; ((textdocument/references)
      ;  (text-document/references id params))

      ; ((textdocument/documentsymbol)
      ;  (text-document/document-symbol id params))

      ; ((textdocument/inlayhint)
      ;  (text-document/inlay-hint id params))

      ; ((textdocument/rename)
      ;  (text-document/rename id params))

      ; ((textdocument/preparerename)
      ;  (text-document/preparerename id params))

      ; ((textdocument/formatting)
      ;  (text-document/formatting! id params))

      ; ((textdocument/rangeformatting)
      ;  (text-document/range-formatting! id params))

      ; ((textdocument/ontypeformatting)
      ;  (text-document/on-type-formatting! id params))

      (else
        ;(log-output (sprintf "invalid request for method ~a\n" method))
        ;(error-response id method-not-found (sprintf "the method ~a was not found" method))
        ;(log-output (text-document/publish-diagnostics "wha"))
        ;(text-document/publish-diagnostics "wha")
        )))

  (mutex-lock! queue)

  (let ((field (mutex-specific queue)))
    (cond
      ((null? field)
       (mutex-unlock! queue))
      (else
        (let ((msg (car field)))
          (mutex-specific-set! queue (cdr field))
          (mutex-unlock! queue)
          (dispatch! msg)
          ))))

  (worker-loop queue workspace-thread workspace-mutex))



(define initialized #f)

(define (process-message queue msg)
  (define (prune x)
    (let ((id (retrieve "id" x 0)))
      (not (equal? id (retrieve "id" msg 0)))))

  (mutex-lock! queue)

  (let-values (((field) (mutex-specific queue))
               ((id method params) (parse-params msg)))
    (case method
      ((initialize)
       (if (not initialized)
         (begin
           (let-values (((workspace-thread workspace-mutex) 
                         (make-workspace msg (make-logger! "workspace") 'workspace)))
             (set! initialized #t)
             (thread-start! workspace-thread)

             (make-and-start-thread! (lambda () (worker-loop queue workspace-thread workspace-mutex)) 'reader1)
             ; (make-and-start-thread! (lambda () (worker-loop queue workspace-thread workspace-mutex)) 'reader2)
             ; (make-and-start-thread! (lambda () (worker-loop queue workspace-thread workspace-mutex)) 'reader3)

             (display/flush (serialize (initialize id params)))
             ))
         ;error here
         (display/flush (error-response id invalid-request "already initialized")))
       )

      ((stop)
       (display/flush (stop id)))

      (($/cancelRequest)
       (mutex-specific-set!
         queue
         (filter prune field)))

      (else
        (log-input "adding to queue")
        (mutex-specific-set! queue (cons msg field)))))

  (mutex-unlock! queue))

(log-input "starting up")
(define queue (make-filled-mutex '() 'queue))

(define (loop)
  (process-message queue (read-msg log-input))
  (loop))

(loop)
