(module requests
  (initialize)

  (import scheme
          responses
          (chicken base))

  (define TextDocSync-None 0)
  (define TextDocSync-Full 1)
  (define TextDocSync-Incremental 2)

  (define sync-options
    (vector (cons 'openClose #t)
            (cons 'change TextDocSync-Full)
            (cons 'willSave #f)
            (cons 'willSaveWaitUntil #f)))

  ; https://github.com/jeapostrophe/racket-langserver/blob/570b19570db40647c5ae69c73a67d3a05466a465/methods.rkt#L17
  (define server-capabilities
    (vector (cons 'textDocumentSync sync-options)
            (cons 'hoverProvider #t)
            (cons 'codeActionProvider #t)
            (cons 'definitionProvider #t)
            (cons 'referencesProvider #t)
            (cons 'completionProvider (vector (cons 'triggerCharacters (list "("))))
            (cons 'signatureHelpProvider (vector (cons 'triggerCharacters (list " " ")" "]"))))
            (cons 'inlayHintProvider #t)
            (cons 'renameProvider #t)
            (cons 'documentHighlightProvider #t)
            (cons 'documentSymbolProvider #t)
            (cons 'documentFormattingProvider #t)
            (cons 'documentRangeFormattingProvider #t)
            (cons 'documentOnTypeFormattingProvider
                  (vector (cons 'firstTriggerCharacter ")") (cons 'moreTriggerCharacter (list "\n" "]"))))))

  (define (initialize id params)
    (success-response
      id
      (vector (cons 'capabilities server-capabilities)
              (cons 'serverInfo (vector (cons 'name "Chicken Scheme LSP")
                                        (cons 'version "0.1.0"))))))
  )
