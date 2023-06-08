(module text-document
  (text-document/publish-diagnostics
    text-document/hover
    text-document/code-action
    text-document/completion
    text-document/signature-help
    text-document/definition
    text-document/document-highlight
    text-document/references
    text-document/document-symbol
    text-document/inlay-hint
    text-document/rename
    text-document/prepareRename
    text-document/formatting!
    text-document/range-formatting!
    text-document/on-type-formatting!)

  (import scheme
          srfi-18
          (chicken base)
          responses
          utils)

  ;;; Queries against the workspace

  (define (text-document/publish-diagnostics msg)
    (diagnostics-message "file:///home/konst/code/scheme-lsp/test.scm"
                         (list (text-document/diagnostics msg))))

  (define (text-document/diagnostics msg)
    (vector-dict 'range (range (cons 0 0) (cons 1 1))
                 'severity 1
                 'code 1
                 'source "scheme-lsp"
                 'message msg))

  (define (text-document/uri msg)
    "")

  (define (text-document/hover id params)
    "")

  (define (text-document/code-action id params)
    "")

  (define (text-document/completion id params)
    "")

  (define (text-document/signature-help id params)
    "")

  (define (text-document/definition id params)
    "")

  (define (text-document/document-highlight id params)
    "")

  (define (text-document/references id params)
    "")

  (define (text-document/document-symbol id params)
    "")

  (define (text-document/inlay-hint id params)
    "")

  (define (text-document/rename id params)
    "")

  (define (text-document/prepareRename id params)
    "")

  (define (text-document/formatting! id params)
    "")

  (define (text-document/range-formatting! id params)
    "")

  (define (text-document/on-type-formatting! id params)
    "")
  )

