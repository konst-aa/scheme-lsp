(module responses
  (serialize
    Diag-Error
    Diag-Warning
    Diag-Information
    Diag-Hint
    success-response
    error-response
    diagnostics-message
    random-id)

  (import scheme
          (chicken base)
          (chicken port)
          (chicken format)
          (chicken random)
          uuid
          utils
          json)

  ;; Constructor for a response object representing success.
  (define (success-response id result)
    (vector-dict 'jsonrpc "2.0"
                 'id id
                 'result result))

  (define not-given (gensym 'not-given))

  (define Diag-Error 1)
  (define Diag-Warning 2)
  (define Diag-Information 3)
  (define Diag-Hint 4)

  (define (random-id)
    (pseudo-random-integer 1000000))

  ;; Constructor for a response object representing failure.
  (define (error-response id code message data)
    (define err (vector-dict 'code code
                             'message message
                             'data data))
    (vector-dict 'jsonrpc "2.0"
                 'id id
                 'error err))

  ;; Constructor for a response object representing diagnostics.
  (define (diagnostics-message uri diags)
    (vector-dict 'jsonrpc "2.0"
                 'method "textDocument/publishDiagnostics"
                 'params (vector-dict 'uri uri
                                      'diagnostics diags)))
  )
