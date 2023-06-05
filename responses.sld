(module responses
  (serialize success-response)

  (import scheme
          (chicken base)
          (chicken port)
          (chicken format)
          json)

  (define (serialize vec)
    (let* ((json-payload (call-with-output-string
                           (lambda (port)
                             (json-write vec port))))
           (len (string-length json-payload)))
      (sprintf "Content-Length: ~A\r\n\r\n~A" len json-payload)))

  (define (success-response id result)
    (serialize (vector (cons "jsonrpc" "2.0")
                       (cons "id" id)
                       (cons "result" result))))
  )
