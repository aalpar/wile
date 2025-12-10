(define-library (scheme file)
  (export open-input-file
          open-output-file
          close-port
          close-input-port
          close-output-port
          input-port?
          output-port?
          input-port-open?
          output-port-open?
          file-exists?
          delete-file))
