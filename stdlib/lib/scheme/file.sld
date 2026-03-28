(define-library (scheme file)
  (description "File I/O: open-input-file, open-output-file, file-exists?, delete-file.")
  (export open-input-file
          open-output-file
          open-binary-input-file
          open-binary-output-file
          call-with-input-file
          call-with-output-file
          with-input-from-file
          with-output-to-file
          file-exists?
          delete-file))
