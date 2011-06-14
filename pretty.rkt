#! env racket

#lang racket

(require racket/pretty)

(define arg1 (vector-ref (current-command-line-arguments) 0))
(define infile (open-input-file arg1))
(define outfile (open-output-file (string-append "pretty_" arg1) 
                              #:exists 
                              'truncate))

(define (pretty-it in out)
    (let ((str (read-line in)))
      (if (eof-object? str)
          (begin (close-output-port out)
                 (close-input-port in))
          (begin (pretty-display (read (open-input-string str)) out)
                 (pretty-it in out)))))

(pretty-it infile outfile)
