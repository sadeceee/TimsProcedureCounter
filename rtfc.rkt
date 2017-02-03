#lang racket
(require racket/file)

; Racket Tims Procedure Counter
; Tim λ Counter
; Tim λ i
; τiϻ λ ι

(define args (vector->list (current-command-line-arguments)))

(define (show-help)
  (displayln "-f <file> | to count function in racket file, <file> := path to file")
  (displayln "-h        | to show help"))

(define (parse args)
  (let ((len (length args)))
    (cond [(and (> len 1) (equal? "-f" (car args))) (cadr args)]
          [(and (> len 0) (equal? "-h" (car args))) (show-help)]
          [else (show-help)])))

(define path-to-file (let ((f (parse args))) (if (string? f) f "")))

(unless (non-empty-string? path-to-file) (error "<ERROR>: Missing path to file after -f."))
(unless (file-exists? path-to-file) (error (string-append "<ERROR>: File not found: " path-to-file)))
(unless (string-contains? path-to-file ".rkt") (error (string-append "<ERROR>: Not a racket file: " path-to-file)))


(define in (open-input-file path-to-file))

(define file-as-list (string-split (string-replace (string-replace (read-line in 'return) "\n" " ") "'(" "") " "))

(define (count-procedures l)
  (let* ((procs (map (compose (curryr string-replace "(" "") (curryr string-replace ")" "")
                              (curryr string-replace "[" "") (curryr string-replace "]" ""))
                     (filter (curryr string-contains? "(") l)))
         (unique-procs (remove-duplicates procs)))
    (sort (map (λ (p) (cons p (count (curry equal? p) procs))) unique-procs)
          (λ (x y) (> (cdr x) (cdr y))))))

(define (display-procedure-count)
  (map (λ (p) (displayln
               (string-append (number->string (cdr p)) " : " (car p))))
       (count-procedures file-as-list))
  (values))

(display-procedure-count)





