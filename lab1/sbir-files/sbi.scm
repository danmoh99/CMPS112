#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.9 2019-01-14 15:19:06-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))


(define *variable-table* (make-hash))
(define *function-table* (make-hash))
(define *array-table* (make-hash))
(define *label-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))
(define (function-get key)
        (hash-ref *function-table* key))
(define (function-put! key value)
        (hash-set! *function-table* key value))
(define (array-get key)
        (hash-ref *array-table* key))
(define (array-put! key value)
        (hash-set! *array-table* key value))
(define (label-get key)
        (hash-ref *label-table* key))
(define (label-put! key value)
        (hash-set! *label-table* key value))

(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(

        (e       ,(exp 1.0))
        (pi      ,(acos -1.0))
        (nan     ,(/ 0.0 0.0))
        (eof     0.0)

     ))
(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    '(

        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (-       ,-)
        (*       ,*)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)
        (log2    ,(lambda (x) (/ (log x) (log 2.0))))
        (abs     ,abs)
        (acos    ,acos)
        (asin    ,asin)
        (atan    ,atan)
        (cos     ,cos)
        (round   ,round)
        (sin     ,sin)
        (tan     ,tan)
        (trunc   ,truncate)
        

    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interpret-program program)
        (printf "Attempting to interpret~n")
        (when (not (null? program))
              (printf "Is the cdr null? ~v~n" (null? (cdr program)))
              (let ((line (car program)))
                   (printf "~v~n" line)
                   (printf "Remaining code: ~v~n" (cdr program))
                   (when (null? (cdr line))
                         (printf "The cdr is null. Trying to recur.~n")
                         (interpret-program (cdr program)))




                   (when (not (symbol? (cadr line)))
                         (let ((statement (cadr line)))
                              (printf "Evaluating expression~v~n" statement)
                              (cond 
                                   ((eq? (car statement) 'print)
                                          (interpret-print (cadr statement)))
                                   ((eq? (car statement) 'goto)
                                         (interpret-goto (cadr statement)))
                              )
                              (when (not (eq? (car statement) 'goto))
                                    (interpret-program (cdr program)))))
                   
                   
                   (when (symbol? (cadr line))
                         (when (null? (cddr line))
                               (interpret-program (cdr program)))
                     
                     
                     
                     ))))


(define (interpret-print arg)
        (printf "Made it to print~n")
        (printf arg)
        (printf "~n"))

(define (interpret-goto arg)
        (interpret-program (label-get arg)))
              

(define (initialize-label read-program)
        (when (not (null? read-program))
            (let ((line (car read-program)))
                 (when (not (null? (cdr line)))
                       (when (symbol? (cadr line))
                             (label-put! (cadr line) read-program)))
                 (initialize-label (cdr read-program)))))
            

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))


(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
               ;;(write-program-by-line sbprogfile program))))
               (initialize-label program)
               (interpret-program program))))

(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))
