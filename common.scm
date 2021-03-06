(define-module (common))

(use-modules
 ;; for list operation
 (srfi srfi-1))

;;
;; Testing
;;

(define-public (assert msg b)
  (if (not b) (error msg)))

;;
;; File handling
;;

;; copied from http://eikmeier.sites.grinnell.edu/csc151S20/readings/files.html
(define-public read-line-of-chars
  (lambda (source)
                                        ; If we're at the end of the line or the end of the file,
                                        ; then there are no more characters, so return the empty list.
    (cond
                                        ; If we're at the end of the file, there are no more characters,
                                        ; so return the empty list.
     [(eof-object? (peek-char source))
      '()]
                                        ; If we're at the end of the line, we're done with the line
                                        ; skip over the end-of-line character and return the empty list.
     [(char=? (peek-char source) #\newline)
      (read-char source)
      '()]
                                        ; Otherwise, read the current character, read the remaining
                                        ; characters, and join them together.
     [else
      (cons (read-char source) (read-line-of-chars source))])))


(define-public (read-line p)
  (let ((line (read-line-of-chars p)))
    (if (null? line)
        line
        (list->string line))))


(define-public (read-file file)
  (call-with-input-file file
    (lambda (p)
      (let f ((line (read-line p)))
        (if (null? line)
            line
            (cons line (f (read-line p))))))))


(define-public (read-file-contents file)
  (list->string
   (call-with-input-file file
     (lambda (p)
       (let f ((c (read-char p)))
         (if (eof-object? c)
             '()
             (cons c (f (read-char p)))))))))

;;
;; Macros
;;

(define-syntax ->
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x (f . (f-rest ...))) (f x f-rest ...))
    ((_ x f) (f x))
    ((_ x (f . (f-rest ...)) rest ...) (-> (f x f-rest ...) rest ...))
    ((_ x f rest ...) (-> (f x) rest ...))))

(export ->)

(define-syntax ->>
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x (f ...)) (f ... x))
    ((_ x f) `(f x))
    ((_ x (f ...) rest ...) (->> (f ... x) rest ...))
    ((_ x f rest ...) (->> (f x) rest ...))))

(export ->>)


;;
;; Lists
;;

(define-public (contains? el lst)
  (cond
   ((null? lst) #f)
   ((equal? el (car lst)) #t)
   (else (contains? el (cdr lst)))))

(define-public (list-set lst idx val)
  (if (null? lst)
      lst
      (cons
       (if (zero? idx)
           val
           (car lst))
       (list-set (cdr lst) (- idx 1) val))))

(define-public (list-update lst idx f)
  (let ((el (list-ref lst idx)))
    (list-set lst idx (f el))))

(define-public (list-indices pred lst)
  (let f ((current-index 0)
          (indices '())
          (lst lst))
    (if (null? lst)
        (reverse indices)
        (let ((index (list-index pred lst)))
          (if (not index)
              (reverse indices)
              (f (+ 1 current-index index)
                 (cons (+ current-index index) indices)
                 (list-tail lst (+ 1 index))))))))

;; https://stackoverflow.com/a/20591545/4379329
(define-public (combinations . lists)
  (fold-right
   (lambda (xs ys)
     (append-map
      (lambda (x)
        (map (lambda (y)
               (cons x y))
             ys))
      xs))
   '(())
   lists))
