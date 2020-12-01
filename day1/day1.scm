#!/usr/local/bin/guile -s
!#

;; for list operation find
(use-modules (srfi srfi-1))

;; for format ~d
(use-modules (ice-9 format))

(define (assert msg b)
  (if (not b) (error msg)))

(define test-expense-report
  (list 979
        1721
        366
        299
        675
        1456))


;;
;; Part 1
;;

(define (part-1 sum lst)
  (let ((hd (car lst))
        (rest (cdr lst)))
    (let ((needle (find (lambda (n)
                            (= sum (+ hd n)))
                          rest)))
      (cond
       ;; if we exhausted the list, return false
       ((null? rest) #f)
       ;; if we haven't found a needle, recurse with next `hd`
       ((not needle) (part-1 sum rest))
       ;; we found a needle
       (else (* hd needle))))))

(assert "Test input for part 1"
        (eqv?
         514579
         (part-1 2020 test-expense-report)))

(define expense-report
  (call-with-input-file "day1-input"
    (lambda (p)
      (let f ((x (read p)))
        (if (eof-object? x)
            '()
            (cons x (f (read p))))))))

(format #t "Part 1: ~d\n" (part-1 2020 expense-report))


;;
;; Part 2
;;

(define (part-2 sum lst)
  (let ((hd (car lst))
        (rest (cdr lst)))
    ;; reuse part 1 to find two numbers that sume to (- 2020 hd)
    (let ((needle (part-1 (- 2020 hd) rest)))
      (cond
       ((null? rest) rest)
       ((not needle) (part-2 sum rest))
       (else (* hd needle))))))

(assert "Test input for part 2"
        (eqv?
         241861950
         (part-2 2020 test-expense-report)))

(format #t "Part 2: ~d\n" (part-2 2020 expense-report))
