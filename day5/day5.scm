#!/usr/local/bin/guile -L .. -s
!#

(use-modules
 ;; for list operation find
 (srfi srfi-1)
 ;; for format ~d
 (ice-9 format)
 (ice-9 receive)
 (common))

(define (split-seats seats choice)
  (if (or (char=? #\F choice)
          (char=? #\L choice))
      (take seats (/ (length seats) 2))
      (drop seats (/ (length seats) 2))))

(define (make-row)
  (iota 128 0))

(define (make-col)
  (iota 8 0))

(define examples
  '("FBFBBFFRLR"
    "BFFFBBFRRR"
    "FFFBBBFRRR"
    "BBFFBBFRLL"))

(define (boarding-pass->seat-id boarding-pass)
  (receive (row-spec col-spec)
      (split-at (string->list boarding-pass) 7)
    (let f ((row-spec row-spec)
            (col-spec col-spec)
            (row (make-row))
            (col (make-col)))
      (cond
       ((= 1 (length row)) (+ (* 8 (car row)) (car col)))

       ((= 1 (length col)) (f (cdr row-spec)
                                   col-spec
                                   (split-seats row (car row-spec))
                                   col))

       (else (f (cdr row-spec)
                 (cdr col-spec)
                 (split-seats row (car row-spec))
                 (split-seats col (car col-spec))))))))


(assert
 "boarding-pass->seat-id test input"
 (equal? '(357 567 119 820) (map boarding-pass->seat-id examples)))

(define input
  (read-file "day5-input"))

(define (part-1 input)
  (apply max (map boarding-pass->seat-id input)))


(assert "part 1 test input" (= 820 (part-1 examples)))


(format #t "Part 1: ~d\n" (part-1 input))


;; (display (sort (map boarding-pass->seat-id input) <))

(define (part-2 input)
  (let f ((seat-ids (sort (map boarding-pass->seat-id input) <)))
    (let ((cur (car seat-ids))
          (next (cadr seat-ids)))
      (if (= 1 (- next cur))
          (f (cdr seat-ids))
          (+ 1 cur)))))


(format #t "Part 2: ~d\n" (part-2 input))
