#!/usr/local/bin/guile -L .. -s
!#

(use-modules
 ;; for format ~d
 (ice-9 format)
 (ice-9 string-fun)
 (common))

(define (valid-xmas? preamble-count numbers)
  (let ((preamble (list-head numbers preamble-count))
        (num (list-ref numbers preamble-count)))
    (contains? num (map (lambda (nums) (apply + nums))
                        (combinations preamble preamble)))))

(define example-input
  (->> (string-split "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576" #\newline)
       (map string->number)))

(assert "valid-xmas" (valid-xmas? 5 example-input))

(define (part-1 preamble-count numbers)
  (let f ((numbers numbers))
    (if (valid-xmas? preamble-count numbers)
        (f (cdr numbers))
        (list-ref numbers preamble-count))))

(assert "example part 1" (= 127 (part-1 5 example-input)))

(define input
  (->> (read-file "day9-input")
       (map string->number)))

(format #t "Part 1: ~d\n" (part-1 25 input))


(define (part-2 number numbers)
  (let f ((nums-left numbers)
          (nums-added '())
          (total 0))
    (cond
     ((null? numbers) #f)
     ((= number total) (+ (apply min nums-added)
                          (apply max nums-added)))
     ((< number total) (part-2 number (cdr numbers)))
     (else (f (cdr nums-left)
              (cons (car nums-left) nums-added)
              (+ total (car nums-left)))))))

(assert "example part 2" (= 62 (part-2 127 example-input)))

(format #t "Part 2: ~d\n" (part-2 466456641 input))
