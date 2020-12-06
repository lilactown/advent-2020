#!/usr/local/bin/guile -L .. -s
!#

(use-modules
 ;; for list operation
 (srfi srfi-1)
 ;; for format ~d
 (ice-9 format)
 (ice-9 string-fun)
 (common))

(define (parse-groups input)
  (-> input
      (string-replace-substring "\n\n" "%")
      (string-split #\%)))

(define example-input
  (parse-groups "abc

a
b
c

ab
ac

a
a
a
a

b"))

(define (part-1 input)
  (->> input
       (map (lambda (group-string)
              ;; convert entire group to one list and filter out newlines
              (filter
               (lambda (c)
                 (not (char=? c #\newline)))
               (string->list group-string))))
       (map (lambda (group-list)
              ;; create hashmap of group's answers
              (let f ((group-list group-list)
                      (group (make-hash-table 26))) ;; max 26 letters
                (if (null? group-list)
                    group
                    (f (cdr group-list)
                       (begin
                         (hashq-set! group (car group-list) #t)
                         group))))))
       (map (lambda (group) (hash-count (const #t) group)))
       (apply +)))

(assert "example part 1" (= 11 (part-1 example-input)))

(define input
  (parse-groups (read-file-contents "day6-input")))

(format #t "Part 1: ~d\n" (part-1 input))

(define (part-2 input)
  (->> input
       (map (lambda (group-string)
              ;; keep each person's choices as a separate list
              (map string->list (string-split group-string #\newline))))
       (map (lambda (group)
              ;; count every letter
              (count (lambda (letter)
                       ;; that shows up in everyones choices
                       (every (lambda (choices)
                                (contains? letter choices))
                              group))
                     '(#\a #\b #\c #\d #\e #\f #\g #\h #\i
                       #\j #\k #\l #\m #\n #\o #\p #\q #\r
                       #\s #\t #\u #\v #\w #\x #\y #\z))))
       (apply +)))

(assert "example part 2" (= 6 (part-2 example-input)))

(format #t "Part 2: ~d\n" (part-2 input))
