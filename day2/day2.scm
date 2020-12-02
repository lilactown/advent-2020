#!/usr/local/bin/guile -L .. -s
!#

(use-modules
 ;; for list operation find
 (srfi srfi-1)
 ;; for format ~d
 (ice-9 format)
 (common))

(define (first-policy policy)
  (car policy))

(define (second-policy policy)
  (car (cdr policy)))

(define (policy-char policy)
  (car (cdr (cdr policy))))

(define (parse-passwords+policys input)
  (map
   (lambda (line)
     (let ((split-line (string-split line #\:)))
       (let ((split-policy (string-split (car split-line) #\space))
             (password (string-trim (car (cdr split-line)))))
         (let ((occurences (string-split (car split-policy) #\-)))
           (list (list
                  (string->number (car occurences)) ;; min
                  (string->number (car (cdr occurences))) ;; max
                  (car (string->list (car (cdr split-policy))))) ;; character
                 (string->list password))))))
   input))


(define test-passwords
  (parse-passwords+policys
   (string-split "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc" #\newline)))


(define (policy-match? policy password occurence-count)
  (let ((policy-min (first-policy policy))
        (policy-max (second-policy policy))
        (char (policy-char policy)))
    (cond
     ((< policy-max occurence-count) #f)
     ((null? password) (<= policy-min occurence-count))
     (else (policy-match?
            policy
            (cdr password)
            (if (eq? char (car password))
                (+ 1 occurence-count)
                occurence-count))))))


(define (part-1 input)
  (count (lambda (policy+pw)
           (policy-match? (car policy+pw) (car (cdr policy+pw)) 0))
         input))

(assert "Test password policy"
        (= 2 (part-1 test-passwords)))

(define passwords+policies
  (parse-passwords+policys (read-file "day2-input")))

(format #t "Part 1: ~d\n" (part-1 passwords+policies))


(define (policy-match2? policy password)
  (let ((char (policy-char policy)))
    (not (eq? (eq? char (list-ref password (- (first-policy policy) 1)))
              (eq? char (list-ref password (- (second-policy policy) 1)))))))

(define (part-2 input)
  (count (lambda (policy+pw)
           (policy-match2? (car policy+pw) (car (cdr policy+pw))))
         input))

(assert "Test part 2 policy"
        (= 1 (part-2 test-passwords)))

(format #t "Part 2: ~d\n" (part-2 passwords+policies))
