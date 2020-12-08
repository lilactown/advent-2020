#!/usr/local/bin/guile -L .. -s
!#

(use-modules
 ;; for format ~d
 (ice-9 format)
 (ice-9 string-fun)
 (common))


(define (make-context code)
  ;; (line code acc)
  (list 0 code 0))

(define (step context)
  (let ((line (car context))
        (code (cadr context))
        (acc (caddr context)))
    (if (<= (length code) line)
        #f
        (let ((instruction (list-ref code line)))
          (let ((op (car instruction))
                (arg (cdr instruction)))
            (cond
             ((string=? op "nop")
              (list (+ 1 line)
                    code
                    acc))
             ((string=? op "acc")
              (list (+ 1 line)
                    code
                    (+ acc arg)))
             ((string=? op "jmp")
              (list (+ line arg)
                    code
                    acc))))))))


(define example-program
  (->> (string-split "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6" #\newline)
       (map (lambda (line)
              (let ((split (string-split line #\space)))
                (cons (car split)
                      (string->number (cadr split))))))))

(->
 (make-context example-program)
 (step)
 (step)
 (step))

(define (part-1 program)
  (let f ((context (make-context program))
          (lines-seen (list 0)))
    (let ((next (step context)))
      (cond
       ;; return accumulator if we have ended
       ((eq? #f next) (caddr context))
       ;; return last instruction and accumulator if we detect a loop
       ((contains? (car next) lines-seen)
        (cons (list-ref (cadr context) (car context))
              (caddr context)) )
       ;; continue
       (else (f next (cons (car next) lines-seen)))))))

(assert "part-1 example"
        (equal? (cons (cons "jmp" -3) 5)
                (part-1 example-program)))

(define input
  (->> (read-file "day8-input")
       (map (lambda (line)
              (let ((split (string-split line #\space)))
                (cons (car split)
                      (string->number (cadr split))))))))

(format #t "Part 1: ~d\n" (cdr (part-1 input)))

(assert "ending example 1"
        (= 0 (part-1 (list (cons "nop" 0)))))

(define (part-2 program)
  (let ((jmp-nop-positions (list-indices
                            (lambda (instruction)
                              (or (string=? "jmp" (car instruction))
                                  (string=? "nop" (car instruction))))
                            program)))
    (let f ((pos-left jmp-nop-positions))
      (let ((result (part-1 (list-update
                             program
                             (car pos-left)
                             (lambda (instruction)
                               (if (string=? "jmp" (car instruction))
                                   (cons "nop" (cdr instruction))
                                   (cons "jmp" (cdr instruction))))))))
        (if (pair? result)
            ;; infinite loop detected; try changing a different instruction
            (f (cdr pos-left))
            result)))))

(assert "part-2 example"
        (= 8 (part-2 example-program)))

(format #t "Part 2: ~d\n" (part-2 input))
