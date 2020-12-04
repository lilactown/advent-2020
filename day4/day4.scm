#!/usr/local/bin/guile -L .. -s
!#

(use-modules
 ;; for list operation find
 (srfi srfi-1)
 ;; for format ~d
 (ice-9 format)
 (ice-9 string-fun)
 (common))

(define (normalize-input input-string)
  (-> input-string
      (string-replace-substring "\n\n" "%")
      (string-replace-substring "\n" " ")
      (string-split #\%)))

(define (string->passport line)
  (->> (string-split line #\space)
       (map (lambda (s)
              (let ((split (string-split s #\:)))
                ;; create a pair ("key" . "value") instead of a list,
                ;; turning the entire structure into an associative list
                (cons (car split) (cadr split)))))))

(define example-batch
  (->>
   "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"
   (normalize-input)
   (map string->passport)))

(define required-attributes
  '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" ;"cid"
    ))

(define (valid-passport? passport)
  (every (lambda (attr) (assoc attr passport)) required-attributes))

(assert "test input" (= 2 (count valid-passport? example-batch)))

(format #t "Part 1: ~d\n" (->> (read-file-contents "day4-input")
                               (normalize-input)
                               (map string->passport)
                               (count valid-passport?)))

(define (apply-rule attr+val)
  (if (not attr+val) ;; #f is returned by assoc if not found
      #f
      (let ((attr (car attr+val))
            (val (cdr attr+val)))
        (cond
         ((string=? attr "byr")
          (let ((int-val (string->number val)))
            (and (= 4 (string-length val))
                 (<= 1920 int-val)
                 (>= 2002 int-val))))
         ((string=? attr "iyr")
          (let ((int-val (string->number val)))
            (and (= 4 (string-length val))
                 (<= 2010 int-val)
                 (>= 2020 int-val))))
         ((string=? attr "eyr")
          (let ((int-val (string->number val)))
            (and (= 4 (string-length val))
                 (<= 2020 int-val)
                 (>= 2030 int-val))))
         ((string=? attr "hgt")
          ;; a number followed by either cm or in:
          ;; If cm, the number must be at least 150 and at most 193.
          ;; If in, the number must be at least 59 and at most 76.
          (let ((units-pos (- (string-length val) 2)))
            (let ((units (substring val units-pos))
                  (val (string->number (substring val 0 units-pos))))
              (or (and (string=? "cm" units)
                       (<= 150 val)
                       (>= 193 val))
                  (and (string=? "in" units)
                       (<= 59 val)
                       (>= 76 val))))))
         ((string=? attr "hcl")
          (let ((list-val (string->list val)))
            (and (char=? #\# (car list-val)) ;; first char is #
                 (= 7 (length list-val))
                 ;; each char is a hex char
                 (every (lambda (c)
                          (any (lambda (hex)
                                 (char=? hex c))
                               '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                                 #\a #\b #\c #\d #\e #\f)))
                        (cdr list-val)))))
         ((string=? attr "ecl")
          (list? (member val '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))
         ((string=? attr "pid")
          (= 9 (length (string->list val))))
         ;; unknown rule
         (else #t)))))


(apply-rule (cons "byr" "1980"))

(apply-rule (cons "pid" "000012345"))

(apply-rule (cons "ecl" "amb"))

(apply-rule (cons "hcl" "#123456"))

(apply-rule (cons "hgt" "64in"))


(define (strict-valid-passport? passport)
  (every (lambda (attr) (apply-rule (assoc attr passport))) required-attributes))

(assert "test part 2" (= 2 (count strict-valid-passport? example-batch)))

(format #t "Part 2: ~d\n" (->> (read-file-contents "day4-input")
                               (normalize-input)
                               (map string->passport)
                               (count strict-valid-passport?)))
