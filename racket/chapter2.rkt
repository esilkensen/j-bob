#lang dracula

;; Load the J-Bob language:
(include-book "j-bob-lang" :dir :teachpacks)

;; Load J-Bob, our little proof assistant:
(include-book "j-bob" :dir :teachpacks)

(defun chapter2.example1 ()
  (J-Bob/step
   (prelude)
   '(if (car (cons a b)) c c)
   '(((Q) (car/cons a b))
     (() (if-same a c))
     (() (if-same (if (equal a 't)
                      (if (equal 'nil 'nil) a b)
                      (equal 'or (cons 'black '(coffee))))
                  c))
     ((Q E 2) (cons 'black '(coffee)))
     ((Q A Q) (equal-same 'nil))
     ((Q A) (if-true a b))
     ((Q A) (equal-if a 't)))))
(chapter2.example1)
;; ==> '(if (if (equal a 't) 't (equal 'or '(black coffee))) c c)

(defun chapter2.example2 ()
  (J-Bob/step
   (prelude)
   '(if (atom (car a))
        (if (equal (car a) (cdr a))
            'hominy
            'grits)
        (if (equal (cdr (car a)) '(hash browns))
            (cons 'ketchup (car a))
            (cons 'mustard (car a))))
   '(((E A 2) (cons/car+cdr (car a)))
     ((E A 2 2) (equal-if (cdr (car a)) '(hash browns))))))
(chapter2.example2)
;; ==> '(if (atom (car a))
;;          (if (equal (car a) (cdr a))
;;              'hominy
;;              'grits)
;;          (if (equal (cdr (car a)) '(hash browns))
;;              (cons 'ketchup (cons (car (car a)) '(hash browns)))
;;              (cons 'mustard (car a))))

(defun chapter2.example3 ()
  (J-Bob/step
   (prelude)
   '(cons 'statement
          (cons
           (if (equal a 'question)
               (cons n '(answer))
               (cons n '(else)))
           (if (equal a 'question)
               (cons n '(other answer))
               (cons n '(other else)))))
   '(((2) (if-same (equal a 'question)
                   (cons
                    (if (equal a 'question)
                        (cons n '(answer))
                        (cons n '(else)))
                    (if (equal a 'question)
                        (cons n '(other answer))
                        (cons n '(other else))))))
     ((2 A 1) (if-nest-A (equal a 'question)
                         (cons n '(answer))
                         (cons n '(else))))
     ((2 E 1) (if-nest-E (equal a 'question)
                         (cons n '(answer))
                         (cons n '(else))))
     ((2 A 2) (if-nest-A (equal a 'question)
                         (cons n '(other answer))
                         (cons n '(other else))))
     ((2 E 2) (if-nest-E (equal a 'question)
                         (cons n '(other answer))
                         (cons n '(other else)))))))
(chapter2.example3)
;; ==> '(cons 'statement
;;            (if (equal a 'question),
;;                (cons (cons n '(answer)) (cons n '(other answer)))
;;                (cons (cons n '(else)) (cons n '(other else)))))
