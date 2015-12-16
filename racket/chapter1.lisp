;; Load the J-Bob language:
(include-book "j-bob-lang" :dir :teachpacks)

;; Load J-Bob, our little proof assistant:
(include-book "j-bob" :dir :teachpacks)

(defun chapter1.example1 ()
  (J-Bob/step
   (prelude)
   '(car (cons 'ham '(eggs)))
   '(((1) (cons 'ham '(eggs)))
     (() (car '(ham eggs))))))

(defun chapter1.example2 ()
  (J-Bob/step
   (prelude)
   '(atom '())
   '((() (atom '())))))

(defun chapter1.example3 ()
  (J-Bob/step
   (prelude)
   '(atom (cons 'ham '(eggs)))
   '(((1) (cons 'ham '(eggs)))
     (() (atom '(ham eggs))))))

(defun chapter1.example4 ()
  (J-Bob/step
   (prelude)
   '(atom (cons a b))
   '((() (atom/cons a b)))))

(defun chapter1.example5 ()
  (J-Bob/step
   (prelude)
   '(equal 'flapjack (atom (cons a b)))
   '(((2) (atom/cons a b))
     (() (equal 'flapjack 'nil)))))

(defun chapter1.example6 ()
  (J-Bob/step
   (prelude)
   '(atom (cdr (cons (car (cons p q)) '())))
   '(((1 1 1) (car/cons p q))
     ((1) (cdr/cons p '()))
     (() (atom '())))))

(defun chapter1.example7 ()
  (J-Bob/step
   (prelude)
   '(atom (cdr (cons (car (cons p q)) '())))
   '(((1) (cdr/cons (car (cons p q)) '()))
     (() (atom '())))))

(defun chapter1.example8 ()
  (J-Bob/step
   (prelude)
   '(car (cons (equal (cons x y) (cons x y))
               '(and crumpets)))
   '(((1 1) (equal-same (cons x y)))
     ((1) (cons 't '(and crumpets)))
     (() (car '(t and crumpets))))))

(defun chapter1.example9 ()
  (J-Bob/step
   (prelude)
   '(equal (cons x y) (cons 'bagels '(and lox)))
   '((() (equal-swap (cons x y) (cons 'bagels '(and lox)))))))

(defun chapter1.example10 ()
  (J-Bob/step
   (prelude)
   '(cons y (equal (car (cons (cdr x) (car y)))
                   (equal (atom x) 'nil)))
   '(((2 1) (car/cons (cdr x) (car y))))))

(defun chapter1.example11 ()
  (J-Bob/step
   (prelude)
   '(cons y (equal (car (cons (cdr x) (car y))) (equal (atom x) 'nil)))
   '(((2 1) (car/cons (car (cons (cdr x) (car y))) '(oats)))
     ((2 2 2) (atom/cons (atom (cdr (cons a b))) (equal (cons a b) c)))
     ((2 2 2 1 1 1) (cdr/cons a b))
     ((2 2 2 1 2) (equal-swap (cons a b) c)))))

(defun chapter1.example12 ()
  (J-Bob/step
   (prelude)
   '(atom (car (cons (car a) (cdr b))))
   '(((1) (car/cons (car a) (cdr b))))))

(chapter1.example1)  ;; 'ham
(chapter1.example2)  ;; 't
(chapter1.example3)  ;; 'nil
(chapter1.example4)  ;; 'nil
(chapter1.example5)  ;; 'nil
(chapter1.example6)  ;; 't
(chapter1.example7)  ;; 't
(chapter1.example8)  ;; 't
(chapter1.example9)  ;; (equal (cons 'bagels '(and lox)) (cons x y))
(chapter1.example10) ;; (cons y (equal (cdr x) (equal (atom x) 'nil)))
(chapter1.example11) ;; (cons y (equal ... (equal (atom x) (atom ...))))
(chapter1.example12) ;; (atom (car a))