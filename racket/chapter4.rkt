#lang dracula

;; Load the J-Bob language:
(include-book "j-bob-lang" :dir :teachpacks)

;; Load J-Bob, our little proof assistant:
(include-book "j-bob" :dir :teachpacks)

;; Chapter 3

(defun defun.pair ()
  (J-Bob/define
   (prelude)
   '(((defun pair (x y)
        (cons x (cons y '())))
      nil))))

(defun defun.first-of ()
  (J-Bob/define
   (defun.pair)
   '(((defun first-of (x)
        (car x))
      nil))))

(defun defun.second-of ()
  (J-Bob/define
   (defun.first-of)
   '(((defun second-of (x)
        (car (cdr x)))
      nil))))

(defun dethm.first-of-pair ()
  (J-Bob/define
   (defun.second-of)
   '(((dethm first-of-pair (a b)
             (equal (first-of (pair a b)) a))
      nil
      ((1 1) (pair a b))
      ((1) (first-of (cons a (cons b 'nil))))
      ((1) (car/cons a (cons b 'nil)))
      (() (equal-same a))))))

(defun dethm.second-of-pair ()
  (J-Bob/define
   (dethm.first-of-pair)
   '(((dethm second-of-pair (a b)
             (equal (second-of (pair a b)) b))
      nil
      ((1) (second-of (pair a b)))
      ((1 1 1) (pair a b))
      ((1 1) (cdr/cons a (cons b '())))
      ((1) (car/cons b '()))
      (() (equal-same b))))))

(defun defun.in-pair? ()
  (J-Bob/define
   (dethm.second-of-pair)
   '(((defun in-pair? (xs)
        (if (equal (first-of xs) '?)
            't
            (equal (second-of xs) '?)))
      nil))))

(defun dethm.in-first-of-pair ()
  (J-Bob/define
   (defun.in-pair?)
   '(((dethm in-first-of-pair (b)
             (equal (in-pair? (pair '? b)) 't))
      nil
      ((1 1) (pair '? b))
      ((1) (in-pair? (cons '? (cons b '()))))
      ((1 Q 1) (first-of (cons '? (cons b '()))))
      ((1 Q 1) (car/cons '? (cons b '())))
      ((1 Q) (equal-same '?))
      ((1) (if-true 't (equal (second-of (cons '? (cons b '()))) '?)))
      (() (equal-same 't))))))

(defun dethm.in-second-of-pair ()
  (J-Bob/define
   (dethm.in-first-of-pair)
   '(((dethm in-second-of-pair (a)
             (equal (in-pair? (pair a '?)) 't))
      nil
      ((1 1) (pair a '?))
      ((1) (in-pair? (cons a (cons '? '()))))
      ((1 Q 1) (first-of (cons a (cons '? '()))))
      ((1 Q 1) (car/cons a (cons '? '())))
      ((1 E 1) (second-of (cons a (cons '? '()))))
      ((1 E 1 1) (cdr/cons a (cons '? '())))
      ((1 E 1) (car/cons '? '()))
      ((1 E) (equal-same '?))
      ((1) (if-same (equal a '?) 't))
      (() (equal-same 't))))))

;; Chapter 4

(defun defun.list0 ()
  (J-Bob/define
   (dethm.in-second-of-pair)
   '(((defun list0? (x)
        (equal x '()))
      nil))))

(defun defun.list1 ()
  (J-Bob/define
   (defun.list0)
   '(((defun list1? (x)
        (if (atom x) 'nil (list0? (cdr x))))
      nil))))

(defun defun.list2 ()
  (J-Bob/define
   (defun.list1)
   '(((defun list2? (x)
        (if (atom x) 'nil (list1? (cdr x))))
      nil))))

(defun dethm.contradiction ()
  (J-Bob/prove
   (list-extend
    (prelude)
    '(defun partial (x)
       (if (partial x) 'nil 't)))
   '(((dethm contradiction () 'nil)
      nil
      (() (if-same (partial x) 'nil))
      ((A) (if-nest-A (partial x) 'nil 't))
      ((E) (if-nest-E (partial x) 't 'nil))
      ((A Q) (partial x))
      ((E Q) (partial x))
      ((A Q) (if-nest-A (partial x) 'nil 't))
      ((E Q) (if-nest-E (partial x) 'nil 't))
      ((A) (if-false 'nil 't))
      ((E) (if-true 't 'nil))
      (() (if-same (partial x) 't))))))

(defun defun.list ()
  (J-Bob/define
   (defun.list2)
   '(((defun list? (x)
        (if (atom x)
            (equal x '())
            (list? (cdr x))))
      (size x)
      ((Q) (natp/size x))
      (() (if-true (if (atom x) 't (< (size (cdr x)) (size x))) 'nil))
      ((E) (size/cdr x))
      (() (if-same (atom x) 't))))))

(defun defun.sub ()
  (J-Bob/define
   (defun.list)
   '(((defun sub (x y)
        (if (atom y)
            (if (equal y '?)
                x
                y)
            (cons (sub x (car y))
                  (sub x (cdr y)))))
      (size y)
      ((Q) (natp/size y))
      (() (if-true (if (atom y)
                       't
                       (if (< (size (car y)) (size y))
                           (< (size (cdr y)) (size y))
                           'nil))
                   'nil))
      ((E Q) (size/car y))
      ((E A) (size/cdr y))
      ((E) (if-true 't 'nil))
      (() (if-same (atom y) 't))))))