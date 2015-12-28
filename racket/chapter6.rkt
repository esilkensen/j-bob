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

;; Chapter 5

(defun defun.memb? ()
  (J-Bob/define
   (defun.sub)
   '(((defun memb? (xs)
        (if (atom xs)
            'nil
            (if (equal (car xs) '?)
                't
                (memb? (cdr xs)))))
      (size xs)
      ((Q) (natp/size xs))
      (() (if-true (if (atom xs)
                       't
                       (if (equal (car xs) '?)
                           't
                           (< (size (cdr xs)) (size xs))))
                   'nil))
      ((E E) (size/cdr xs))
      ((E) (if-same (equal (car xs) '?) 't))
      (() (if-same (atom xs) 't))))))

(defun defun.remb ()
  (J-Bob/define
   (defun.memb?)
   '(((defun remb (xs)
        (if (atom xs)
            '()
            (if (equal (car xs) '?)
                (remb (cdr xs))
                (cons (car xs)
                      (remb (cdr xs))))))
      (size xs)
      ((Q) (natp/size xs))
      (() (if-true (if (atom xs)
                       't
                       (< (size (cdr xs)) (size xs)))
                   'nil))
      ((E) (size/cdr xs))
      (() (if-same (atom xs) 't))))))

(defun dethm.memb?/remb0 ()
  (J-Bob/define
   (defun.remb)
   '(((dethm memb?/remb0 ()
             (equal (memb? (remb '())) 'nil))
      nil
      ((1 1) (remb '()))
      ((1 1 Q) (atom '()))
      ((1 1) (if-true '() (if (equal (car '()) '?)
                              (remb (cdr '()))
                              (cons (car '())
                                    (remb (cdr '()))))))
      ((1) (memb? '()))
      ((1 Q) (atom '()))
      ((1) (if-true 'nil
                    (if (equal (car '()) '?)
                        't
                        (memb? (cdr '())))))
      (() (equal-same 'nil))))))

(defun dethm.memb?/remb1 ()
  (J-Bob/define
   (dethm.memb?/remb0)
   '(((dethm memb?/remb1 (x1)
             (equal (memb? (remb (cons x1 '()))) 'nil))
      nil
      ((1 1) (remb (cons x1 '())))
      ((1 1 Q) (atom/cons x1 '()))
      ((1 1) (if-false '() (if (equal (car (cons x1 '())) '?)
                               (remb (cdr (cons x1 '())))
                               (cons (car (cons x1 '()))
                                     (remb (cdr (cons x1 '())))))))
      ((1 1 Q 1) (car/cons x1 '()))
      ((1 1 A 1) (cdr/cons x1 '()))
      ((1 1 E 1) (car/cons x1 '()))
      ((1 1 E 2 1) (cdr/cons x1 '()))
      ((1) (if-same (equal x1 '?)
                    (memb? (if (equal x1 '?)
                               (remb '())
                               (cons x1 (remb '()))))))
      ((1 A 1) (if-nest-A (equal x1 '?)
                          (remb '())
                          (cons x1 (remb '()))))
      ((1 E 1) (if-nest-E (equal x1 '?)
                          (remb '())
                          (cons x1 (remb '()))))
      ((1 A) (memb?/remb0))
      ((1 E) (memb? (cons x1 (remb '()))))
      ((1 E Q) (atom/cons x1 (remb '())))
      ((1 E) (if-false 'nil
                       (if (equal (car (cons x1 (remb '()))) '?)
                           't
                           (memb? (cdr (cons x1 (remb '())))))))
      ((1 E Q 1) (car/cons x1 (remb '())))
      ((1 E E 1) (cdr/cons x1 (remb '())))
      ((1 E E) (memb?/remb0))
      ((1 E) (if-nest-E (equal x1 '?) 't 'nil))
      ((1) (if-same (equal x1 '?) 'nil))
      (() (equal-same 'nil))))))

(defun dethm.memb?/remb2 ()
  (J-Bob/define
   (dethm.memb?/remb1)
   '(((dethm memb?/remb2 (x1 x2)
             (equal (memb? (remb (cons x2 (cons x1 '())))) 'nil))
      nil
      ((1 1) (remb (cons x2 (cons x1 '()))))
      ((1 1 Q) (atom/cons x2 (cons x1 '())))
      ((1 1) (if-false 'nil
                       (if (equal (car (cons x2 (cons x1 '()))) '?)
                           (remb (cdr (cons x2 (cons x1 '()))))
                           (cons (car (cons x2 (cons x1 '())))
                                 (remb (cdr (cons x2 (cons x1 '()))))))))
      ((1 1 Q 1) (car/cons x2 (cons x1 '())))
      ((1 1 A 1) (cdr/cons x2 (cons x1 '())))
      ((1 1 E 1) (car/cons x2 (cons x1 '())))
      ((1 1 E 2 1) (cdr/cons x2 (cons x1 '())))
      ((1) (if-same (equal x2 '?)
                    (memb? (if (equal x2 '?)
                               (remb (cons x1 '()))
                               (cons x2 (remb (cons x1 '())))))))
      ((1 A 1) (if-nest-A (equal x2 '?)
                          (remb (cons x1 '()))
                          (cons x2 (remb (cons x1 '())))))
      ((1 E 1) (if-nest-E (equal x2 '?)
                          (remb (cons x1 '()))
                          (cons x2 (remb (cons x1 '())))))
      ((1 A) (memb?/remb1 x1))
      ((1 E) (memb? (cons x2 (remb (cons x1 '())))))
      ((1 E Q) (atom/cons x2 (remb (cons x1 '()))))
      ((1 E) (if-false 'nil
                       (if (equal (car (cons x2 (remb (cons x1 '())))) '?)
                           't
                           (memb? (cdr (cons x2 (remb (cons x1 '()))))))))
      ((1 E Q 1) (car/cons x2 (remb (cons x1 '()))))
      ((1 E E 1) (cdr/cons x2 (remb (cons x1 '()))))
      ((1 E E) (memb?/remb1 x1))
      ((1 E) (if-nest-E (equal x2 '?) 't 'nil))
      ((1) (if-same (equal x2 '?) 'nil))
      (() (equal-same 'nil))))))

;; Chapter 6

(defun dethm.memb?/remb ()
  (J-Bob/define
   (dethm.memb?/remb2)
   '(((dethm memb?/remb (xs)
             (equal (memb? (remb xs)) 'nil))
      (list-induction xs)
      ((A 1 1) (remb xs))
      ((A 1 1) (if-nest-A (atom xs)
                          '()
                          (if (equal (car xs) '?)
                              (remb (cdr xs))
                              (cons (car xs) (remb (cdr xs))))))
      ((A 1) (memb? '()))
      ((A 1 Q) (atom '()))
      ((A 1) (if-true 'nil
                      (if (equal (car '()) '?)
                          't
                          (memb? (cdr '())))))
      ((A) (equal-same 'nil))
      ((E A 1 1) (remb xs))
      ((E A 1 1) (if-nest-E (atom xs)
                            '()
                            (if (equal (car xs) '?)
                                (remb (cdr xs))
                                (cons (car xs) (remb (cdr xs))))))
      ((E A 1) (if-same (equal (car xs) '?)
                        (memb? (if (equal (car xs) '?)
                                   (remb (cdr xs))
                                   (cons (car xs) (remb (cdr xs)))))))
      ((E A 1 A 1) (if-nest-A (equal (car xs) '?)
                              (remb (cdr xs))
                              (cons (car xs) (remb (cdr xs)))))
      ((E A 1 E 1) (if-nest-E (equal (car xs) '?)
                              (remb (cdr xs))
                              (cons (car xs) (remb (cdr xs)))))
      ((E A 1 E) (memb? (cons (car xs) (remb (cdr xs)))))
      ((E A 1 E Q) (atom/cons (car xs) (remb (cdr xs))))
      ((E A 1 E) (if-false 'nil
                           (if (equal (car (cons (car xs)
                                                 (remb (cdr xs))))
                                      '?)
                               't
                               (memb? (cdr (cons (car xs)
                                                 (remb (cdr xs))))))))
      ((E A 1 E Q 1) (car/cons (car xs) (remb (cdr xs))))
      ((E A 1 E E 1) (cdr/cons (car xs) (remb (cdr xs))))
      ((E A 1 E) (if-nest-E (equal (car xs) '?)
                            't
                            (memb? (remb (cdr xs)))))
      ((E A 1) (if-same (equal (car xs) '?) (memb? (remb (cdr xs)))))
      ((E A 1) (equal-if (memb? (remb (cdr xs))) 'nil))
      ((E A) (equal-same 'nil))
      ((E) (if-same (equal (memb? (remb (cdr xs))) 'nil) 't))
      (() (if-same (atom xs) 't))))))