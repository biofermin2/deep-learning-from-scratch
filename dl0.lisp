;;;; dl0.lisp
(defpackage #:dl0
  (:use #:cl))
(in-package #:dl0)

;; p25
;; (defun and-gate (x1 x2)
;;   (let* ((w1 0.5)
;; 	(w2 0.5)
;; 	(theta 0.7)
;; 	(tmp (+ (* x1 w1) (* x2 w2))))
;;     (if (<= tmp theta)
;; 	0
;;       1)))                              ; =>AND-GATE 


;; (and-gate 0 0)                          ; =>0 
;; (and-gate 1 0)                          ; =>0 
;; (and-gate 0 1)                          ; =>0 
;; (and-gate 1 1)                          ; =>1 

;; #1
;; (defun nand-gate (x1 x2)
;;   (let* ((w1 -0.5)
;; 	 (w2 -0.5)
;; 	 (theta -0.7)
;; 	 (tmp (+ (* x1 w1) (* x2 w2))))
;;     (if (<= tmp theta)
;; 	1
;; 	0)))

;; #2
;; (defun nand-gate (x1 x2)
;;   (if (zerop (and-gate x1 x2))
;;       1
;;       0))				; =>NAND-GATE 

;; (nand-gate 0 0)				; =>1 
;; (nand-gate 1 0)				; =>1 
;; (nand-gate 0 1)				; =>1 
;; (nand-gate 1 1)				; =>0 

;; p27
(defun v* (v1 v2)
  "２つのベクターの積を求める"
  (vector (* (aref v1 0) (aref v2 0))
	  (* (aref v1 1) (aref v2 1)))) ; =>V* 

;; (v* #(0 1) #(0.5 0.5))			; => #(0.0 0.5)
;
(defun vec+ (v)
  "ベクターの成分同士の和を求める"
	 (+ (aref v 0) (aref v 1)))     ; =>VEC+ 

;; (vec+ #(0.0 0.5))                       ; =>0.5 

(defun and-gate (x1 x2)
  (let* ((x (vector x1 x2))
	 (w #(0.5 0.5))
	 (b -0.7)
	 (tmp (+ (vec+ (v* w x)) b)))
    (if (<= tmp 0)
	0
	1)))				; =>AND-GATE 

;; (and-gate 0 0)				; =>0 
;; (and-gate 0 1)				; =>0 
;; (and-gate 1 0)				; =>0 
;; (and-gate 1 1)				; =>1 

;; (defun nand-gate (x1 x2)
;;   (let* ((x (vector x1 x2))
;; 	 (w #(-0.5 -0.5))
;; 	 (b 0.7)
;; 	 (tmp (+ (vec+ (v* x w))
;; 		 b)))
;;     (if (<= tmp 0)
;; 	0
;; 	1)))				; => NAND-GATE
;;#２
(defun nand-gate (x1 x2)
  (if (zerop (and-gate x1 x2))
      1
      0))                               ; =>NAND-GATE 

;; (nand-gate 0 0)				; =>1 
;; (nand-gate 0 1)				; =>1 
;; (nand-gate 1 0)				; =>1 
;; (nand-gate 1 1)				; =>0 

(defun or-gate (x1 x2)
  (let* ((x (vector x1 x2))
         (w #(0.5 0.5))
         (b -0.2)
         (tmp (+ (vec+ (v* w x))
                 b)))
    (if (<= tmp 0)
        0
        1)))                            ; =>OR-GATE 

;; (or-gate 0 0)                           ; => 0
;; (or-gate 0 1)                           ; => 1
;; (or-gate 1 0)                           ; => 1
;; (or-gate 1 1)                           ; => 1

(defun xor-gate (x1 x2)
  (let* ((s1 (nand-gate x1 x2))
         (s2 (or-gate x1 x2))
         (y (and-gate s1 s2)))
    y))                                 ; => XOR-GATE

;; (xor-gate 0 0)                          ; =>0 
;; (xor-gate 0 1)                          ; =>1 
;; (xor-gate 1 0)                          ; =>1 
;; (xor-gate 1 1)                          ; =>0 

;; p42

(defmethod sigmoid ((x number))
  (/ 1 (+ 1 (exp (* x -1)))))           ; =>#<STANDARD-METHOD COMMON-LISP-USER::SIGMOID (NUMBER) {10031B4143}> 

;; (sigmoid 1.0)                           ; =>0.7310586 
;; (sigmoid 2.0)                           ; =>0.880797 

(defmethod sigmoid ((v vector))
  (let* ((lst (coerce v 'list))
         (v (mapcar #'(lambda (x) (/ 1 (+ 1 (exp (* x -1))))) lst)))
    (apply #'vector v)))                ; =>#<STANDARD-METHOD COMMON-LISP-USER::SIGMOID (VECTOR) {1003B872F3}> 

;; (sigmoid #(-1 0 1))                     ; =>#(0.26894143 0.5 0.7310586) 
;; (sigmoid #(1.0 2.0))                    ; => #(0.7310586 0.880797)

(defmethod step-func ((x number))
  (if (> x 0)
      1
      0))                               ; =>#<STANDARD-METHOD COMMON-LISP-USER::STEP-FUNC (NUMBER) {1003E67403}> 

;; (step-func 1)                           ; =>1 
;; (step-func 0)                           ; =>0 
;; (step-func -1)                          ; =>0 


(defmethod step-func ((v vector))
  (let* ((lst (coerce v 'list))
         (v (mapcar #'(lambda (x) (if (> x 0) 1 0)) lst)))
    (values (apply #'vector v)
            (mapcar #'(lambda (x) (if (<= x 0) nil t)) v)))) ; =>#<STANDARD-METHOD COMMON-LISP-USER::STEP-FUNC (VECTOR) {1005350F83}> 

;; (step-func #(-1 1 2))                   ; =>#(0 1 1)
;; (NIL T T) 

;;p52
(defun relu (x)
  (max 0 x))                            ; =>RELU 

;; (relu 3)                                ; => 3
;; (relu -3)                               ; => 0
;; (relu 0)                                ; => 0
;; (relu 0.5)                              ; => 0.5


(defun ndim (arry)
  "配列の次元数を求める関数をpython風に定義"
  (array-rank arry))                    ; =>NDIM 

;; (ndim #(1 2 3 4))                       ; => 1


;; (setf A (make-array '(2 3) :element-type 'fixnum :initial-contents '((1 2 3) (4 5 6)))) ; =>#2A((1 2 3) (4 5 6)) 
;; (setf B (make-array '(3 2) :element-type 'fixnum :initial-contents '((1 2) (3 4) (5 6)))) ; => #2A((1 2) (3 4) (5 6))

(defun shape (mat)
  (array-dimensions mat))               ; => SHAPE

;; (shape A)                               ; => (2 3)
;; (shape B)                               ; => (3 2)

(defun dot-p (m1 m2)
  (eq (car (last (shape m1))) (car (shape m2)))) ; => DOT-P

;; (dot-p A B)                             ; => T

(defun result-shape (m1 m2)
  (list (car (shape m1)) (car (last (shape m2))))) ; => RESULT-SHAPE

;; (result-shape A B)                                 ; => (2 2)

