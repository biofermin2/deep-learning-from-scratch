;; p25
(defun and-gate (x1 x2)
  (let* ((w1 0.5)
	(w2 0.5)
	(theta 0.7)
	(tmp (+ (* x1 w1) (* x2 w2))))
    (if (<= tmp theta)
	0
      1)))                              ; =>AND-GATE 

(and-gate 0 0)                          ; =>0 
(and-gate 1 0)                          ; =>0 
(and-gate 0 1)                          ; =>0 
(and-gate 1 1)                          ; =>1 

;; (defun nand-gate (x1 x2)
;;   (let* ((w1 -0.5)
;; 	 (w2 -0.5)
;; 	 (theta -0.7)
;; 	 (tmp (+ (* x1 w1) (* x2 w2))))
;;     (if (<= tmp theta)
;; 	1
;; 	0)))
(defun nand-gate (x1 x2)
  (if (zerop (and-gate x1 x2))
      1
      0))				; =>NAND-GATE 

(nand-gate 0 0)				; =>1 
(nand-gate 1 0)				; =>1 
(nand-gate 0 1)				; =>1 
(nand-gate 1 1)				; =>0 

(make-array 2)				; =>#(0 0) 
(vector 0 1)				; => #(0 1)
(vector 0.5 0.5)			; =>#(0.5 0.5) 
(aref #(0 1) 1)				; => 1
(aref #(0 1) 0)				; => 0

;; p27
(defun v* (v1 v2)
  (vector (* (aref v1 0) (aref v2 0))
	  (* (aref v1 1) (aref v2 1)))) ; =>V* 

(v* #(0 1) #(0.5 0.5))			; => #(0.0 0.5)
;; 配列の長さを調べる
(array-dimensions #(0 1))		; => (2)

(defun vec+ (v)
	 (+ (aref v 0) (aref v 1)))     ; =>VEC+ 

(sum-vec #(0.0 0.5))			; => 0.5

(defun and-gate (x1 x2)
  (let* ((x (vector x1 x2))
	 (w #(0.5 0.5))
	 (b -0.7)
	 (tmp (+ (vec+ (v* w x)) b)))
    (if (<= tmp 0)
	0
	1)))				; =>AND-GATE 

(and-gate 0 0)				; =>0 
(and-gate 0 1)				; =>0 
(and-gate 1 0)				; =>0 
(and-gate 1 1)				; =>1 

(defun nand-gate (x1 x2)
  (let* ((x (vector x1 x2))
	 (w #(-0.5 -0.5))
	 (b 0.7)
	 (tmp (+ (vec+ (v* x w))
		 b)))
    (if (<= tmp 0)
	0
	1)))				; => NAND-GATE
;;#２
(defun nand-gate (x1 x2)
  (if (zerop (and-gate x1 x2))
      1
      0))                               ; =>NAND-GATE 

(nand-gate 0 0)				; =>1 
(nand-gate 0 1)				; =>1 
(nand-gate 1 0)				; =>1 
(nand-gate 1 1)				; =>0 

(defun or-gate (x1 x2)
  (let* ((x (vector x1 x2))
         (w #(0.5 0.5))
         (b -0.2)
         (tmp (+ (vec+ (v* w x))
                 b)))
    (if (<= tmp 0)
        0
        1)))                            ; =>OR-GATE 

(or-gate 0 0)                           ; => 0
(or-gate 0 1)                           ; => 1
(or-gate 1 0)                           ; => 1
(or-gate 1 1)                           ; => 1

(defun xor-gate (x1 x2)
  (let* ((s1 (nand-gate x1 x2))
         (s2 (or-gate x1 x2))
         (y (and-gate s1 s2)))
    y))                                 ; => XOR-GATE

(xor-gate 0 0)                          ; =>0 
(xor-gate 0 1)                          ; =>1 
(xor-gate 1 0)                          ; =>1 
(xor-gate 1 1)                          ; =>0 

(signum 0)                              ; => 0
(signum 1)                              ; => 1
(signum -1)                             ; => -1
(signum 4)                              ; => 1
(signum -4)                             ; => -1
