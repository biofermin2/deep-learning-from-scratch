;;p52
(defun relu (x)
  (max 0 x))                            ; =>RELU 

(relu 3)                                ; => 3
(relu -3)                               ; => 0
(relu 0)                                ; => 0
(relu 0.5)                              ; => 0.5


(array-dimensions #(1 2 3 4))           ; => (4)
(length #(1 2 3 4))                     ; => 4
(array-rank #(1 2 3 4))                 ; => 1

(defun ndim (arry)
  "配列の次元数を求める関数をpython風に定義"
  (array-rank arry))                    ; =>NDIM 

(ndim #(1 2 3 4))                       ; => 1
(defun shape (arry)
  (array-dimensions arry))              ; => SHAPE
(shape #(1 2 3 4))                      ; =>(4) 
(shape #((1 2) (3 4)))                  ; =>(2) 


(setq array1 (make-array '(2 3) :initial-element 0)) ; =>#2A((0 0 0) (0 0 0)) 
(array-rank array1)                                  ; => 2
    (array-dimensions array1)                            ; => (2 3)
;; (coerce '((1 2) (3 4) (5 6)) 'array)    ; => 
;;(make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))
(setq array2 (make-array '(3 2) :initial-contents '((1 2) (3 4) (5 6)))) ; =>#2A((1 2) (3 4) (5 6)) 
(array-rank array2)                     ; => 2
(array-dimensions array2)               ; => (3 2)
(setq array3 (make-array '(2 2 3) :initial-element 0)) ; => #3A(((0 0 0) (0 0 0)) ((0 0 0) (0 0 0)))
(array-rank array3)                                    ; =>3 
(array-dimensions array3)                              ; =>(2 2 3) 
(shape array2)                                         ; => (3 2)
(ndim array2)                                          ; => 2
