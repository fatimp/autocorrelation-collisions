(in-package :autocorrelation-collisions)

;; Statically typed wrappers around FFTPACK5:
(declaim (inline rfft))
(sera:-> rfft ((simple-array single-float (*)))
         (values (simple-array (complex single-float) (*)) &optional))
(defun rfft (array)
  (fft:rfft array))

(declaim (inline inverse-rfft))
(sera:-> inverse-rfft ((simple-array (complex single-float) (*)) alex:positive-fixnum)
         (values (simple-array single-float (*)) &optional))
(defun inverse-rfft (array length)
  (fft:inverse-rfft array length))

(declaim (inline map*))
(defun map* (fn array &rest arrays)
  (apply #'map-into
         (make-array (length array)
                     :element-type (array-element-type array))
         fn array arrays))

(sera:-> bitstring->floats ((simple-array bit (*)))
         (values (simple-array single-float (*)) &optional))
(defun bitstring->floats (array)
  (declare (optimize (speed 3)))
  (map '(vector single-float)
       #'float array))

(sera:-> autocorrelation ((simple-array bit (*)))
         (values (simple-array fixnum (*)) &optional))
(defun autocorrelation (array)
  "Calculate periodic autocorrelation for a bit vector ARRAY. For example:
CL-USER> (autocorrelation-collisions:autocorrelation #*100011)
#(3 2 1 0 1 2)"
  (declare (optimize (speed 3)))
  (let ((fft (rfft (bitstring->floats array))))
    (map '(vector fixnum)
         #'round
         (inverse-rfft
          (map* (lambda (x y)
                  (declare (type (complex single-float) x y))
                  (* (length array) x y))
                fft (map* #'conjugate fft))
          (length array)))))

(sera:-> int->bit-array (alex:non-negative-fixnum
                         alex:positive-fixnum)
         (values (simple-array bit (*)) &optional))
(defun int->bit-array (x n)
  "Convert an integer X to a bit array of length N. The following must
hold: 0 ≤ X < 2^N."
  (declare (optimize (speed 3)))
  (let ((array (make-array n :element-type 'bit)))
    (loop for i below n do
      (setf (aref array i)
            (ldb (byte 1 i) x)))
    array))

(sera:-> find-collisions ((integer 1 60))
         (values hash-table &optional))
(defun find-collisions (bits)
  "Calculate autocorrelation for all bit sequences of length BITS and
return a hash table with autocorrelations as keys and lists of
corresponding bit sequences as values."
  (declare (optimize (speed 3)))
  (loop with table = (make-hash-table :test #'equalp)
        for i below (ash 1 bits)
        for poly = (int->bit-array i bits)
        for autocorrelation = (autocorrelation poly) do
          (push poly (gethash autocorrelation table))
        finally (return table)))

(sera:-> circular-shift-p ((simple-array bit (*))
                           (simple-array bit (*)))
         (values boolean &optional))
(defun circular-shift-p (x y)
  "Return T if the bit sequence X is obtained by circular-shifting Y,
NIL otherwise."
  (declare (optimize (speed 3)))
  (let ((ft-x (rfft (bitstring->floats x)))
        (ft-y (rfft (bitstring->floats y))))
    (= (reduce #'+ x)
       ;; X and Y have the same number of 1s
       (reduce #'+ y)
       ;; And this number = the maximal element of X⋆Y
       (reduce #'max
               (map '(vector fixnum)
                    #'round
                    (inverse-rfft
                     (map*
                      (lambda (%x %y) (* %x %y (length x)))
                      ft-x (map-into ft-y #'conjugate ft-y))
                     (length x)))))))

(sera:-> trivial-pair-p ((simple-array bit (*))
                         (simple-array bit (*)))
         (values boolean &optional))
(defun trivial-pair-p (x y)
  "Return T if X is either circular-shifted Y or circular-shifted
reverse of Y, NIL otherwise."
  (declare (optimize (speed 3)))
  (or (circular-shift-p x y)
      (circular-shift-p x (reverse y))))

(sera:-> filter-non-trivial (hash-table)
         (values hash-table &optional))
(defun filter-non-trivial (table)
  (declare (optimize (speed 3)))
  (maphash
   (lambda (key value)
     (setf (gethash key table)
           (remove-duplicates
            value :test #'trivial-pair-p)))
   table)
  table)

(sera:-> non-trivial-collisions (alex:positive-fixnum)
         (values list &optional))
(defun non-trivial-collisions (n)
  "Return all lists of bit vectors of length N which have the same
autocorrelation, but cannot be trivially obtained from each other by
circular-shifting and reversion."
  (let ((table (filter-non-trivial
                (find-collisions n)))
        acc)
    (maphash
     (lambda (key value)
       (declare (ignore key))
       (if (> (length value) 1)
           (push value acc)))
     table)
    acc))

;; To check if the number of non-trivial bit vectors correlates with
;; number of divisors of the bit vector's length.

(sera:-> divisors (alex:positive-fixnum)
         (values list &optional))
(defun divisors (n)
  "Return all M_i which are divisors of N so that 1 ≤ M_i ≤ N."
  (declare (optimize (speed 3)))
  (labels ((%divisors (i acc)
             (declare (type alex:positive-fixnum i))
             (if (> i n) acc
                 (%divisors
                  (1+ i)
                  (if (zerop (rem n i))
                      (cons i acc) acc)))))
    (%divisors 1 nil)))
