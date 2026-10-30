(in-package #:cl-sml)

(defun make-portable-weak-eq-hash-table ()
  #+sbcl (make-hash-table :test #'eq :weakness :key)
  #-sbcl (make-hash-table :test #'eq))

;;; SML programs loop by tail recursion.  The compiler turns a function's
;;; calls to itself in tail position into jumps on every implementation; SBCL
;;; also merges the other tail calls, but only below DEBUG 3, so generated
;;; functions ask for SBCL's default debug level.
(defun portable-tail-call-declarations ()
  #+sbcl '((declare (optimize (debug 1))))
  #-sbcl nil)

(defun portable-sml-lambda-form (name arg body)
  #+sbcl (if name
             `(sb-int:named-lambda ,(format nil "sml:~A" name) (,arg)
                ,@(portable-tail-call-declarations)
                ,body)
             `(lambda (,arg) ,@(portable-tail-call-declarations) ,body))
  #-sbcl (progn name `(lambda (,arg) ,body)))

;;; SML reals are IEEE doubles whose operations never trap: overflow gives an
;;; infinity, invalid operations a NaN.  SBCL and ECL trap by default; ABCL
;;; only signals overflow and underflow from its transcendental functions.
(defmacro with-portable-float-traps-masked (&body body)
  #+sbcl `(sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :inexact)
            ,@body)
  #+ecl (let ((traps (gensym "TRAPS")))
          `(let ((,traps (ext:trap-fpe 'last nil)))
             (unwind-protect
                  (progn (ext:trap-fpe t nil) ,@body)
               (ext:trap-fpe ,traps t))))
  #+abcl (let ((overflow (gensym "OVERFLOW"))
               (underflow (gensym "UNDERFLOW")))
           `(let ((,overflow (sys:float-overflow-mode))
                  (,underflow (sys:float-underflow-mode)))
              (unwind-protect
                   (progn (sys:float-overflow-mode nil)
                          (sys:float-underflow-mode nil)
                          ,@body)
                (sys:float-overflow-mode ,overflow)
                (sys:float-underflow-mode ,underflow))))
  #-(or sbcl ecl abcl) `(progn ,@body))

(defun portable-float-nan ()
  #+ecl (ext:nan)
  #+sbcl (with-portable-float-traps-masked
          (- sb-ext:double-float-positive-infinity
             sb-ext:double-float-positive-infinity))
  #+(and (not ecl) (not sbcl))
  (let ((zero 0d0))
    (handler-case
        (with-portable-float-traps-masked (/ zero zero))
      (arithmetic-error (condition)
        (error "This Lisp cannot produce an IEEE NaN: ~A" condition)))))

(defun portable-float-nan-p (x)
  #+ecl (and (floatp x) (ext:float-nan-p x))
  #+sbcl (and (floatp x) (sb-ext:float-nan-p x))
  #+(and (not ecl) (not sbcl))
  (and (floatp x) (/= x x)))

(defun portable-float-infinite-p (x)
  (declare (ignorable x))
  #+ecl (and (floatp x) (ext:float-infinity-p x))
  #+sbcl (and (floatp x) (sb-ext:float-infinity-p x))
  #+(and (not ecl) (not sbcl))
  (and (floatp x)
       (not (portable-float-nan-p x))
       (or (> x most-positive-double-float)
           (< x (- most-positive-double-float)))))

(defun portable-float-infinity (negativep)
  #+ecl (if negativep ext:double-float-negative-infinity
                         ext:double-float-positive-infinity)
  #+sbcl (if negativep sb-ext:double-float-negative-infinity
                         sb-ext:double-float-positive-infinity)
  #+(and (not ecl) (not sbcl))
  (let ((zero 0d0))
    (or (handler-case
            (with-portable-float-traps-masked
              (/ (if negativep -1d0 1d0) zero))
          (arithmetic-error () nil))
      (if negativep (- most-positive-double-float)
          most-positive-double-float))))

(defun portable-command-line-arguments ()
  (uiop:command-line-arguments))

(defun portable-rational-to-double (q)
  "The double nearest to the rational Q, ties to even, or NIL when Q is
beyond the double range.  Hosts' COERCE and readers do not all round
correctly (ECL reads 1d23 one ulp too high), so this uses integer
arithmetic; only exactly representable values are handed to the host."
  (cond
    ((and (integerp q) (< (abs q) (expt 2 53)))
     (coerce q 'double-float))
    ((minusp q)
     (let ((magnitude (portable-rational-to-double (- q))))
       (and magnitude (- magnitude))))
    (t
     ;; 2^l <= q < 2^(l+1); the result has 53 significant bits, fewer when
     ;; it is subnormal.
     (let ((l (- (integer-length (numerator q)) (integer-length (denominator q)))))
       (when (< q (expt 2 l))
         (decf l))
       (let* ((e (max (- l 52) -1074))
              (n (round q (expt 2 e))))
         (when (<= (* n (expt 2 e)) most-positive-double-float)
           (with-portable-float-traps-masked
             (scale-float (coerce n 'double-float) e))))))))

(defun portable-flonum-to-digits (value)
  "Return K and DIGITS such that the positive double VALUE is 0.DIGITS * 10^K,
with DIGITS the shortest string that reads back as VALUE.  This is the
free-format algorithm of Burger and Dybvig, \"Printing Floating-Point
Numbers Quickly and Accurately\" (PLDI 1996).  It only uses integer
arithmetic, so every host prints the same digits; FORMAT's and the reader's
float conversions differ between implementations."
  (when (zerop value)
    (return-from portable-flonum-to-digits (values 0 "0")))
  (multiple-value-bind (f e) (integer-decode-float (coerce value 'double-float))
    ;; Subnormals have the minimum exponent; some hosts normalize them.
    (loop while (< e -1074)
          do (setf f (ash f -1) e (1+ e)))
    (let ((high-ok (evenp f))
          (low-ok (evenp f))
          (digits (make-string-output-stream))
          r s m+ m-)
      (labels ((generate (k)
                 (loop
                   (multiple-value-bind (d remainder) (truncate (* r 10) s)
                     (setf r remainder
                           m+ (* m+ 10)
                           m- (* m- 10))
                     (let ((tc1 (or (< r m-) (and low-ok (= r m-))))
                           (tc2 (or (> (+ r m+) s) (and high-ok (= (+ r m+) s)))))
                       (when (or tc1 tc2)
                         (write-char (digit-char (cond ((not tc1) (1+ d))
                                                       ((not tc2) d)
                                                       ((< (* r 2) s) d)
                                                       (t (1+ d))))
                                     digits)
                         (return (values k (get-output-stream-string digits))))
                       (write-char (digit-char d) digits)))))
               (scale ()
                 (let ((k 0))
                   (loop while (or (> (+ r m+) s) (and high-ok (= (+ r m+) s)))
                         do (setf s (* s 10))
                            (incf k))
                   (loop while (and (> r m-)
                                    (let ((x (* (+ r m+) 10)))
                                      (or (< x s) (and (not high-ok) (= x s)))))
                         do (setf r (* r 10)
                                  m+ (* m+ 10)
                                  m- (* m- 10))
                            (decf k))
                   (generate k))))
        (let ((hidden-bit-only (= f (expt 2 52))))
          (if (>= e 0)
              (let ((be (expt 2 e)))
                (if hidden-bit-only
                    (setf m- be m+ (* be 2) r (* f m+ 2) s 4)
                    (setf r (* f be 2) s 2 m+ be m- be)))
              (if (and hidden-bit-only (/= e -1074))
                  (setf m- 1 m+ 2 r (* f 4) s (* (expt 2 (- 1 e)) 2))
                  (setf r (* f 2) s (* (expt 2 (- e)) 2) m+ 1 m- 1))))
        (scale)))))
