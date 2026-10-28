(in-package #:cl-sml)

(defun make-portable-weak-eq-hash-table ()
  #+sbcl (make-hash-table :test #'eq :weakness :key)
  #-sbcl (make-hash-table :test #'eq))

(defun portable-sml-lambda-form (name arg body)
  #+sbcl (if name
             `(sb-int:named-lambda ,(format nil "sml:~A" name) (,arg) ,body)
             `(lambda (,arg) ,body))
  #-sbcl (progn name `(lambda (,arg) ,body)))

(defmacro with-portable-float-traps-masked (&body body)
  #+sbcl `(sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :inexact)
            ,@body)
  #-sbcl `(progn ,@body))

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

(defun portable-flonum-to-digits (value)
  "Return VALUE's shortest round-tripping decimal digits and decimal point.
The standard FORMAT G directive is used with increasing precision, then the
candidate is checked by reading it back as the same float format."
  (when (zerop value)
    (return-from portable-flonum-to-digits (values 0 "0")))
  (let* ((float-format (typecase value
                         (short-float 'short-float)
                         (single-float 'single-float)
                         (double-float 'double-float)
                         (long-float 'long-float)))
         (max-precision (case float-format
                          ((short-float single-float) 9)
                          (double-float 17)
                          (long-float 21)))
         (candidate
           (loop for precision from 1 to max-precision
                 for text = (string-trim '(#\Space #\Tab #\Newline)
                                         (format nil "~,vG" precision value))
                 for parsed = (let ((*read-default-float-format* float-format))
                                (ignore-errors (read-from-string text)))
                 when (and (numberp parsed)
                           (= value (coerce parsed float-format)))
                   return text)))
    (unless candidate
      (setf candidate (string-trim '(#\Space #\Tab #\Newline)
                                   (format nil "~,vG" max-precision value))))
    (let* ((exponent-position (position-if (lambda (ch) (find ch "eEdDfFlLsS"))
                                           candidate))
           (exponent (if exponent-position
                         (parse-integer candidate :start (1+ exponent-position))
                         0))
           (mantissa (subseq candidate 0 exponent-position))
           (dot-position (or (position #\. mantissa) (length mantissa)))
           (raw-digits (remove #\. mantissa))
           (leading-zeroes (or (position-if-not (lambda (ch) (char= ch #\0))
                                                 raw-digits)
                               (1- (length raw-digits))))
           (digits (string-right-trim "0" (subseq raw-digits leading-zeroes)))
           (decimal-position (+ dot-position exponent (- leading-zeroes))))
      (values decimal-position (if (string= digits "") "0" digits)))))
