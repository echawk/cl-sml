(in-package #:cl-sml)

(defun make-portable-weak-eq-hash-table ()
  #+sbcl (make-hash-table :test #'eq :weakness :key)
  #-sbcl (make-hash-table :test #'eq))

(defun make-portable-synchronized-eq-hash-table (&key weak)
  "An EQ hash table that threads may read and write at once, with weak
keys when WEAK and the host has them.  ABCL's hash tables are synchronized
already."
  #+sbcl (if weak
             (make-hash-table :test 'eq :synchronized t :weakness :key)
             (make-hash-table :test 'eq :synchronized t))
  #+ecl (progn weak (make-hash-table :test 'eq :synchronized t))
  #-(or sbcl ecl) (progn weak (make-hash-table :test 'eq)))

(defun call-with-portable-thread-stack-size (size thunk)
  "Call THUNK, which makes a thread, so that the thread gets a control stack
of SIZE bytes where the host lets one choose (SBCL; elsewhere THUNK is
just called).  SIZE NIL means the host's default."
  #+sbcl (if size
             (let ((old (sb-alien:extern-alien "thread_control_stack_size"
                                               sb-alien:unsigned)))
               (setf (sb-alien:extern-alien "thread_control_stack_size"
                                            sb-alien:unsigned)
                     size)
               (unwind-protect (funcall thunk)
                 (setf (sb-alien:extern-alien "thread_control_stack_size"
                                              sb-alien:unsigned)
                       old)))
             (funcall thunk))
  #-sbcl (progn size (funcall thunk)))

(declaim (inline portable-closure-first-value))
(defun portable-threads-with-stack-size-p ()
  "Can CALL-ON-PORTABLE-THREAD give a new thread a stack of its own size?"
  #+(and sbcl sb-thread) t
  #-(and sbcl sb-thread) nil)

(defun call-on-portable-thread (stack-size specials thunk)
  "THUNK's values, THUNK called on a new thread with a control stack of
STACK-SIZE bytes and the special variables SPECIALS bound to their values
here (a new thread otherwise sees their global values); this thread waits
for it, and passes it an interactive interrupt (Ctrl-C) as a BREAK.  Where
PORTABLE-THREADS-WITH-STACK-SIZE-P is false, THUNK is called here."
  #+(and sbcl sb-thread)
  (let* ((values (mapcar #'symbol-value specials))
         (thread (call-with-portable-thread-stack-size
                  stack-size
                  (lambda ()
                    (sb-thread:make-thread
                     (lambda ()
                       (progv specials values
                         (multiple-value-list (funcall thunk))))
                     :name "cl-sml (large stack)")))))
    (loop
      (handler-case
          (return (values-list (sb-thread:join-thread thread :default nil)))
        (sb-sys:interactive-interrupt ()
          (sb-thread:interrupt-thread
           thread (lambda () (break "Interactive interrupt")))))))
  #-(and sbcl sb-thread)
  (progn stack-size specials (funcall thunk)))

(defun portable-closure-first-value (function)
  "The first value FUNCTION closes over, or NIL where FUNCTION is no
closure or the host does not tell."
  #+sbcl (and (sb-kernel:closurep function)
              (sb-kernel:%closure-index-ref function 0))
  #-sbcl (progn function nil))

(defmacro portable-truly-the (type form)
  "FORM, whose value the compiler may take to be of TYPE without checking."
  #+sbcl `(sb-ext:truly-the ,type ,form)
  #-sbcl `(the ,type ,form))

(defun portable-jump-table-declaration (enable)
  "Optimize qualities that let the compiler (SBCL) compile a CASE to a
jump table when ENABLE, and that forbid it otherwise; NIL elsewhere."
  #+sbcl `((sb-c:jump-table ,(if enable 1 0)))
  #-sbcl (progn enable nil))

(defparameter *portable-fixnum-int-p*
  #+sbcl t #-sbcl nil
  "Is SML's Int.int exactly the host's fixnums?  (SBCL's are 63 bits, as
cl-sml's Int is; other hosts' fixnums are narrower.)")

;;; The optimize policy of the Lisp the compiler emits (ROADMAP item 6),
;;; declared in each function it generates rather than proclaimed, so that
;;; loading SML never changes the policy of other Lisp code.  SML programs
;;; loop by tail recursion: the compiler turns a function's calls to itself
;;; in tail position into jumps on every implementation, and SBCL also
;;; merges the other tail calls, but only below DEBUG 3, so no policy here
;;; reaches it.
(defvar *sml-optimize-policy* :default
  "The optimize policy of compiled SML: :DEFAULT (safe, and the CL compiler
may optimize as usual), :DEBUG (full run-time checks and more debugging
information, still with tail calls merged), :FAST (SPEED 3, SAFETY 0: the
CL compiler trusts the types cl-sml declares; SML's own checks, such as
Overflow, Div and Subscript, stay, so SML semantics do not change), or a
list of OPTIMIZE
qualities such as ((SPEED 3) (SAFETY 1) (DEBUG 1)).  Read when SML is
compiled.")

(defun sml-optimize-qualities ()
  (case *sml-optimize-policy*
    (:default '((speed 1) (safety 1) (debug 1)))
    (:debug '((speed 0) (safety 3) (debug 2)))
    (:fast '((speed 3) (safety 0) (debug 0)))
    (t *sml-optimize-policy*)))

(defun portable-tail-call-declarations ()
  `((declare (optimize ,@(sml-optimize-qualities)))))

(defun portable-sml-lambda-form (name arg body)
  #+sbcl (if name
             `(sb-int:named-lambda ,(format nil "sml:~A" name) (,arg)
                ,@(portable-tail-call-declarations)
                ,body)
             `(lambda (,arg) ,@(portable-tail-call-declarations) ,body))
  #-sbcl (progn name `(lambda (,arg) ,@(portable-tail-call-declarations) ,body)))

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

;;; Setting the floating-point modes is slow (on arm64 SBCL, masking and
;;; restoring the traps around one multiplication made it about 70 times
;;; slower), while reading them is cheap.  So compiled SML masks the traps
;;; once per function, and only when they are not masked already (they are
;;; whenever SML calls SML): WITH-SML-IEEE-FLOATS around the body of each
;;; function that does real arithmetic, and SML-IEEE around each real
;;; operation, which needs nothing more inside such a body (it sees the
;;; symbol macro %SML-FLOATS-MASKED bound to T) and tests the modes first
;;; elsewhere (at top level, in code compiled apart).  A function value
;;; made inside such a body has a WITH-SML-IEEE-FLOATS of its own, as it
;;; may be called after the body returns.

(define-symbol-macro %sml-floats-masked nil)

#+sbcl
(defconstant +sml-float-trap-bits+
  (dpb (sb-vm::float-trap-mask '(:invalid :divide-by-zero :overflow :inexact))
       sb-vm:float-traps-byte 0)
  "The bits of SBCL's floating-point modes that enable the traps SML masks.")

(defmacro sml-float-traps-masked-p ()
  "Are the traps SML masks masked now?  (Always false where that cannot be
tested cheaply.)"
  #+sbcl `(zerop (logand (sb-vm:floating-point-modes) +sml-float-trap-bits+))
  #-sbcl nil)

(defmacro with-sml-ieee-floats (&body body)
  "BODY, the body of a compiled SML function, run with the float traps
masked and with its SML-IEEE forms compiled for that."
  (let ((function (gensym "IEEE-BODY")))
    `(flet ((,function ()
              (symbol-macrolet ((%sml-floats-masked t))
                ,@body)))
       (if (sml-float-traps-masked-p)
           (,function)
           (with-portable-float-traps-masked (,function))))))

(defmacro sml-ieee (form &environment environment)
  "FORM, real arithmetic, with IEEE results (infinities and NaNs) rather
than traps."
  (if (macroexpand-1 '%sml-floats-masked environment)
      form
      `(if (sml-float-traps-masked-p)
           ,form
           (with-portable-float-traps-masked ,form))))

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

(defmacro portable-double-nan-p (x)
  "Is the double X a NaN?  Open-coded where the host has a primitive."
  #+sbcl `(sb-ext:float-nan-p ,x)
  #-sbcl `(portable-float-nan-p ,x))

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

(defun portable-control-stack-size ()
  "The size in bytes of the current thread's control stack, or NIL where
that is not known."
  #+sbcl (- (sb-sys:sap-int (sb-vm::current-thread-offset-sap
                             sb-vm::thread-control-stack-end-slot))
            (sb-sys:sap-int (sb-vm::current-thread-offset-sap
                             sb-vm::thread-control-stack-start-slot)))
  #-sbcl nil)

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
