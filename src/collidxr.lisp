;;;; collidxr.lisp

(in-package #:collidxr)

;;; plotting

(defgeneric plot* (backend input &rest args &key &allow-other-keys)
  (:documentation "Plot INPUT with BACKEND. Typically you can just call `plot' instead."))

(defmethod plot* ((backend (eql :vgplot)) (list list) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (let* ((lists (if (listp (car list))
                    list
                    (list list)))
         (max (reduce #'max (mapcar #'length lists)))
         (x (vgplot:range 0 max))
         (conc (apply #'concatenate 'list lists))
         (y-min (reduce #'min conc))
         (y-max (reduce #'max conc))
         (vgplot-args (loop :for i :in lists
                            :for n :from 0 :upto 100
                            :append (list x i (write-to-string n)))))
    (apply #'vgplot:plot vgplot-args)
    (vgplot:axis (list 0 (1- max) y-min y-max)) ; to make sure the plot only is as wide as the values we've provided, and doesn't stretch further.
    lists))

;; FIX: for `plot', instead maybe use https://github.com/applied-science/emacs-vega-view?tab=readme-ov-file#common-lisp
(defgeneric plot (object &rest args &key &allow-other-keys)
  (:documentation "Plot OBJECT."))

(defmethod plot ((list list) &rest args &key (backend :vgplot) &allow-other-keys)
  (apply #'plot* backend list args))

(defmethod plot ((buffer cl-collider::buffer) &rest args &key &allow-other-keys)
  (apply #'plot (coerce (elt (buffer-to-array buffer) 0) 'list) args)) ; FIX: plot all channels

;;; pseudo-ugens

(defun b2 (in &optional (pan 0) (level 1)) ; FIX: handle IN having more than 2 channels
  "Get IN wrapped in a `pan2.ar' or `balance2.ar' as appropriate. In other words, if IN is mono, wrap it in `pan2.ar', and if IN is stereo, wrap it in `balance2.ar'. PAN and LEVEL are the panning and amplitude arguments, respectively."
  (ecase (length (ensure-list in)) ; (eql 'cons (type-of in)) ; FIX: delete this comment if this works
    (1 (pan2.ar (ensure-car in) pan level))
    (2 (balance2.ar (car in) (cadr in) pan level))))

;;; ds

(defun modify-params (params) ; FIX: maybe we should try to auto-detect the use of a GATE argument and inject it if it's used? it would certainly be convenient...
  "Get PARAMS, a `defsynth' argument list, altered to include dur, tempo, amp, pan, and out arguments if they're not already specified.

See also: `ds'"
  (let ((param-names (mapcar #'car params)))
    (dolist (x (list (list (intern "DUR" *package*) 1)
                     (list (intern "TEMPO" *package*) 1)
                     (list (intern "AMP" *package*) 0.5)
                     (list (intern "PAN" *package*) 0)
                     (list (intern "OUT" *package*) 0)))
      (unless (position (car x) param-names)
        (setf params (append params (list x)))))
    params))

;;; ds

(defun ds-convert-body (body)
  "Convert BODY, a plist of keys and values a la `pbind'. Any key with the name _ or - is changed into a gensym. All gensyms and ignorable symbols are returned as a second value. Metadata such as synthdef variant information is provided as the third return value."
  (labels ((ignorable-symbol (symbol)
             (position (symbol-name symbol) (list "_" "-") :test #'string=))
           (parse-value (value last-gensym)
             (if (listp value)
                 (loop :for idx :from 0
                       :for i :in value
                       :if (= idx 0)
                         :collect i
                       :else
                         :collect (parse-value i last-gensym))
                 (if (and (symbolp value)
                          (ignorable-symbol value))
                     last-gensym
                     value))))
    (let (ignored-variables
          metadata)
      (values (loop :for (key value) :on body :by #'cddr
                    :when (eql :-variants key)
                      :do (push :variants metadata)
                          (push value metadata)
                          (push (intern (symbol-name key)) ignored-variables)
                    :if (eql :metadata key)
                      :do (setf metadata (append (reverse value) metadata))
                    :else
                      :collect (let ((value-parsed (parse-value value (car ignored-variables)))
                                     (key-parsed (if (ignorable-symbol key)
                                                     (car (push (gensym) ignored-variables))
                                                     (intern (symbol-name key)))))
                                 (list key-parsed value-parsed)))
              ignored-variables
              (nreverse metadata)))))

;; FIX: `synthdef-metadata' can't be used for input-bus because it is common to ALL synths of that name, not the specific one!!!!!!! maybe use (sc::meta node) to store a plist of metadata instead?
(defun parse-ds-args-and-body (synth-name args body &key (type 'ds))
  "Parse a lambda list for a synth or node definition. Returns the altered lambda list, altered body, gensyms, and metadata as values."
  (when (eql 0 (position :fx args))
    (setf args (cdr args))
    (ecase type
      (ds (setf body (list* :sig `(in.ar in 2) body))
          (unless (find 'in args :test #'string-equal :key #'car)
            (setf args (list* (list (intern "IN") 0) args))))
      (dn (unless (synthdef-metadata synth-name :input-bus)
            (setf (synthdef-metadata synth-name :input-bus) (bus-audio :chanls 2)))
          (setf body (list* :sig `(in.ar ,(busnum (synthdef-metadata synth-name :input-bus)) 2) body)))))
  ;; FIX: :ctl ?
  (multiple-value-bind (cbody gensyms metadata) (ds-convert-body body)
    (values args cbody gensyms metadata)))

(defparameter *ds-synth-package* (uiop:ensure-package 'synths) ; FIX: make optional?
  "The package that `ds' will define synths in.")

(defmacro ds (name args &body body)
  ;; FIX: need to allow the use of replace-out.ar instead of always only out.ar.
  ;; FIX: allow arguments to include lag time, rate (i.e. :ir, :tr, :kr, :ar), a spec, etc
  "Syntax sugar for defining synths. Unlike `defsynth', you don't need to use `let*'. Instead, specify the synthdef's body as a set of pairs of names and ugens, similar to using `cl-patterns:pbind' Effectively, the `let*' is implied, as each name is given the value of the ugen. Use \"-\" for unused/ignored ugens and \"out\" for the synth's output.

Example:

;; (ds :foo ((gate 1) (freq 440))
;;   :env (env-gen.kr (env-adsr 0.01 0.1 0.5 0.1) :gate gate :act :free)
;;   :sig (pulse.ar freq)
;;   :sig (rlpf.ar sig (* freq 2) 0.5)
;;   :- (poll.kr (impulse.kr 1) sig)
;;   :sig (pan2.ar sig pan (* amp env))
;;   :out sig)

See also: `dn'"
  (check-type body property-list)
  (let ((fx-p (eql :fx (car args)))
        (sig (intern "SIG" *package*))
        (dur (intern "DUR" *package*))
        (tempo (intern "TEMPO" *package*))
        (pan (intern "PAN" *package*))
        (amp (intern "AMP" *package*))
        (out (intern "OUT" *package*)))
    (multiple-value-bind (args body gensyms metadata) (parse-ds-args-and-body name args body :type 'ds)
      (let ((args (modify-params args)))
        `(prog1 (defsynth ,name ,args
                  (declare (ignorable ,dur ,tempo))
                  (let* ,(remove out body :key #'car)
                    ,@(when gensyms
                        `((declare (ignorable ,@gensyms))))
                    (,(if fx-p 'replace-out.ar 'out.ar)
                     ,out
                     ,(or (cadr (assoc out body))
                          `(b2 ,sig ,pan ,(if (assoc 'env body)
                                              `(* env ,amp)
                                              amp))))))
           ,@(when metadata
               `((doplist (k v (list ,@metadata))
                   (setf (synthdef-metadata ,name k) v))))
           ,@(let ((package-symbol (ensure-symbol name *ds-synth-package*))
                   (package-name (package-name *ds-synth-package*))
                   (defun-args (append (list '&key) (mapcar (fn (subseq _ 0 2)) args))))
               `((cl:defun ,package-symbol ,defun-args
                   (cl-collider:synth ',name
                                      ,@(loop :for arg :in args
                                              :for arg-name := (car arg)
                                              :append (list (make-keyword (symbol-name arg-name)) arg-name))))
                 (export ',package-symbol ,package-name))))))))

(defun synth-variant (name &rest args) ; FIX: should also accept :variant or :-variant in ARGS to specify the variant
  "Like `cl-collider:synth', but can also start a synth variant. To specify a variant, NAME should be in the format NAME.VARIANT.

Synth variants are specified in the synthdef metadata. Set the metadata key :VARIANTS to a plist mapping the name of the variant to the plist of arguments to the synthdef.

Example:

;; ;; define the \"noisy\" variant for the :tb303 synth:
;; (setf (synthdef-metadata :tb303 :variants) (list :noisy (list :dist 1 :clip 1 :bias 1)))
;; ;; play the variant:
;; (synth-variant :tb303.noisy :freq 420 :dist 0)
;; ;; notice that you can override the arguments set in the variant.

See also: `cl-collider:synth'"
  (destructuring-bind (name &optional variant) (mapcar 'intern (string-split (symbol-name name) :char-bag #\. :count 2))
    (apply 'cl-collider:synth name (append (cadr (member variant (synthdef-metadata name :variants)
                                                         :test (lambda (x y)
                                                                 (when (symbolp y)
                                                                   (string= x y)))))
                                           args))))

;;; dn

(defun find-dn (name)
  "Find a `dn' by name."
  (gethash name (cl-collider::node-proxy-table *s*)))

;; FIX: use sig if out is not provided
(defmacro dn (name &optional (args nil args-p) &body body)
  "\"Define Node\"--define a named node proxy. Similar to `cl-collider:proxy', but with the following additional features:

- `pbind'-like syntax (no `let*').
- ARGS are inserted as `with-controls'.
- Bindings named _ or - are automatically declared ignorable.
- When :fx is provided as an item in ARGS, allocate a 2-channel bus and bind sig to (in.ar BUS 2).

See also: `ds'"
  (when (and (null args-p)
             (null body))
    (return-from dn `(find-dn ,name)))
  (when (and args-p
             (null args)
             (or (null body)
                 (equal (list nil) body)))
    (return-from dn `(progn
                       (when (synthdef-metadata ,name :input-bus)
                         ;; FIX: make sure the bus is freed when the `dn' is stopped with `free', `release', etc.
                         (bus-free (synthdef-metadata ,name))
                         (setf (synthdef-metadata ,name :input-bus) nil))
                       (cl-collider:proxy ,name nil))))
  (let ((pos (or (getf body :-pos) :head))
        (out (intern "OUT" *package*)))
    (remove-from-plistf body :-pos)
    (multiple-value-bind (args body gensyms metadata) (parse-ds-args-and-body name args body :type 'dn)
      `(progn
         (cl-collider:proxy ,name
                            (with-controls ,args
                              (let* ,(remove out body :key #'car)
                                ,@(when gensyms
                                    `((declare (ignorable ,@gensyms))))
                                ,(cadr (assoc out body))))
                            :pos ,pos)
         ,@(when metadata
             `((doplist (k v (list ,@metadata))
                 (setf (synthdef-metadata ,name k) v))))))))
