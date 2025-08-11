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

;; FIX: for `plot', instead maybe use https://github.com/applied-science/emacs-vega-view
(defgeneric plot (object &rest args &key &allow-other-keys)
  (:documentation "Plot OBJECT."))

(defmethod plot ((list list) &rest args &key (backend :vgplot) &allow-other-keys)
  (apply #'plot* backend list args))

(defmethod plot ((buffer cl-collider::buffer) &rest args &key &allow-other-keys)
  (apply #'plot (coerce (elt (buffer-to-array buffer) 0) 'list) args)) ; FIX: plot all channels

;;; b2

(defun b2 (in &optional (pan 0) (level 1)) ; FIX: handle IN having more than 2 channels
  "Get IN wrapped in a `pan2.ar' or `balance2.ar' as appropriate. In other words, if IN is mono, wrap it in `pan2.ar', and if IN is stereo, wrap it in `balance2.ar'. PAN and LEVEL are the panning and amplitude arguments, respectively."
  (ecase (length (ensure-list in)) ; (eql 'cons (type-of in)) ; FIX: delete this comment if this works
    (1 (pan2.ar (ensure-car in) pan level))
    (2 (balance2.ar (elt in 0) (elt in 1) pan level))))

;;; fbnode
;; Inspired by the FbNode class in the "Feedback" SuperCollider quark; https://quark.sccode.org/Feedback/FbNode.html

(defclass fbnode ()
  ((num-channels :initarg :num-channels :initform 1)
   (frames :initarg :frames :initform 1024)
   (phase :initarg :phase :initform nil)
   (max-delay-time :initarg :max-delay-time :initform nil)
   (local-buf :initarg :local-buf :initform nil)
   (interpolation :initarg :interpolation :initform 2))
  (:documentation "Feedback node, for creating feedback loops within synthdefs. It's recommended to use `fbn' to create an `fbnode', and then `fbn-read' and `fbn-write' to read and write to it.

See also: `fbn', `fbn-read', `fbn-write'"))

(defmethod cl-collider::floatfy ((fbnode fbnode))
  (with-slots (num-channels phase local-buf interpolation) fbnode
    (let ((block-size (sc::server-options-block-size (sc::server-options *s*))))
      (buf-rd.ar num-channels local-buf (sc::-~ phase block-size) 1 interpolation))))

(defun fbn (&optional (num-channels 1) max-delay-time (interpolation 2))
  "Create a `fbnode' for doing feedback within synthdefs. NUM-CHANNELS is the number of channels of the internal delay buffer. MAX-DELAY-TIME is the length in seconds that the delay buffer should be allocated to, defaulting to the minimum delay possible (one block). INTERPOLATION is the type of interpolation to use when reading from the delay, as per `buf-rd.ar'.

Example:

;; (defsynth :echoed ((freq 440) (delay 0.5) (feed 0.5) (amp 0.5) (out 0))
;;  (let* ((fbn (fbn 2 0.5))
;;         (sig (var-saw.ar (+ (rand.ir -2.0 (list 2.0 2.0)) freq) 0 0.5 (env-gen.kr (perc 0.01 0.25))))
;;         (sig (rlpf.ar (+ sig (* feed (fbn-read fbn delay)))
;;                       (* freq 0.9) 0.9)))
;;    (fbn-write fbn sig)
;;    (detect-silence.ar (leak-dc.ar sig) 1.0e-4 :time delay :act :free)
;;    (out.ar out (* sig amp))))

See also: `fbnode', `fbn-read', `fbn-write'"
  (let* ((sample-rate (sr *s*))
         (block-size (sc::server-options-block-size (sc::server-options *s*)))
         (max-delay-time (or max-delay-time (/ block-size sample-rate)))
         (frames (+ (* max-delay-time sample-rate) block-size))
         (fbnode (make-instance 'fbnode :num-channels num-channels
                                        :frames frames
                                        :phase (phasor.ar 0 1 0 frames)
                                        :max-delay-time max-delay-time
                                        :local-buf (local-buf frames num-channels)
                                        :interpolation interpolation)))
    (clear-buf (slot-value fbnode 'local-buf))
    fbnode))

(defun fbn-read (fbnode &optional (delay (slot-value fbnode 'max-delay-time)))
  "Read from FBNODE at a time DELAY seconds from its input. DELAY defaults to the `fbnode''s max delay time.

See also: `fbnode', `fbn', `fbn-write'"
  (let* ((block-size (sc::server-options-block-size (sc::server-options *s*)))
         (sample-rate (sr *s*))
         (offset (sc::min~ (sc::max~ (sc::*~ delay sample-rate) block-size)
                           (sc::-~ (slot-value fbnode 'frames) block-size))))
    (buf-rd.ar (slot-value fbnode 'num-channels)
               (slot-value fbnode 'local-buf)
               (sc::-~ (slot-value fbnode 'phase) offset)
               1
               (slot-value fbnode 'interpolation))))

(defun fbn-write (fbnode input)
  "Write INPUT to FBNODE.

See also: `fbnode', `fbn', `fbn-read'"
  (buf-wr.ar input (slot-value fbnode 'local-buf) (slot-value fbnode 'phase))
  input)

;;; ds/dn utilities

(defun standard-params (params &key (out t)) ; FIX: maybe we should try to auto-detect the use of a GATE argument and inject it if it's used? it would certainly be convenient...
  "Get PARAMS, a `defsynth' argument list, altered to include dur, tempo, amp, pan, and out arguments if they're not already specified.

See also: `ds'"
  (setf params (if (eql :fx (car params)) (cdr params) params))
  (dolist (x `((,(intern "DUR" *package*) 1)
               (,(intern "TEMPO" *package*) 1)
               (,(intern "PAN" *package*) 0)
               (,(intern "AMP" *package*) 0.5)
               ,@(when out `((,(intern "OUT" *package*) 0))))
             params)
    (unless (position (car x) params :key #'car)
      (setf params (append params (list x))))))

(defun body-plist-code (body)
  "Convert BODY, a plist of keys and values a la `pbind'. Any key with the name _ or - is changed into a gensym. All gensyms and ignorable symbols are returned as a second value. Metadata such as synthdef variant information is provided as the third return value."
  (labels ((ignorable-symbol (symbol)
             (position (symbol-name symbol) (list "_" "-") :test #'string=))
           (parse-value (value last-gensym)
             (cond ((listp value)
                    (loop :for idx :from 0
                          :for i :in value
                          :if (zerop idx)
                            :collect i
                          :else
                            :collect (parse-value i last-gensym)))
                   ((and (symbolp value)
                         (ignorable-symbol value))
                    last-gensym)
                   (t value))))
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

(defparameter *ds-synth-package* (uiop:ensure-package 'synths) ; FIX: make optional - if it's nil, just use the current package
  "The package that `ds' will define synths in.")

;;; ds

;; FIX: need to allow the use of replace-out.ar instead of always only out.ar.
;; FIX: allow arguments to include lag time, rate (i.e. :ir, :tr, :kr, :ar), a spec, etc
(defmacro ds (name params &body body)
  "\"Define Synth\"; syntax sugar wrapping `defsynth'. It does the following:

- Provides a more concise syntax for writing synthdefs; BODY is simply a plist mapping variable names to their values. No `let*' needed!
- If not specified by the user, auto-inserts parameters for dur, tempo, pan, amp, and out with sensible defaults, marked as ignorable, to avoid warnings if they aren't used.
- Bindings named - or _ are automatically declared ignorable.
- If out is bound in BODY, its value is automatically used as the input for an `out.ar'.
- If there is no out in BODY but there is a sig, sig is automatically fed into a `b2' inside an `out.ar'. In other words, the pan, amp, and out arguments will \"just work\".

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
  ;; (when (and (null args-p) ; FIX: uncomment when `sc:find-synthdef' exists
  ;;            (null body))
  ;;   (return-from ds `(find-synthdef ,name)))
  (let* ((fx-p (eql :fx (car params)))
         (sig (intern "SIG" *package*))
         (dur (intern "DUR" *package*))
         (tempo (intern "TEMPO" *package*))
         (pan (intern "PAN" *package*))
         (amp (intern "AMP" *package*))
         (out (intern "OUT" *package*))
         (params (standard-params params)))
    (when fx-p
      (setf body (list* :sig `(in.ar in 2) body))
      (unless (find 'in params :test #'string-equal :key #'car)
        (setf params (list* (list (intern "IN" *package*) 0) params))))
    (multiple-value-bind (body gensyms metadata) (body-plist-code body)
      `(prog1 (defsynth ,name ,params
                (declare (ignorable ,dur ,tempo))
                (let* ,(remove out body :key #'car)
                  ,@(when gensyms
                      `((declare (ignorable ,@gensyms))))
                  (,(if fx-p 'replace-out.ar 'out.ar)
                   ,out
                   ,(or (cadr (assoc out body))
                        `(b2 ,sig ,pan ,(if (assoc 'env body)
                                            `(* env ,amp) ; FIX: if the user modifies AMP within the body, does it actually take effect here?
                                            amp))))))
         ,@(when metadata
             `((doplist (k v (list ,@metadata))
                 (setf (synthdef-metadata ,name k) v))))
         ,@(let ((package-symbol (ensure-symbol name *ds-synth-package*))
                 (package-name (package-name *ds-synth-package*))
                 (defun-args (append (list '&key) (mapcar (fn (subseq _ 0 2)) params))))
             `((cl:defun ,package-symbol ,defun-args
                 (cl-collider:synth ',name
                                    ,@(loop :for arg :in params
                                            :for arg-name := (car arg)
                                            :append (list (make-keyword (symbol-name arg-name)) arg-name))))
               (export ',package-symbol ,package-name)))))))

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

(defun find-proxy (name)
  "Get the node for the `proxy' named NAME."
  (gethash name (cl-collider::node-proxy-table *s*)))

;; FIX: is it possible to have TEMPO automatically updated when the tempo in cl-patterns is changed?
;; FIX: `synthdef-metadata' shouldn't be used for input-bus because it is common to ALL synths of that name, not the specific one!!!!!!! maybe use (sc::meta node) to store a plist of metadata instead?
(defmacro dn (name &optional (params nil params-p) &body body) ; FIX: should also include dur, tempo, etc, as per `standard-params'
  "\"Define Node\"--define a named node proxy. Similar to `cl-collider:proxy', but with the following additional features:

- Provides a more concise syntax for writing proxies; BODY is simply a plist mapping variable names to their values. No `let*' needed!
- PARAMS are inserted as `with-controls'.
- Without BODY, just returns the node.
- If not specified by the user, auto-inserts parameters for dur, tempo, pan, amp, and out with sensible defaults, marked as ignorable, to avoid warnings if they aren't used.
- Bindings named - or _ are automatically declared ignorable.
- :pos binding can be used to specify the :pos of the node within its group (i.e. :head, :tail, etc).
- When :fx is the first element of PARAMS, allocate a 2-channel bus and bind sig to (in.ar BUS 2).
- If out is bound in BODY, its value is automatically used as the input for an `out.ar'.
- If there is no out in BODY but there is a sig, sig is automatically fed into a `b2' inside an `out.ar'. In other words, the pan, amp, and out arguments will \"just work\".
- Can be provided as an :instrument in a `cl-patterns:pbind' to set the parameters of the node.
- Can be provided as an :out in a `cl-patterns:pbind' to send the output of triggered synths to the node (for use with :fx nodes).

Example:

;; (dn :echo (:fx (time 1.0) (decay 0.5))
;;   :sig (comb-c.ar sig 1 time decay))

See also: `ds'"
  (check-type body property-list)
  (when (and (null params-p)
             (null body))
    (return-from dn `(find-proxy ,name)))
  (when (and params-p
             (null params)
             (or (null body)
                 (equal (list nil) body)))
    (return-from dn `(progn
                       (when (synthdef-metadata ,name :input-bus)
                         ;; FIX: make sure the bus is freed when the `dn' is stopped with `free', `release', etc.
                         (bus-free (synthdef-metadata ,name))
                         (setf (synthdef-metadata ,name :input-bus) nil))
                       (cl-collider:proxy ,name nil))))
  (let* ((fx-p (eql :fx (car params)))
         (pos (getf body :pos :head))
         (sig (intern "SIG" *package*))
         (out (intern "OUT" *package*))
         (params (standard-params params :out nil)))
    (remove-from-plistf body :pos)
    (when fx-p
      (unless (synthdef-metadata name :input-bus)
        (setf (synthdef-metadata name :input-bus) (bus-audio :chanls 2)))
      (setf body (list* :sig `(in.ar ,(busnum (synthdef-metadata name :input-bus)) 2) body)))
    (multiple-value-bind (body gensyms metadata) (body-plist-code body)
      `(prog1
           (cl-collider:proxy ,name
                              (with-controls ,params
                                (let* ,(remove out body :key #'car)
                                  ,@(when gensyms
                                      `((declare (ignorable ,@gensyms))))
                                  ,(or (cadr (assoc out body))
                                       sig)))
                              :pos ,pos)
         ,@(when metadata
             `((doplist (k v (list ,@metadata))
                 (setf (synthdef-metadata ,name k) v))))))))
