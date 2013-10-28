(in-package :score-pane)
(defparameter *inactive-colour* +black+) ;; +gray50+
(defclass score-view (view)
  ((light-glyphs-ink :initform *inactive-colour* :initarg :light-glyphs-ink :accessor light-glyphs-ink)
   (%number-of-pages :initform "-" :accessor number-of-pages)
   (%current-page-number :initform "-" :accessor current-page-number)))

(defclass score-pane (esa-pane-mixin application-pane) ())

(defmethod initialize-instance :after ((pane score-pane) &rest args)
  (declare (ignore args))
  (setf (stream-default-view pane) (make-instance 'score-view)))

(defparameter *font* nil)
(defparameter *fonts* (make-array 100 :initial-element nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; output recording

(defclass score-output-record (displayed-output-record)
  ((parent :initarg :parent :initform nil :accessor output-record-parent)
   (x :initarg :x1 :initarg :x-position)
   (y :initarg :y1 :initarg :y-position)
   (width)
   (height)
   (ink :initarg :ink :reader displayed-output-record-ink)))

(defmethod initialize-instance :after ((record score-output-record)
                                       &key x2 y2 size)
  (declare (ignore size))
  (with-slots (x y width height) record
    (setf width (abs (- x2 x))
          height (abs (- y2 y)))))

(defmethod bounding-rectangle* ((record score-output-record))
  (with-slots (x y width height) record
    (values x y (+ x width) (+ y height))))

(defmethod output-record-position ((record score-output-record))
  (with-slots (x y) record
    (values x y)))

(defmethod (setf output-record-position) (new-x new-y (record score-output-record))
  (with-slots (x y) record
    (setf x new-x y new-y)))

(defmethod output-record-start-cursor-position ((record score-output-record))
  (values nil nil))

(defmethod (setf output-record-start-cursor-position) (x y (record score-output-record))
  (declare (ignore x y))
  nil)

(defmethod output-record-end-cursor-position ((record score-output-record))
  (values nil nil))

(defmethod (setf output-record-end-cursor-position) (x y (record score-output-record))
  (declare (ignore x y))
  nil)

(defmethod output-record-hit-detection-rectangle* ((record score-output-record))
  (bounding-rectangle* record))

(defmethod output-record-refined-position-test  ((record score-output-record) x y)
  (declare (ignore x y))
  t)

;;; remove this when McCLIM is fixed
(defmethod region-intersects-region-p (region (record score-output-record))
  (with-bounding-rectangle* (x1 y1 x2 y2) record
    (region-intersects-region-p region (make-rectangle* x1 y1 x2 y2))))

;;;;;;;;;;;;;;;;;; pixmap drawing

(climi::def-grecording draw-pixmap (() pixmap pm-x pm-y) ()
  (climi::with-transformed-position ((medium-transformation medium) pm-x pm-y)
    (setf (slot-value climi::graphic 'pm-x) pm-x
          (slot-value climi::graphic 'pm-y) pm-y)
    (values pm-x pm-y (+ pm-x (pixmap-width pixmap)) (+ pm-y (pixmap-height pixmap)))))

(climi::def-graphic-op draw-pixmap (pixmap pm-x pm-y))

(defmethod medium-draw-pixmap* ((medium clim:medium) pixmap pm-x pm-y)
  (copy-from-pixmap pixmap 0 0 (pixmap-width pixmap) (pixmap-height pixmap)
                    medium pm-x pm-y))

(climi::defmethod* (setf output-record-position) :around
    (nx ny (record draw-pixmap-output-record))
    (climi::with-standard-rectangle* (:x1 x1 :y1 y1)
        record
      (with-slots (pm-x pm-y)
          record
        (let ((dx (- nx x1))
              (dy (- ny y1)))
          (multiple-value-prog1
              (call-next-method)
            (incf pm-x dx)
            (incf pm-y dy))))))

(climi::defrecord-predicate draw-pixmap-output-record (pm-x pm-y)
  (and (climi::if-supplied (pm-x coordinate)
         (climi::coordinate= (slot-value climi::record 'pm-x) pm-x))
       (climi::if-supplied (pm-y coordinate)
         (climi::coordinate= (slot-value climi::record 'pm-y) pm-y))))

(defun draw-pixmap* (sheet pixmap x y
                           &rest args
                           &key clipping-region transformation)
  (declare (ignore clipping-region transformation))
  (climi::with-medium-options (sheet args)
    (medium-draw-pixmap* medium pixmap x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; drawing functions

;;; A staff step is half of the distance between two staff lines.
;;; Given a staff-step value, determine the corresponding number of
;;; pixels in the current font.  The sign of the value returned is 
;;; the same as that of the argument.
;;; But is that reasonable?  It seems more logical to have it return
;;; the opposite sign, so that the result from staff-step is always
;;; added to some y coordinate.
(defun staff-step (n)
  (* n (/ (staff-line-distance *font*) 2)))

;;;;;;;;;;;;;;;;;; notehead

(define-presentation-type notehead () :options (name x staff-step))

(defun draw-notehead (stream name x staff-step)
  (sdl::draw-shape stream *font* 
                   (ecase name
                     ((:breve :long) :breve-notehead)
                     (:whole :whole-notehead)
                     (:half :half-notehead)
                     (:filled :filled-notehead))
                   x (staff-step (- staff-step))))

(define-presentation-method present
    (object (type notehead) stream (view score-view) &key)
  (with-output-as-presentation (stream object 'notehead)
    (draw-notehead stream name x staff-step)))

;;;;;;;;;;;;;;;;;; accidental

(defun draw-accidental (stream name x staff-step)
  (sdl::draw-shape stream *font* name x (staff-step (- staff-step))))

;;;;;;;;;;;;;;;;;; clef

(defun draw-clef (stream name x staff-step)
  (sdl::draw-shape stream *font* 
                   (ecase name
                     ;; FIXME: while using the same glyph for :TREBLE
                     ;; and :TREBLE8vb is fine from a musical point of
                     ;; view, some differentiation (by putting an
                     ;; italic 8 underneath, for instance) would be
                     ;; good.
                     ((:treble15ma :treble8va :treble  :treble8vb) :g-clef)
                     ((:bass8va    :bass      :bass8vb :bass15mb)  :f-clef)
                     ((:c)                                         :c-clef))
                   x (staff-step (- staff-step)))
  (case name 
    ((:treble15ma :bass15ma)            #|TODO: draw 15ma above|#)
    ((:treble8va  :bass8va)             #|TODO: draw  8va above|#)
    ((:treble8vb  :bass8vb)             #|TODO: draw  8vb below|#)
    ((:treble15mb :bass15mb)            #|TODO: draw 15mb below|#)))

(define-presentation-type clef () :options (name x staff-step))

(define-presentation-method present
    (object (type clef) stream (view score-view) &key)
  (with-output-as-presentation (stream object 'clef)
    (draw-clef stream name x staff-step)))

;;;;;;;;;;;;;;;;;; time signature

(defun draw-time-signature-component (stream component x)
  (flet ((component-name (c)
           (ecase c
             (1 :time-signature-1)
             (2 :time-signature-2)
             (3 :time-signature-3)
             (4 :time-signature-4)
             (5 :time-signature-5)
             (6 :time-signature-6)
             (7 :time-signature-7)
             (8 :time-signature-8))))
    (etypecase component
      ((integer 1 8)
       (let* ((design (sdl::ensure-design *font* (component-name component))))
         (sdl::draw-shape stream *font* design x (staff-step -2))
         (bounding-rectangle-width design)))
      ((cons (integer 1 8) (integer 1 8))
       (destructuring-bind (num . den) component
         (let* ((num-name (component-name num))
                (den-name (component-name den))
                (ndesign (sdl::ensure-design *font* num-name))
                (ddesign (sdl::ensure-design *font* den-name)))
           (sdl::draw-shape stream *font* num-name x (staff-step -4))
           (sdl::draw-shape stream *font* den-name x (staff-step 0))
           (max (bounding-rectangle-width ndesign)
                (bounding-rectangle-width ddesign))))))))

;;;;;;;;;;;;;;;;;; rest

(defun draw-rest (stream duration x staff-step)
  (sdl::draw-shape stream *font*
                   (ecase duration
                     (4 :long-rest)
                     (2 :breve-rest)
                     (1 :whole-rest)
                     (1/2 :half-rest)
                     (1/4 :quarter-rest)
                     (1/8 :8th-rest)
                     (1/16 :16th-rest)
                     (1/32 :32nd-rest)
                     (1/64 :64th-rest)
                     ;; FIXME 128th
                     )
                   x (staff-step (- staff-step))))

;;;;;;;;;;;;;;;;;; flags down

(defun draw-flags-down (stream nb x staff-step)
  (sdl::draw-shape stream *font*
                   (ecase nb
                     (1 :flags-down-1)
                     (2 :flags-down-2)
                     (3 :flags-down-3)
                     (4 :flags-down-4)
                     (5 :flags-down-5))
                   x (staff-step (- staff-step))))

;;;;;;;;;;;;;;;;;; flags up

(defun draw-flags-up (stream nb x staff-step)
  (sdl::draw-shape stream *font*
                   (ecase nb
                     (1 :flags-up-1)
                     (2 :flags-up-2)
                     (3 :flags-up-3)
                     (4 :flags-up-4)
                     (5 :flags-up-5))
                   x (staff-step (- staff-step))))

;;;;;;;;;;;;;;;;;; dot

(defun draw-dot (stream x staff-step)
  (sdl::draw-shape stream *font* :dot x (staff-step (- staff-step))))

;;;;;;;;;;;;;;;;;; staff line

(defun draw-staff-line (pane x1 staff-step x2)
  (multiple-value-bind (down up) (staff-line-offsets *font*)
    (let ((y1 (+ (- (staff-step staff-step)) up))
          (y2 (+ (- (staff-step staff-step)) down)))
      (draw-rectangle* pane x1 y1 x2 y2))))

(defclass staff-output-record (output-record)
  ((parent :initarg :parent :initform nil :accessor output-record-parent)
   (x :initarg :x1 :initarg :x-position)
   (y :initarg :y1 :initarg :y-position)
   (width :initarg :width)
   (height :initarg height)
   (staff-lines :initform '() :reader output-record-children)))

(defmethod bounding-rectangle* ((record staff-output-record))
  (with-slots (x y width height) record
    (values x y (+ x width) (+ y height))))

(defmethod output-record-position ((record staff-output-record))
  (with-slots (x y) record
    (values x y)))

(defmethod (setf output-record-position) (new-x new-y (record staff-output-record))
  (with-slots (x y staff-lines) record
    (setf x new-x y new-y)
    (loop for staff-line in staff-lines
          do (multiple-value-bind (xx yy) (output-record-position staff-line)
               (setf (output-record-position staff-line)
                     (values (+ xx (- new-x x))
                             (+ yy (- new-y y))))))))  

(defmethod output-record-start-cursor-position ((record staff-output-record))
  (values nil nil))

(defmethod (setf output-record-start-cursor-position) (x y (record staff-output-record))
  (declare (ignore x y))
  nil)

(defmethod output-record-end-cursor-position ((record staff-output-record))
  (values nil nil))

(defmethod (setf output-record-end-cursor-position) (x y (record staff-output-record))
  (declare (ignore x y))
  nil)

(defmethod output-record-hit-detection-rectangle* ((record staff-output-record))
  (bounding-rectangle* record))

(defmethod output-record-refined-position-test  ((record staff-output-record) x y)
  (declare (ignore x y))
  t)

;;; remove this when McCLIM is fixed
(defmethod region-intersects-region-p (region (record staff-output-record))
  (with-bounding-rectangle* (x1 y1 x2 y2) record
    (region-intersects-region-p region (make-rectangle* x1 y1 x2 y2))))

(defmethod add-output-record (child (record staff-output-record))
  (push child (slot-value record 'children)))

(defmethod delete-output-record (child (record staff-output-record) &optional (errorp t))
  (with-slots (staff-lines) record
    (when (and errorp (not (member child staff-lines :test #'eq)))
      (error "not a child"))
    (setf staff-lines (delete child staff-lines :test #'eq))))

(defmethod clear-output-record ((record staff-output-record))
  (setf (slot-value record 'staff-lines) '()))

(defmethod output-record-count ((record staff-output-record))
  (length (slot-value record 'staff-lines)))

(defmethod replay-output-record ((record staff-output-record) stream
                                 &optional (region +everywhere+)
                                 (x-offset 0) (y-offset 0))
  (loop for staff-line in (slot-value record 'staff-lines)
        do (replay-output-record staff-line stream region x-offset y-offset)))

(define-presentation-type staff () :options (x1 x2))

(define-presentation-type fiveline-staff () :inherit-from 'staff :options (x1 x2))

(defun draw-fiveline-staff (pane x1 x2)
  (multiple-value-bind (left right) (bar-line-offsets *font*)
    (loop for staff-step from 0 by 2
          repeat 5
          do (draw-staff-line pane (+ x1 left) staff-step (+ x2 right)))))

(define-presentation-method present
    (object (type fiveline-staff) stream (view score-view) &key)
  (with-output-as-presentation (stream object 'fiveline-staff)
    (draw-fiveline-staff stream x1 x2)))

(define-presentation-type lyrics-staff () :inherit-from 'staff :options (x1 x2))

(defun draw-lyrics-staff (pane x1 x2)
  (declare (ignore x2))
  (multiple-value-bind (left right) (bar-line-offsets *font*)
    (declare (ignore right))
    (draw-text* pane "--" (+ x1 left) 0)))

(define-presentation-method present
    (object (type lyrics-staff) stream (view score-view) &key)
  (with-output-as-presentation (stream object 'lyrics-staff)
    (draw-lyrics-staff stream x1 x2)))

;;;;;;;;;;;;;;;;;; stem

(defun draw-stem (pane x y1 y2)
  (multiple-value-bind (left right) (stem-offsets *font*)
    (let ((x1 (+ x left))
          (x2 (+ x right)))
      (draw-rectangle* pane x1 y1 x2 y2))))
                             
(defun draw-right-stem (pane x y1 y2)
  (multiple-value-bind (dx dy) (notehead-right-offsets *font*)
    (draw-stem pane (+ x dx) (- y1 dy) y2)))

(defun draw-left-stem (pane x y1 y2)
  (multiple-value-bind (dx dy) (notehead-left-offsets *font*)
    (draw-stem pane (+ x dx) (- y1 dy) y2)))

;;;;;;;;;;;;;;;;;; ledger line

(defun draw-ledger-line (pane x staff-step)
  (multiple-value-bind (down up) (ledger-line-y-offsets *font*)
    (multiple-value-bind (left right) (ledger-line-x-offsets *font*)
      (let ((x1 (+ x left))
            (y1 (- (+ (staff-step staff-step) down)))
            (x2 (+ x right))
            (y2 (- (+ (staff-step staff-step) up))))
        (draw-rectangle* pane x1 y1 x2 y2)))))

                             
;;;;;;;;;;;;;;;;;; bar line

(defun draw-bar-line (pane x y1 y2)
  (multiple-value-bind (left right) (bar-line-offsets *font*)
    (let ((x1 (+ x left))
          (x2 (+ x right)))
      ;; see comment in ROUND-COORDINATE in McCLIM's CLX backend
      (draw-rectangle* pane (floor (+ x1 0.5)) y1 (floor (+ x2 0.5)) y2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; beam drawing

(defclass beam-output-record (score-output-record)
  ((light-glyph-p :initarg :light-glyph-p)
   (clipping-region :initarg :clipping-region)
   (thickness :initarg :thickness)))

;;; draw a horizontal beam around the vertical reference 
;;; point y.
(defun draw-horizontal-beam (medium x1 y x2)
  (multiple-value-bind (down up) (beam-offsets *font*)
    (draw-rectangle* medium x1 (+ y up) x2 (+ y down))))


(defclass downward-beam-output-record (beam-output-record)
  ())

(defmethod medium-draw-downward-beam* (medium x1 y1 x2 y2 thickness)
  (let ((inverse-slope (abs (/ (- x2 x1) (- y2 y1)))))
    (loop for y from y1 below y2
          for x from x1 by inverse-slope do
          (let ((upper (sdl::ensure-beam-segment-design :down :upper (- (round (+ x inverse-slope)) (round x))))
                (upper-tr (make-translation-transformation (round x) y))
                (lower (sdl::ensure-beam-segment-design :down :lower (- (round (+ x inverse-slope)) (round x))))
                (lower-tr (make-translation-transformation (round x) (+ y thickness))))
            (climi::medium-draw-bezier-design* medium (transform-region upper-tr upper))
            (climi::medium-draw-bezier-design* medium (transform-region lower-tr lower))
            (medium-draw-rectangle* medium (round x) (1+ y) (round (+ x inverse-slope)) (+ y thickness) t)))))

(defmethod medium-draw-downward-beam* 
    ((medium clim-postscript::postscript-medium) x1 y1 x2 y2 thickness)
  (draw-polygon* (medium-sheet medium) `(,x1 ,y1 ,x1 ,(+ y1 thickness) ,x2 ,(+ y2 thickness) ,x2 ,y2) :closed t :filled t))

(defmethod medium-draw-upward-beam* (medium x1 y1 x2 y2 thickness)
  (let ((inverse-slope (abs (/ (- x2 x1) (- y2 y1)))))
    (loop for y from y1 above y2
          for x from x1 by inverse-slope do
          (let ((upper (sdl::ensure-beam-segment-design :up :upper (- (round (+ x inverse-slope)) (round x))))
                (upper-tr (make-translation-transformation (round x) y))
                (lower (sdl::ensure-beam-segment-design :up :lower (- (round (+ x inverse-slope)) (round x))))
                (lower-tr (make-translation-transformation (round x) (+ y thickness -1))))
            (climi::medium-draw-bezier-design* medium (transform-region upper-tr upper))
            (climi::medium-draw-bezier-design* medium (transform-region lower-tr lower))
            (medium-draw-rectangle* medium (round x) y (round (+ x inverse-slope)) (1- (+ y thickness)) t)))))

(defmethod medium-draw-upward-beam* 
    ((medium clim-postscript::postscript-medium) x1 y1 x2 y2 thickness)
  (draw-polygon* (medium-sheet medium) `(,x1 ,y1 ,x1 ,(+ y1 thickness) ,x2 ,(+ y2 thickness) ,x2 ,y2) :closed t :filled t))

(defmethod replay-output-record ((record downward-beam-output-record) stream
                                 &optional (region +everywhere+)
                                 (x-offset 0) (y-offset 0))
  (declare (ignore x-offset y-offset region))
  (with-bounding-rectangle* (x1 y1 x2 y2) record
    (with-slots (thickness ink clipping-region) record
      (let ((medium (sheet-medium stream)))
        (with-drawing-options 
            (medium :ink ink :clipping-region clipping-region)
          (medium-draw-downward-beam* medium x1 y1 x2 (- y2 thickness) thickness))))))

(defclass upward-beam-output-record (beam-output-record)
  ())

(defmethod replay-output-record ((record upward-beam-output-record) stream
                                 &optional (region +everywhere+)
                                 (x-offset 0) (y-offset 0))
  (declare (ignore x-offset y-offset region))
  (with-bounding-rectangle* (x1 y1 x2 y2) record
    (with-slots (thickness ink clipping-region) record
      (let ((medium (sheet-medium stream)))
        (with-drawing-options 
            (medium :ink ink :clipping-region clipping-region)
          (medium-draw-upward-beam* medium x1 (- y2 thickness) x2 y1 thickness))))))

(defun transform-beam-attributes (transformation x1 y1 x2 y2 down up thickness)
  (multiple-value-bind (xx1 yy1)
      (transform-position transformation x1 y1)
    (multiple-value-bind (xx2 yy2)
        (transform-position transformation x2 y2)
      (multiple-value-bind (xd yd)
          (transform-distance transformation 0 down)
        (declare (ignore xd))
        (multiple-value-bind (xu yu)
            (transform-distance transformation 0 up)
          (declare (ignore xu))
          (multiple-value-bind (xt yt)
              (transform-distance transformation 0 thickness)
            (declare (ignore xt))
            (values xx1 yy1 xx2 yy2 yd yu yt)))))))

;;; draw a sloped beam.  The vertical reference points 
;;; of the two end points are indicated by y1 and y2. 
(defun draw-sloped-beam (medium x1 y1 x2 y2)
  (multiple-value-bind (down up) (beam-offsets *font*)
    (let ((transformation (medium-transformation (medium-sheet medium)))
          (thickness (- down up)))
      (cond ((< y1 y2)
             (when (stream-recording-p (medium-sheet medium))
               (multiple-value-bind (xx1 yy1 xx2 yy2 yd yu yt)
                   (transform-beam-attributes transformation x1 y1 x2 y2
                                              down up thickness)
                 (stream-add-output-record
                  (medium-sheet medium) 
                  (make-instance 'downward-beam-output-record
                                 :x1 xx1 :y1 (+ yy1 yu) :x2 xx2 :y2 (+ yy2 yd)
                                 :thickness yt :ink (medium-ink medium)
                                 :clipping-region (transform-region transformation (medium-clipping-region medium))))))
             (when (stream-drawing-p (medium-sheet medium))
               (medium-draw-downward-beam* medium x1 (+ y1 up) x2 (+ y2 up) thickness)))
            (t
             (when (stream-recording-p (medium-sheet medium))
               (multiple-value-bind (xx1 yy1 xx2 yy2 yd yu yt)
                   (transform-beam-attributes transformation x1 y1 x2 y2
                                              down up thickness)
                 (stream-add-output-record
                  (medium-sheet medium) 
                  (make-instance 'upward-beam-output-record
                                 :x1 xx1 :y1 (+ yy2 yu) :x2 xx2 :y2 (+ yy1 yd)
                                 :thickness yt :ink (medium-ink medium)
                                 :clipping-region (transform-region transformation (medium-clipping-region medium))))))
             (when (stream-drawing-p (medium-sheet medium))
               (medium-draw-upward-beam* medium x1 (+ y1 up) x2 (+ y2 up) thickness)))))))

;;; an offset of -1 means hang, 0 means straddle and 1 means sit
(defun draw-beam (pane x1 staff-step-1 offset1 x2 staff-step-2 offset2)
  (if (> x1 x2)
      (draw-beam pane x2 staff-step-2 offset2 x1 staff-step-1 offset1)
      (multiple-value-bind (left right) (stem-offsets *font*)
        (let* ((xx1 (+ x1 left))
               (xx2 (+ x2 right))
               (offset (beam-hang-sit-offset *font*))
               (y1 (- (+ (staff-step staff-step-1) (* offset1 offset))))
               (y2 (- (+ (staff-step staff-step-2) (* offset2 offset))))
               (medium (sheet-medium pane)))
          (if (= y1 y2)
              (draw-horizontal-beam pane xx1 y1 xx2)
              (draw-sloped-beam medium xx1 y1 xx2 y2))))))

(defun draw-tie-up (pane x1 x2 staff-step)
  (let ((dist (/ (- x2 x1) (staff-step 4/3))))
    (if (> dist 19)
        (let ((xx1 (round (+ x1 (staff-step 10))))
              (xx2 (round (- x2 (staff-step 10))))
              (y1 (- (round (staff-step (+ staff-step 11/3)))))
              (thickness (round (staff-step 2/3))))
          (sdl::draw-shape pane *font* :large-tie-up-left xx1 (staff-step (- staff-step)))
          (sdl::draw-shape pane *font* :large-tie-up-right xx2 (staff-step (- staff-step)))
          (draw-rectangle* pane xx1 y1 xx2 (+ y1 thickness)))
        (let ((glyph-name (cond ((> dist 18) :large-tie-10-up)
                                ((> dist 17) :large-tie-9-up)
                                ((> dist 16) :large-tie-8-up)
                                ((> dist 15) :large-tie-7-up)
                                ((> dist 14) :large-tie-6-up)
                                ((> dist 13) :large-tie-5-up)
                                ((> dist 12) :large-tie-4-up)
                                ((> dist 11) :large-tie-3-up)
                                ((> dist 10) :large-tie-2-up)
                                ((> dist 9) :large-tie-1-up)
                                ((> dist 8) :small-tie-8-up)
                                ((> dist 7) :small-tie-7-up)
                                ((> dist 6) :small-tie-6-up)
                                ((> dist 5) :small-tie-5-up)
                                ((> dist 4) :small-tie-4-up)
                                ((> dist 3) :small-tie-3-up)
                                ((> dist 2) :small-tie-2-up)
                                (t :small-tie-1-up))))
          (sdl::draw-shape pane *font* glyph-name 
                           (round (* 0.5 (+ x1 x2))) (staff-step (- staff-step)))))))

(defun draw-tie-down (pane x1 x2 staff-step)
  (let ((dist (/ (- x2 x1) (staff-step 4/3))))
    (if (> dist 19)
        (let ((xx1 (round (+ x1 (staff-step 10))))
              (xx2 (round (- x2 (staff-step 10))))
              (y1 (- (round (staff-step (- staff-step 8/3)))))
              (thickness (round (staff-step 2/3))))
          (sdl::draw-shape pane *font* :large-tie-down-left xx1 (staff-step (- staff-step)))
          (sdl::draw-shape pane *font* :large-tie-down-right xx2 (staff-step (- staff-step)))
          (draw-rectangle* pane xx1 y1 xx2 (+ y1 thickness)))
        (let ((glyph-name (cond ((> dist 18) :large-tie-10-down)
                                ((> dist 17) :large-tie-9-down)
                                ((> dist 16) :large-tie-8-down)
                                ((> dist 15) :large-tie-7-down)
                                ((> dist 14) :large-tie-6-down)
                                ((> dist 13) :large-tie-5-down)
                                ((> dist 12) :large-tie-4-down)
                                ((> dist 11) :large-tie-3-down)
                                ((> dist 10) :large-tie-2-down)
                                ((> dist 9) :large-tie-1-down)
                                ((> dist 8) :small-tie-8-down)
                                ((> dist 7) :small-tie-7-down)
                                ((> dist 6) :small-tie-6-down)
                                ((> dist 5) :small-tie-5-down)
                                ((> dist 4) :small-tie-4-down)
                                ((> dist 3) :small-tie-3-down)
                                ((> dist 2) :small-tie-2-down)
                                (t :small-tie-1-down))))
          (sdl::draw-shape pane *font* glyph-name 
                           (round (* 0.5 (+ x1 x2))) (staff-step (- staff-step)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; convenience macros

(defmacro with-notehead-right-offsets ((right up) &body body)
  `(multiple-value-bind (,right ,up) (notehead-right-offsets *font*)
    ,@body))

(defmacro with-notehead-left-offsets ((left down) &body body)
  `(multiple-value-bind (,left ,down) (notehead-left-offsets *font*)
    ,@body))

(defmacro with-suspended-note-offset (offset &body body)
  `(let ((,offset (suspended-note-offset *font*)))
    ,@body))

(defmacro with-score-pane (pane &body body)
  `(progn
     (clear-output-record (stream-output-history ,pane))
     ,@body))

(defmacro with-vertical-score-position ((pane yref) &body body)
  `(with-translation (,pane 0 ,yref)
    ,@body))

(defmacro with-staff-size (size &body body)
  (let ((size-var (gensym)))
    `(let ((,size-var ,size))
      (unless (aref *fonts* ,size-var)
        (setf (aref *fonts* ,size-var)
              (make-font ,size-var)))
      (let ((*font* (aref *fonts* ,size-var)))
        ,@body))))  

(defmacro with-light-glyphs (pane &body body)
  `(with-drawing-options (,pane :ink (light-glyphs-ink (stream-default-view ,pane)))
     ,@body))
