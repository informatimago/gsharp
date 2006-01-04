(in-package :clim-internals)

(defmethod replay-output-record :around (record stream &optional region (x-offset 0) (y-offset 0))
  (declare (ignore record region x-offset y-offset))
  (with-sheet-medium (medium stream)
    (letf (((medium-ink medium) (medium-ink medium))
	   ((medium-foreground medium) (medium-foreground medium))
	   ((medium-background medium) (medium-background medium))
	   ((medium-transformation medium) (medium-transformation medium))
	   ((medium-clipping-region medium) (medium-clipping-region medium))
	   ((medium-line-style medium) (medium-line-style medium))
	   ((medium-text-style medium) (medium-text-style medium))
	   ((medium-default-text-style medium) (medium-default-text-style medium)))
      (call-next-method))))

(defmethod set-medium-graphics-state :after ((state gs-clip-mixin) medium)
  (setf (medium-clipping-region medium)
	(region-intersection (medium-clipping-region medium)
			     (graphics-state-clip state))))
