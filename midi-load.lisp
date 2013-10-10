(in-package :gsharp-midi-load)

(defparameter *cache* (cons nil nil))

(defun midi-stream-p (stream)
  (if (eq stream (car *cache*))
      (cdr *cache*)
      (setf (car *cache*) stream
            (cdr *cache*) (ignore-errors
                            (let ((start (file-position stream)))
                              (unwind-protect (handler-case (read-midi-file (pathname stream))
                                                ((or unknown-event header) (midi-condition)
                                                  (error "Not a midi file ~S ~S" stream midi-condition)))
                                (file-position stream start)))))))


(assert (equal '(nil nil)
               (with-open-file (stream #P"~/works/gsharp/src/abnotation/files/bach-suite-spacing-1.gsh")
                 (list (midi-stream-p stream)
                       (midi-stream-p stream)))))





(defun read-buffer-from-midi-stream (stream)
  (let ((file (midi-stream-p stream))
        (buffer ))
    
    ))



(with-open-file (stream #P"~/works/gsharp/src/abnotation/files/1-canal.mid")
  (list (midi-stream-p stream)
        (midi-stream-p stream)))
