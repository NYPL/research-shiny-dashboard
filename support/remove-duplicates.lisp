#!/usr/local/bin/lispscript

(defvar /all-files/ (zsh "ls | sort" :split t))
(defvar /holder/ (make-hash-table :test #'equal))
(defvar /all-to-delete/ nil)


(defun get-file-base (afilename)
  (let ((adate (~e afilename •(-\d{4}-\d{2}-\d{2})\.•)))
    (when adate
      (~r afilename adate ""))))

(for-each /all-files/
  (let ((thebase (get-file-base value!)))
    (when thebase
      (with-hash-entry (/holder/ thebase)
        (push value! entry!)))))

(for-each /holder/
  (when (> (length value!) 1)
    (setf /all-to-delete/ (append /all-to-delete/ (cdr (sort value! #'string-not-lessp))))))


(if (not (null /all-to-delete/))
  (if (y-or-n-p (fn "Delete following files? ~S~%" /all-to-delete/))
    (progn
      (mapcar (lambda (x) (zsh (fn •rm "~A"• x) :echo t)) /all-to-delete/)
      (ft (green "done~%")))
    (ft (yellow "exiting without deleting~%")))
  (ft (yellow "no duplicates to delete~%")))


