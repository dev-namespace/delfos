;;; delfos-utils.el --- General AI interface for emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; Utils.

;;; Code:

(defun delfos-utils-next-string-element (lst val)
  (let ((idx (cl-position val lst :test 'string=)))
    (if idx
        (if (= idx (- (length lst) 1))
            (car lst)
          (nth (+ idx 1) lst))
      (car lst))))

(defun delfos-utils-get-first-line ()
  "Return the content of the first line of the current buffer as a string."
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun delfos-utils-replace-regexp-in-string-odd (regexp to-string string)
  "Replace even instances of REGEXP with TO-STRING in STRING."
  (let ((counter 0))
    (replace-regexp-in-string
     regexp
     (lambda (match)
       (prog1
           (if (not (zerop (mod counter 2)))
               to-string
             match)
         (setq counter (1+ counter))))
     string)))

(provide 'delfos-utils)

;;; delfos-utils.el ends here
