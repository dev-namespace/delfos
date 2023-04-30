;;; delfos-widgets.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; Org widgets for the delfos buffer.

;;; Code:

(require 'anaphora)

(defun delfos-widgets-select-render (label options selected &optional help)
  (let* ((options-string (format "\"%s\"" (mapconcat 'identity options "\" \"")))
         (function (format "(let((choice (completing-read \"Choose an option: \" '(%s) nil t))) (setq buffer-read-only nil) (forward-line 1) (kill-line) (if (member choice '%s) (insert choice) (insert \"%s\")) (setq buffer-read-only t))" options-string (prin1-to-string options) selected))
         (link (format "[[elisp:%s][> %s]]" function label)))
    (insert link)
    (if help (insert (format " /%s/" help)))
    (insert "\n")
    (insert selected)
    (insert "\n\n")))

(defun delfos-widgets-checkbox-render (label checked)
  "Render a checkbox widget in org-mode with LABEL and a CHECKED state."
  (char-to-string (char-after (- (point-at-eol) 2)))
  (point-at-eol)
  (let* ((checkbox-symbol (if checked "X" " "))
         (toggle-function (format "(let ((current-status (char-to-string (char-after (- (point-at-eol) 2))))) (message current-status) (setq buffer-read-only nil) (save-excursion (goto-char (- (point-at-eol) 2)) (delete-char 1) (insert (if (string= current-status \" \") \"X\" \" \"))) (setq buffer-read-only t))"))
         (link (format "[[elisp:%s][%s]]" toggle-function label)))
    (insert link)
    (insert " ")
    (insert (format "[%s]" checkbox-symbol))
    (insert "\n\n")))

(defun delfos-widgets-checkbox-set-value (label value)
        "Get the selected value for the given label from the org buffer."
        (interactive "sLabel: ")
        (save-excursion
        (goto-char (point-min))
        (let ((search-pattern (format "\\[\\[elisp:.*\\]\\[%s\\]\\]" label)))
          (when (re-search-forward search-pattern nil t)
            (goto-char (- (point-at-eol) 2))
            (delete-char 1)
            (insert (if value "X" " "))))))

(defun delfos-widgets-select-set-value (label value)
  "Set the selected value for the given label in the org buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((search-pattern (format "\\[\\[elisp:.*\\]\\[> %s\\]\\]" label)))
      (when (re-search-forward search-pattern nil t)
        (forward-line)
        (setq buffer-read-only nil)
        (delete-region (line-beginning-position) (line-end-position))
        (insert value)
        (setq buffer-read-only t)))))


(defun delfos-widgets-get-value (label)
  (interactive "sLabel: ")
  (aif (delfos-widgets-select-get-value label) it
    (delfos-widgets-checkbox-get-value label)))

(defun delfos-widgets-select-get-value (label)
  "Get the selected value for the given label from the org buffer."
  (interactive "sLabel: ")
  (save-excursion
    (goto-char (point-min))
    (let ((search-pattern (format "\\[\\[elisp:.*\\]\\[> %s\\]\\]" label))
          (selected-value nil))
      (when (re-search-forward search-pattern nil t)
        (forward-line)
        (setq selected-value (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
      (message (format "select value: %s" selected-value))
      selected-value)))

(defun delfos-widgets-checkbox-get-value (label)
        "Get the selected value for the given label from the org buffer."
        (interactive "sLabel: ")
        (save-excursion
        (goto-char (point-min))
        (let ((search-pattern (format "\\[\\[elisp:.*\\]\\[%s\\]\\]" label))
              (selected-value nil))

          (when (re-search-forward search-pattern nil t)
            (setq selected-value (char-equal (char-after (- (point-at-eol) 2)) ?X)))
          selected-value)))

(defun delfos-widgets-select-get-value-boolean (label)
  (if (equal (delfos-widgets-select-get-value label) "yes") t nil))


(provide 'delfos-widgets)

;;; delfos-widgets.el ends here
