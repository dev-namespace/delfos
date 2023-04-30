;;; delfos-buffer.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; This library defines functions and commands for rendering and interacting
;; with the delfos buffer.

;;; Code:

(require 'dash)
(require 'delfos-utils)
(require 'delfos-widgets)
(require 'cl-lib)

(cl-defstruct delfos-buffer-section title render-function)

;; @TODO cl-defun for kwargs
(defun delfos-buffer-init (&optional buffer-title modus sections)
  (setq buffer-read-only nil)
  (erase-buffer)
  (delfos-buffer-render-header buffer-title)
  (when modus
    (delfos-buffer-render-modus modus))
  (insert "\n")
  (when sections
    (-each sections #'delfos-buffer-render-section)
    (delfos-buffer-render-message-section)
    (setq buffer-read-only t)))

(defun delfos-buffer-render-header (&optional buffer-title)
  (insert (format "%s\n" (or buffer-title "Unnamed")))
  (insert "============================================================\n"))

(defun delfos-buffer-render-modus (modus-name)
  (insert "Mode: ")
  (insert (format "%s\n" modus-name)))

(defun delfos-buffer-render-plugins (plugins)
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (plugins-rendered-p (string-match-p "Plugins:" line)))
    (if plugins-rendered-p (delete-region (line-beginning-position) (line-end-position))
      (insert "Plugins: ")
      (-each plugins (lambda (plugin) (insert (format "%s | " plugin))))
      (delete-char -3)
      (insert "\n"))))

(defun delfos-buffer-render-section (section)
  (insert (format "* %s\n" (delfos-buffer-section-title section)))
  (funcall (delfos-buffer-section-render-function section))
  (insert "\n"))

(defun delfos-buffer-render-message-section ()
  (insert "* Messages\n"))

(defun delfos-buffer-render-message (msg)
  (let ((buffer-read-only nil))
    (goto-char (point-max))
    (insert (delfos-message-render msg))
    (insert "\n\n")))

(defun delfos-buffer-get-modus ()
  (interactive)
  (save-excursion
    (goto-line 3)
    (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (if (string-match-p "Mode: " line)
          (let* ((modus (replace-regexp-in-string "Mode: " "" line)))
            modus)
        nil))))

(defun delfos-buffer-get-plugins ()
  (interactive)
  (save-excursion
    (goto-line 4)
    (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (if (string-match-p "Plugins:" line)
          (let* ((plugin-string (replace-regexp-in-string "Plugins: " "" line))
                 (plugins (split-string plugin-string " | ")))
            plugins)
        nil))))

(defun delfos-buffer-get-messages (&optional parser-list)
  (let ((blocks (delfos-org-get-subheadings "Messages"))
        (parsers (or parser-list delfos-message-parsers)))
    (seq-map (lambda (block)
               (let ((parser (-find
                              (lambda (parser) (funcall (delfos-message-parser-predicate parser) block))
                              parsers)))
                 (when parser (funcall (delfos-message-parser-parser parser) block))))
             blocks)))

(defun delfos-buffer-get-title ()
  (delfos-utils-get-first-line))

(defun delfos-buffer-rename ()
  "Rename the current buffer."
  (interactive)
  (let ((new-name (read-from-minibuffer "New name: ")))
    (save-excursion
      (goto-char (point-min))
      (setq buffer-read-only nil)
      (delete-region (point) (line-end-position))
      (insert new-name)
      (setq buffer-read-only t))
    new-name))

(defun delfos-buffer-toggle-edit ()
  "Toggle edit mode for the current buffer."
  (interactive)
  (setq buffer-read-only (not buffer-read-only)))

(provide 'delfos-buffer)

;;; delfos-buffer.el ends here
