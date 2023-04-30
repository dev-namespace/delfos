;;; delfos-sidebar.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; Delfos sidebar implementation.

;;; Code:

(require 'f)
(require 'delfos-thread)

;; TODO implement sidebar updates

(defvar delfos-sidebar-buffer-name "*Delfos Sidebar*")

(defun delfos-sidebar--get-threads ()
  (f-files delfos-thread-directory
           (lambda (file)
             (string= (f-ext file) "delfos"))
           t))

(defun delfos-sidebar--open-thread (thread)
  (let ((current-point (point)))
    (message (format "point: %s" current-point))
    (with-selected-window (delfos-sidebar-find-main-window)
      (delfos-switch-to-thread thread)
      (delfos-sidebar-open))
    (goto-char current-point)))

(defun delfos-sidebar--render-thread-button (thread)
  (let ((button (insert-button thread 'action (lambda (_button) (delfos-sidebar--open-thread thread)))))
    (button-put button 'face 'default)
    button))

(defun delfos-sidebar-render ()
  "Render the sidebar."
  (setq buffer-read-only t)
  (org-mode)
  (setq display-line-numbers nil)
  (insert "Delfos Sidebar\n")
  (insert (make-string (window-body-width (get-buffer-window)) ?=) "\n\n")

  (let ((button (insert-button "[Clear threads]" 'action (lambda (_button) (delfos-thread-delete-all)))))
    (button-put button 'face '(:background "#666")))

  ;; TODO: Main first
  (insert "\n\n* Threads\n")
  (let ((threads (delfos-sidebar--get-threads)))
    (dolist (thread threads)
      (let ((thread-name (replace-regexp-in-string ".delfos" ""  (f-filename thread)) ))
        (insert "** ")
        (when (string= thread-name delfos-current-thread)
          (insert "="))
        (delfos-sidebar--render-thread-button thread-name)
        (when (string= thread-name delfos-current-thread)
          (insert "="))
        (insert "\n")))))

(defun delfos-sidebar-open ()
  "Open the Delfos sidebar buffer in a side window on the right."
  (let ((buffer (get-buffer-create delfos-sidebar-buffer-name)))
    (display-buffer-in-side-window buffer '((side . right)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (delfos-sidebar-render)))))

(defun delfos-sidebar-find-main-window ()
  "Find the main window in the current frame, excluding the Delfos sidebar."
  (let ((main-window nil))
    (dolist (window (window-list))
      (unless (string= (buffer-name (window-buffer window)) delfos-sidebar-buffer-name)
        (setq main-window window)))
    main-window))

(provide 'delfos-sidebar)

;;; delfos-sidebar.el ends here
