;;; delfos-dialogue.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; This library defines functions that implement the chat-like flow, bridging the
;; the gap between the user input, the delfos buffer and the delfos modus.

;;; Code:

(require 'cl-lib)
(require 'delfos-message)
(require 'delfos-modus)
(require 'delfos-thread)
(require 'delfos-buffer)
(require 'delfos-feedback)

(defun delfos-dialogue-on-send (msg thread-name)
  (delfos-thread-with
   thread-name
   (let ((buffer-focused-p (equal (current-buffer) (window-buffer (selected-window)))))
     (delfos-buffer-render-message msg)
     (when buffer-focused-p
       (delfos-feedback-show-spinner)))))

(defun delfos-dialogue-on-receive (answer thread-name)
  (delfos-thread-with
   thread-name
   (delfos-buffer-render-message answer)
   (delfos-feedback-play-bell)
   (delfos-feedback-hide-spinner)))

(cl-defun delfos-dialogue-send (handler msg thread-name &rest kwargs)
  "Manage query MSG feedback/render while executing delfos-mode FUNCTION."
  (unless (delfos-message-p msg)
    (error "MSG must be a delfos-message"))
  (let* ((input-callback (plist-get kwargs :callback))
         (query-callback (lambda (answer)
                           (delfos-dialogue-on-receive answer thread-name)
                           (when input-callback
                             (funcall input-callback answer))))
         (updated-kwargs (plist-put kwargs :callback query-callback)))
    (delfos-dialogue-on-send msg thread-name)
    (apply handler msg updated-kwargs)))

(defun delfos-dialogue-send-from-buffer (msg)
  (let* ((modus (delfos-modus-get (delfos-buffer-get-modus)))
        (kwargs (funcall (delfos-modus-get-config-from-buffer modus)))
        (handler (delfos-modus-process-question modus))
        (thread-name (delfos-buffer-get-title)))
    (apply #'delfos-dialogue-send handler msg thread-name kwargs)))

(defun delfos-dialogue-input (text)
  (interactive "sYour message: ")
  (cl-assert (eq major-mode 'delfos-mode))
  (delfos-dialogue-send-from-buffer (make-delfos-message-user :content text)))

(defun delfos-dialogue-input-from-buffer ()
  (interactive)
  (let ((text (delfos-input-buffer)))
    (delfos-dialogue-input text)))

(provide 'delfos-dialogue)
;;; delfos-dialogue.el ends here
