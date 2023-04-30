;;; delfos-feedback.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; This library implements different feedback mechanisms like spinners, sounds, etc.

;;; Code:

(require 'spinner)

(defvar delfos-feedback-alarm-sound (expand-file-name ".media/alarm.wav" (file-name-directory load-file-name)))
(defvar delfos-feedback-spinner (spinner-create 'progress-bar-filled))
(defvar delfos-feedback-spinner-overlay nil)
(defvar delfos-feedback-spinner-timer nil)

(defun delfos-feedback-play-bell ()
  "Play the bell sound using the alarm sound file specified in `delfos-feedback-alarm-sound`."
  (condition-case _err
      (play-sound-file delfos-feedback-alarm-sound)
    (error (message "Failed to play sound"))))

(defun delfos-feedback-show-spinner ()
  "Display a spinner at the end of the current buffer to indicate ongoing activity."
  (if delfos-feedback-spinner-timer
      (cancel-timer delfos-feedback-spinner-timer))

  (let ((buffer (current-buffer)))
    (setq delfos-feedback-spinner-overlay (make-overlay (point-max) (point-max)))
    (spinner-start delfos-feedback-spinner)
    (overlay-put delfos-feedback-spinner-overlay 'before-string (spinner-print delfos-feedback-spinner))

    (setq delfos-feedback-spinner-timer
          (run-with-timer
           0 0.1
           (lambda ()
             (with-current-buffer buffer
               (when (overlay-buffer delfos-feedback-spinner-overlay)
                 (overlay-put delfos-feedback-spinner-overlay 'before-string (spinner-print delfos-feedback-spinner)))))))))

(defun delfos-feedback-hide-spinner ()
  "Hide the spinner previously displayed by `delfos-feedback-show-spinner`."
  (when delfos-feedback-spinner-timer
    (cancel-timer delfos-feedback-spinner-timer))
  (when delfos-feedback-spinner
    (spinner-stop delfos-feedback-spinner))
  (when delfos-feedback-spinner-overlay
    (delete-overlay delfos-feedback-spinner-overlay)))

(provide 'delfos-feedback)
;;; delfos-feedback.el ends here
