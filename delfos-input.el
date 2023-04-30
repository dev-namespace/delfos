;;; delfos-input.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; This library provides different ways to capture user input.

;;; Code:

;; TODO audio input

(defvar delfos-input-buffer-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'exit-recursive-edit)
    map)
  "Keymap for `delfos-input-text'.")

(defun delfos-input-buffer ()
  "Capture user input in a temporary buffer"
  (interactive)
  (let ((input-text nil)
        (original-buffer (current-buffer))
        (input-buffer (generate-new-buffer "*Delfos Input*")))
    (with-current-buffer input-buffer
      (org-mode)
      (use-local-map delfos-input-buffer-keymap)
      (setq-local mode-line-format (concat "Enter text. Press " (key-description (kbd "C-c C-c")) " when done.")))
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer input-buffer)
    (when evil-mode
      (evil-emacs-state))
    (unwind-protect
        (recursive-edit)
      (setq input-text (buffer-substring-no-properties (point-min) (point-max)))
      (kill-buffer input-buffer)
      (delete-window))
    input-text))

(defun delfos-input-format-text (input-text)
  (replace-regexp-in-string "^\\*" "***" input-text))

(provide 'delfos-input)
;;; delfos-input.el ends here
