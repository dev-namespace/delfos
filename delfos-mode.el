;;; delfos-mode.el --- Description -*- lexical-binding: t; -*-
;;
;;; delfos-llm.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; This file configures the delfos mode.

;;; Code:

(require 'org)

(defvar delfos-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "C-c C-a") 'delfos-hello)
    map)
  "Keymap for `delfos-mode'.")

(define-key delfos-mode-map (kbd "C-c C-m") 'delfos-dialogue-input)
(define-key delfos-mode-map (kbd "C-c C-i") 'delfos-dialogue-input-from-buffer)
(define-key delfos-mode-map (kbd "C-c C-n") 'delfos-switch-to-thread)
(define-key delfos-mode-map (kbd "C-c C-s") 'delfos-thread-save)
(define-key delfos-mode-map (kbd "C-c C-l") 'delfos-thread-load)
(define-key delfos-mode-map (kbd "C-c C-q") 'delfos-thread-clear)
(define-key delfos-mode-map (kbd "C-c C-d") 'delfos-thread-delete)
(define-key delfos-mode-map (kbd "C-c C-r") 'delfos-buffer-rename)
(define-key delfos-mode-map (kbd "C-c C-e") 'delfos-buffer-toggle-edit)
(define-key delfos-mode-map (kbd "C-c C-p") 'delfos-thread-persist-hidden)

(define-derived-mode delfos-mode org-mode "Delfos"
  (use-local-map delfos-mode-map))

(provide 'delfos-mode)
;;; delfos-mode.el ends here
