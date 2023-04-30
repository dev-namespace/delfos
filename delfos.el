;;; delfos.el --- AI interaction framework -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>
;; Created: March 25, 2023
;;
;; Homepage: https://github.com/dev-namespace/delfos
;; Keywords: emacs tools gpt llm ai
;;
;; Package-Version: 0.1
;; Package-Requires: ((emacs "26.1") (dash "2.19.1") (seq "1.9"))

;;; Commentary:

;; Delfos is a framework for interacting with AI models that puts
;; considerable focus on extendability. Delfos provides a chat-like interface
;; to dialogue with chatbots and interactive functions to provide copilot
;; like features within the IDE.
;;
;; Delfos can be extended with different backends (delfos-backend.el) and modes
;; (delfos-modus.el) that provide different functionality. The llm-modus is
;; used by default providing a basic text interface for LLMs.
;;
;;; Code:

(require 'delfos-llm)
(require 'delfos-sidebar)
(require 'delfos-dialogue)
(require 'delfos-input)

(defvar delfos-directory (expand-file-name "delfos" user-emacs-directory))
(defvar delfos-default-modus delfos-llm)
(defvar delfos-current-thread "Main")

(unless (file-exists-p delfos-directory)
  (make-directory delfos-directory t))

(defun delfos-start ()
  (interactive)
  (delfos-switch-to-thread "Main")
  (delfos-sidebar-open))

;; TODO search in list with open and saved threads, plus can create new one
(defun delfos-switch-to-thread (thread-name &optional modus)
  (interactive "sThread name: ")
  (delfos-thread-open thread-name modus)
  (setq delfos-current-thread thread-name)
  (switch-to-buffer (delfos-thread-get-buffer thread-name)))

;;; Loading libraries

(provide 'delfos)

(require 'delfos-mode)
(require 'delfos-feedback)
(require 'delfos-openai)
(require 'delfos-utils)
(require 'delfos-widgets)
(require 'delfos-copilot)
(require 'delfos-message)

;;; delfos.el ends here
