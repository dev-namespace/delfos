;;; delfos-backend.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; This file provides an interface for creating chatbot backends.

;;; Code:

(require 'cl-lib)
(require 'secrets)

(cl-defstruct delfos-backend complete-chat extract-answer models default-model)

(defun delfos-backend-prompt-secret (label &optional prompt)
  (let ((api-key (read-from-minibuffer (or prompt (format "Enter secret (%s): " label)))))
    (secrets-delete-item "login" (format "delfos-%s" label))
    (secrets-create-item "login" (format "delfos-%s" label) api-key)
    api-key))

(defun delfos-backend-get-or-prompt-secret (label &optional prompt)
  (let ((secret (delfos-backend-get-secret label)))
    (if secret
        secret
      (delfos-backend-prompt-secret label prompt))))

(defun delfos-backend-get-secret (label)
  (secrets-get-secret "login" (format "delfos-%s" label)))

(provide 'delfos-backend)
;;; delfos-backend.el ends here
