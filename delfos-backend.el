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
(require 'f)

(defvar rc-path (expand-file-name ".delfosrc" user-emacs-directory))

(make-directory user-emacs-directory t)

(cl-defstruct delfos-backend complete-chat extract-answer models default-model)

(defun delfos-backend-prompt-secret (label &optional prompt)
  (let ((api-key (read-from-minibuffer (or prompt (format "Enter secret (%s): " label)))))
    (condition-case _err
        (secrets-delete-item "login" (format "delfos-%s" label))
      (secrets-create-item "login" (format "delfos-%s" label) api-key)
      (error (with-temp-file rc-path (insert api-key))))
    api-key))

(defun delfos-backend-get-or-prompt-secret (label &optional prompt)
  (let ((secret (delfos-backend-get-secret label)))
    (if secret
        secret
      (delfos-backend-prompt-secret label prompt))))

(defun delfos-backend-get-secret (label)
  (condition-case _err
      (secrets-get-secret "login" (format "delfos-%s" label))
    (error  (condition-case _err (f-read-text rc-path)
              (error nil)))))


(provide 'delfos-backend)
;;; delfos-backend.el ends here
