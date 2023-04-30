;;; delfos-modus.el --- Description -*- lexical-binding: t; -*-
;;
;;; delfos-llm.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; This file implements the delfos modus abstraction. A delfos modus defines what
;; happens when a message is sent to the chatbot. It also takes charge of adding
;; configuration options to the delfos buffer. A delfos buffer has one and only one
;; modus assigned.

;;; Code:

(require 'cl-lib)

(cl-defstruct delfos-modus name process-question init-buffer get-config-from-buffer)

(defvar delfos-modus-list (make-hash-table :test 'equal))

(defun delfos-modus-set (key value)
  (puthash key value delfos-modus-list))

(defun delfos-modus-get (key)
  (gethash key delfos-modus-list))

(provide 'delfos-modus)
;;; delfos-modus.el ends here
