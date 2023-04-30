;;; delfos-message.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; This file provides functions for composing delfos messages.

;;; Code:

(require 'cl-lib)
(require 'delfos-org)

(defvar delfos-message-separator "------------------------------------------------------------------------------------------")

(cl-defstruct delfos-message content)
(cl-defstruct delfos-message-parser predicate parser type)
(cl-defstruct (delfos-message-user (:include delfos-message)))
(cl-defstruct (delfos-message-assistant (:include delfos-message)))

(defvar delfos-message-parsers (list))

(add-to-list 'delfos-message-parsers
  (make-delfos-message-parser
   :type "user"
   :predicate (lambda (block) (string-prefix-p "~User~" (delfos-org-block-title block)))
   :parser (lambda (block) (make-delfos-message-user :content (delfos-org-block-content block)))))

(add-to-list 'delfos-message-parsers
  (make-delfos-message-parser
   :type "assistant"
   :predicate (lambda (block) (string-prefix-p "=Language Model=" (delfos-org-block-title block)))
   :parser (lambda (block) (make-delfos-message-assistant :content (delfos-org-block-content block)))))

(cl-defgeneric delfos-message-render (message))

(cl-defmethod delfos-message-render ((message delfos-message-user))
  (let ((header (concat "** ~User~ [" (format-time-string "%Y-%m-%d %H:%M:%S") "]")))
    (format "%s %s\n%s" header delfos-message-separator (delfos-message-user-content message))))

(cl-defmethod delfos-message-render ((message delfos-message-assistant))
  (let ((header "** =Language Model="))
    (format "%s\n%s" header (delfos-message-assistant-content message))))

(defun delfos-message-format-input (input-text)
  (replace-regexp-in-string "^\\*" "***" input-text))

(provide 'delfos-message)

;;; delfos-message.el ends here
