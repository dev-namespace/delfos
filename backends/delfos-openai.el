;;; delfos-openai.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; Delfos backend for OpenAI API.

;;; Code:

(require 'json)
(require 'request)
(require 'delfos-utils)
(require 'cl-lib)
(require 'dash)

(require 'delfos-backend)
(require 'delfos-message)

(defvar delfos-openai-base-system-message "You're an AI assistant running inside emacs. ")
(defvar delfos-openai-base-extra-system-message "You must provide short but complete responses to the user. ")
(defvar delfos-openai-default-model "gpt-3.5-turbo")

(defconst delfos-openai-backend (make-delfos-backend
                               :models '("gpt-3.5-turbo" "gpt4")
                               :default-model delfos-openai-default-model
                               :complete-chat #'delfos-openai-complete-chat
                               :extract-answer #'delfos-openai-extract-answer-from-response))

(cl-defun delfos-openai-complete-chat (input-text callback &key model context system)
  (let* ((system-messages (delfos-openai-format-system system))
         (context-messages (delfos-openai-format-context context))
         (messages (vconcat system-messages context-messages
                            `(((role . "user")
                               (content . ,input-text))))))
    (request
      "https://api.openai.com/v1/chat/completions"
      :type "POST"
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,(concat "Bearer "
                                             (delfos-backend-get-or-prompt-secret
                                              "openai" "Enter OpenAI API key: "))))
      :data (json-encode
             `(("model" . ,(or model delfos-openai-default-model))
               ("messages" . ,messages)
               ("temperature" . 0.7)))
      :parser 'json-read
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (funcall callback (request-response-data response)))))))

(defun delfos-openai-format-system (&optional system)
  (let ((system-message-content
         (format "%s %s" delfos-openai-base-system-message
                 (or system delfos-openai-base-extra-system-message))))
    `(((role . "system") (content . ,system-message-content)))))

(defun delfos-openai-format-message (msg)
  (cond
   ((delfos-message-user-p msg)
    `((role . "user") (content . ,(delfos-message-content msg))))
   ((delfos-message-assistant-p msg)
    `((role . "assistant") (content . ,(delfos-message-content msg))))))

(defun delfos-openai-format-context (messages)
  (-map #'delfos-openai-format-message messages))

(defun delfos-openai-format-answer (input)
  (delfos-openai--format-code-block input))

(defun delfos-openai--format-code-block (input)
  (let* ((start-regexp "```\\(.*\\)")
         (end-regexp "```")
         (org-end "#+END_SRC")
         (output (delfos-utils-replace-regexp-in-string-odd end-regexp org-end input)))
    (replace-regexp-in-string
     start-regexp
     (lambda (match)
       (concat "#+BEGIN_SRC " (match-string 1 match)))
     output)))

(defun delfos-openai-extract-answer-from-response (response)
  (let* ((choices (cdr (assoc 'choices response)))
         (first-choice (aref choices 0))
         (first-message (cdr (assoc 'message first-choice)))
         (content (cdr (assoc 'content first-message)))
         (formated-content (delfos-openai-format-answer content)))
    formated-content))

(provide 'delfos-openai)

;;; delfos-openai.el ends here
