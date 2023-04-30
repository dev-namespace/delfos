;;; delfos-llm.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; Basic delfos modus for interacting with LLMs.

;;; Code:

(require 'cl-lib)

(require 'delfos-buffer)
(require 'delfos-widgets)
(require 'delfos-thread)
(require 'delfos-message)
(require 'delfos-feedback)
(require 'delfos-modus)
(require 'delfos-backend)
(unless (featurep 'delfos-openai)
  (load (expand-file-name "backends/delfos-openai.el" (file-name-directory load-file-name))))

(defconst delfos-llm (make-delfos-modus :name "llm"
                                    :process-question 'delfos-llm-process-question
                                    :init-buffer 'delfos-llm-init-buffer
                                    :get-config-from-buffer 'delfos-llm-extract-buffer-config))

(delfos-modus-set "llm" delfos-llm)

;; TODO enable other backends
(defconst delfos-llm-backend delfos-openai-backend)
(defconst delfos-llm-model-options (delfos-backend-models delfos-llm-backend))

(defvar delfos-llm-instructions-section
  (make-delfos-buffer-section
   :title "Instructions"
   :render-function #'delfos-llm-render-instructions))

(defun delfos-llm-render-instructions ()
  (insert "Press ~C-c C-m~ to ask a question\n")
  (insert "Press ~C-c C-n~ to create a new thread\n")
  (insert "Press ~C-c C-d~ to delete thread\n"))

(defun delfos-llm-render-config (&optional model context)
  (delfos-widgets-select-render "Model" delfos-llm-model-options (or model "gpt-3.5-turbo"))
  (delfos-widgets-checkbox-render "Context" (or context nil)))

(defun delfos-llm-extract-buffer-config ()
  (cl-assert (equal (delfos-buffer-get-modus) "llm") nil "Not a llm buffer")
  (let* ((model (delfos-widgets-select-get-value "Model"))
        (context (delfos-widgets-checkbox-get-value "Context"))
        (kwargs (list :model model)))
    (when context
      (plist-put kwargs :context (delfos-buffer-get-messages)))))

(defun delfos-llm-init-buffer (&optional title model context)
  (let ((instructions-section delfos-llm-instructions-section)
        (config-section (make-delfos-buffer-section
                         :title "LLM Config"
                         :render-function (lambda () (delfos-llm-render-config model context)))))
    (delfos-buffer-init title "llm" (list instructions-section config-section))))

(defun delfos-llm--extract-answer (response)
  (funcall (delfos-backend-extract-answer delfos-llm-backend) response))

(cl-defun delfos-llm-send (msg callback &key context model system)
  (funcall (delfos-backend-complete-chat delfos-llm-backend) msg callback
           :model model
           :context context
           :system system))

(cl-defun delfos-llm-process-question (msg &key callback model context system)
  (let ((callback (lambda (response)
                    (let ((answer (make-delfos-message-assistant
                                   :content (delfos-llm--extract-answer response))))
                      (when callback
                        (funcall callback answer))))))
    (delfos-llm-send (delfos-message-content msg) callback
                   :model model
                   :context context
                   :system system)))

;; Commands

(defun delfos-llm-toggle-context ()
  "Toggle context for the current buffer."
  (interactive)
  (let ((buffer-read-only nil))
    (delfos-widgets-checkbox-set-value "Context" (not (delfos-widgets-checkbox-get-value "Context")))))

(defun delfos-llm-set-model ()
  "Set the model for the current buffer."
  (interactive)
  (let ((choice (completing-read "Choose an option" delfos-llm-model-options nil t))
        (buffer-read-only nil))
    (delfos-widgets-select-set-value "Model" choice)))

(defun delfos-llm-rotate-model ()
  "Rotate the model for the current buffer."
  (interactive)
  (let* ((current-model (delfos-widgets-select-get-value "Model"))
         (buffer-read-only nil)
         (next-model (delfos-utils-next-string-element delfos-llm-model-options current-model)))
    (delfos-widgets-select-set-value "Model" next-model)))
(provide 'delfos-buffer)


(provide 'delfos-llm)

;;; delfos-llm.el ends here
