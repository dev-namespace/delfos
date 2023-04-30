;;; delfos-org.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; Org-related utilities.

;;; Code:

(require 'org)
(require 'cl-lib)

(cl-defstruct delfos-org-block title content block)

(defun delfos-org-get-subheadings (parent-heading)
  (interactive "sParent heading: ")
  (let ((ast (org-element-parse-buffer 'headline)))
    (org-element-map ast 'headline
      (lambda (headline)
        (let* ((title (org-element-property :raw-value headline))
               (begin (org-element-property :begin headline))
               (content-begin (org-element-property :contents-begin headline))
               (end (org-element-property :contents-end headline))
               (parent (org-element-property :parent headline))
               (parent-title (org-element-property :raw-value parent)))
          (when (and parent (string= parent-heading parent-title))
            (make-delfos-org-block :title title
                                 :content (buffer-substring-no-properties content-begin end)
                                 :block (buffer-substring-no-properties begin end))))))))


(defun delfos-org-format-code-block (input)
  "Convert code blocks in INPUT string to Org-mode code blocks."
  (let* ((start-regexp "```\\(.*\\)")
         (end-regexp "```")
         (org-end "\n#+END_SRC"))
    (replace-regexp-in-string
     start-regexp
     (lambda (match)
       (concat "#+BEGIN_SRC " (match-string 1 match) "\n"))
     (s-replace-all `((,end-regexp . ,org-end))
                    input))))

(provide 'delfos-org)
;;; delfos-org.el ends here
