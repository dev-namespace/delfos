;;; delfos-thread.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; This file implements the delfos thread abstraction. Threads wrap buffer instances
;; that can persist through the filesystem.

;;; Code:

(require 'delfos-buffer)
(require 'cl-lib)
(require 'delfos-mode)

(defvar delfos-thread-directory (expand-file-name "delfos/threads" user-emacs-directory))
(defvar delfos-thread-list (make-hash-table :test 'equal))
(cl-defstruct delfos-thread name buffer hidden)

(unless (file-exists-p delfos-thread-directory)
  (make-directory delfos-thread-directory t))

(defmacro delfos-thread-with (thread &rest body)
  "Execute BODY in the context of THREAD, or execute BODY directly."
  `(if ,thread
       (with-current-buffer (delfos-thread-get-buffer-name ,thread)
         ,@body)
     (progn ,@body)))

(defun delfos-thread-get (thread-name)
  (gethash thread-name delfos-thread-list))

(defun delfos-thread-create (thread-name &optional buffer-name hidden)
  (let ((thread (make-delfos-thread :name thread-name
                                  :buffer (or buffer-name
                                              (delfos-thread-make-buffer-name thread-name))
                                  :hidden hidden)))
    (puthash thread-name thread delfos-thread-list)
    thread))

(defun delfos-thread-get-create (thread-name &optional buffer-name hidden)
  (let ((thread (or (delfos-thread-get thread-name)
                    (delfos-thread-create thread-name buffer-name hidden))))
    thread))

(defun delfos-thread-get-buffer (thread-name)
  ;; TODO Am I sure I want delfos-thread-get-*create*?
  (let* ((thread (delfos-thread-get-create thread-name))
         (buffer-exists (get-buffer (delfos-thread-buffer thread)))
        (buffer (get-buffer-create (delfos-thread-buffer thread))))
    (unless buffer-exists
      (with-current-buffer buffer
        ;; TODO maybe this shouldn't be here
        (delfos-mode)))
    buffer))

(defun delfos-thread-make-buffer-name (thread-name)
  "Return the buffer name for THREAD-NAME."
  (format "*Delfos*: %s" thread-name))

(defun delfos-thread-get-buffer-name (thread-name)
  (delfos-thread-buffer (delfos-thread-get thread-name)))

(defun delfos-thread-get-file (thread-name)
  (expand-file-name (concat thread-name ".delfos") delfos-thread-directory))

(defun delfos-thread-name-from-file (thread-file)
  "Convert THREAD-FILE to thread name"
  (file-name-sans-extension (file-name-nondirectory thread-file)))

(defun delfos-thread-open (thread-name &optional modus)
  (let* ((thread (delfos-thread-get-create thread-name))
         (buffer (get-buffer-create (delfos-thread-get-buffer-name thread-name))))

    (with-current-buffer buffer
        (unless (eq major-mode 'delfos-mode) (delfos-mode))

      (if (delfos-thread-file-exists-p thread-name)
          (delfos-thread-load (delfos-thread-get-file thread-name))
        (progn (funcall (delfos-modus-init-buffer (or modus delfos-default-modus)) thread-name)
               (delfos-thread-save))))))

(defun delfos-thread-clear ()
  (interactive)
  (cl-assert (eq major-mode 'delfos-mode))
  (let ((thread-name (delfos-buffer-get-title)))
    (funcall (delfos-modus-init-buffer (or (delfos-modus-get (delfos-buffer-get-modus))
                                         delfos-default-modus))
             thread-name)))

(defun delfos-thread-save (&optional thread-name)
  (interactive)
  (cl-assert (or thread-name (eq major-mode 'delfos-mode)))
  (delfos-thread-with
   thread-name
   (let* ((thread-name (concat (or thread-name (delfos-buffer-get-title)) ".delfos"))
          (thread-file (expand-file-name thread-name delfos-thread-directory)))
     (unless (file-exists-p delfos-thread-directory)
       (make-directory delfos-thread-directory t))
     (write-region (point-min) (point-max) thread-file)
     (message "Saved thread to %s" thread-file))))

(defun delfos-thread-persist-hidden (hidden-buffer)
  (interactive)
  (let ((buffer (or hidden-buffer (current-buffer)))
        (with-current-buffer buffer
          (thread-name (delfos-buffer-rename)))
        (delfos-thread-save thread-name))))


;; @TODO switching logic shouldn't go here
(defun delfos-thread-load (thread-file)
  (interactive
   (let ((default-directory delfos-thread-directory))
     (list (read-file-name "Choose a file: "))))
  (let*
      ((thread-name (delfos-thread-name-from-file thread-file))
       (thread (delfos-thread-get-create thread-name)))
    (switch-to-buffer (delfos-thread-get-buffer thread-name))
    (org-element-cache-reset)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert-file-contents thread-file)
    (setq buffer-read-only t)
    (org-element-cache-reset)))

;; @TODO switching logic shouldn't go here
(defun delfos-thread-delete (&optional thread-name)
  (interactive)
  (let* ((thread (or thread-name (delfos-buffer-get-title)))
        (file (delfos-thread-get-file thread)))
    ;; @TODO remove from delfos-thread-list
    (switch-to-buffer (delfos-thread-get-buffer "Main"))
    (kill-buffer (delfos-thread-get-buffer thread-name))
    (delete-file file)))

(defun delfos-thread-delete-all ()
  "Delete all saved delfos threads."
  (interactive)
  (let ((delfos-files (directory-files delfos-thread-directory t "\\.delfos\\'")))
    (dolist (file delfos-files)
      ;; @TODO remove threads from delfos-thread-list
      (when (file-regular-p file)
        (delete-file file)))))

(defun delfos-thread-file-exists-p (thread-name)
  (let ((thread-file (expand-file-name (concat thread-name ".delfos") delfos-thread-directory)))
    (file-exists-p thread-file)))

(provide 'delfos-thread)

;;; delfos-thread.el ends here
