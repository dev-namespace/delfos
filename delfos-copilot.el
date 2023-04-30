;;; delfos-copilot.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 namespace
;;
;; Author: namespace <src.namespace@gmail.com>
;; Maintainer: namespace <src.namespace@gmail.com>

;;; Commentary:

;; This library defines functions for interacting with the Delfos system anywhere.
;; Some commands provide an interface to include the current region or buffers
;; as input context.

;;; Code:

(require 'delfos-message)
(require 'delfos-dialogue)
(require 'delfos-llm)
(require 'delfos-input)

;; @TODO recover this
;; (defvar hive-copilot-ask-japanese-system "A student is learning japanese. Your objective is to keep track of the\nvocabulary and grammar he doesn't understand yet (if any!) and generate a JSON\nas output.\n\nEXAMPLE INPUT ---\nteacher: Ohayou gozaimasu! Kyou no tenki wa dou desu ka?\nstudent: ii desu :)\nteacher: Sore wa yokatta! Kyou no sentaku wa nani wo suru ka kimemashita ka?\nstudent: what is suru and kimemashita?\nteacher: \"suru\" (する) means \"to do\" and \"kimemashita\" (決めました) means \"decided\" or \"made a decision.\" So, in the sentence \"Kyou no sentaku wa nani wo suru ka kimemashita ka?\", I am asking if you have decided what you will do for today's choice or activity.\nstudent: Sou ka. Eto... kyou no activity wa sushi o taberu {with} watashi no onichan\nteacher: Naruhodo. Eto... kyou no katsudou wa sushi o taberu to watashi no oniisan desu ne.\nstudent: what does that mean?\nteacher: My response, \"Naruhodo. Eto... kyou no katsudou wa sushi o taberu to watashi no oniisan desu ne.\" means \"I see. So... today's activity is eating sushi with your older brother, right?\" In this sentence, I used \"to\" (と) to connect \"sushi o taberu\" (eating sushi) and \"watashi no oniisan\" (your older brother) as you wanted to say \"with.\"\nstudent: wakaru\nteacher: Sore wa ii! Sorezore no sushi no tokuchou ni tsuite hanashimashou ka?\nstudent: what does tsuite and hanashimashou mean?\nteacher: \"tsuite\" (ついて) means \"about\" or \"regarding,\" and \"hanashimashou\" (話しましょう) means \"let's talk\" or \"let's discuss.\" So, in the sentence \"Sorezore no sushi no tokuchou ni tsuite hanashimashou ka?\", I am asking if you would like to talk or discuss the characteristics or features of each type of sushi.\nstudent: ie. kaeru {now}.\nteacher: Wakarimashita. Ja, mata ne!\n\nEXAMPLE OUTPUT ---\n{\n  \"new_vocabulary\": [\"sure\", \"kimemashita\", \"katsudou\", \"oniisan\", \"wakaru\",\n\"hanashimashou\", \"tsuite\", \"ima\"],\n  \"grammar\": [\"use 'to' to connect actions with people\"]\n}")

(defconst delfos-copilot-thread "[Delfos Copilot]")
(defconst delfos-copilot-buffer-name "*Delfos copilot buffer*")

(defun delfos-copilot-thread-create (&optional refresh model context)
  (delfos-thread-create delfos-copilot-thread delfos-copilot-buffer-name t)
  (let ((buffer-exists-p (get-buffer delfos-copilot-buffer-name))
        (buffer (delfos-thread-get-buffer delfos-copilot-thread)))
    (with-current-buffer buffer
      (unless (and buffer-exists-p (not refresh))
        (delfos-llm-init-buffer delfos-copilot-thread model context)))))

(defun delfos-copilot-thread-display (_response)
  (interactive)
  (let ((buffer-exists-p (get-buffer delfos-copilot-buffer-name))
        (buffer (delfos-thread-get-buffer delfos-copilot-thread))
        (display-buffer-alist
         (cons '("\\*Delfos copilot buffer\\*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 100))
               display-buffer-alist)))
    (with-current-buffer buffer
      (view-mode)
      (unless buffer-exists-p
        (delfos-llm-init-buffer delfos-copilot-thread)))
    (pop-to-buffer buffer)))

(defun delfos-copilot-ask (text)
  (interactive "sYour message: ")
  (let* ((msg (make-delfos-message-user :content text))
         (thread delfos-copilot-thread))
    (delfos-copilot-thread-create t)
    (delfos-dialogue-send #'delfos-llm-process-question msg thread
                :context (list (make-delfos-message-user :content "what is the weather like?\n"))
                :callback #'delfos-copilot-thread-display
                :model "gpt-4")))

(defun delfos-copilot-ask-with-input-buffer ()
  (interactive)
  (let* ((text (delfos-input-buffer)))
    (delfos-copilot-ask text)))

(defun delfos-copilot-ask-about-region ()
  (interactive)
  (if (use-region-p)
      (let* ((region-content (buffer-substring-no-properties (region-beginning) (region-end)))
             (msg (make-delfos-message-user
                   :content (read-from-minibuffer "Question: ")))
             (context (list (make-delfos-message-user :content region-content))))
        (delfos-copilot-thread-create t "gpt4")
        (delfos-dialogue-send #'delfos-llm-process-question msg delfos-copilot-thread
                    :context context
                    :callback 'delfos-copilot-thread-display
                    :model "gpt-4"))
    (message "No region selected.")))

(defvar hive-copilot-modify-region-system-message "You will receive a buffer REGION and your task is to return a replacement that closely follows the INSTRUCTIONS. . Do not provide any explanations. Do not create code blocks: only return the text that will replace the selected region.")
(defvar hive-copilot-modify-region-with-file-context-system-message "You will receive a buffer REGION and FILE-CONTENT to give context to that REGION. your task is to return a replacement for the REGION that closely follows the INSTRUCTIONS. Do not provide any explanations. Do not create code blocks: only return the text that will replace the selected REGION")

;; TODO abstract replace in region, etc. and move to delfos-input.
(defun delfos-copilot-ask-modify-region ()
  (interactive)
  (let* ((buffer (current-buffer))
         (start (make-marker))
         (end (make-marker))
         (region-content (buffer-substring-no-properties (region-beginning) (region-end)))
         (indentation (current-indentation))
         (instructions (read-from-minibuffer "Instructions: "))
         (question (make-delfos-message-user
                 :content (format "REGION:%s\n INSTRUCTIONS:%s" region-content instructions)))
         (callback (lambda (answer)
                     (let ((replacement (delfos-message-content answer)))
                       (with-current-buffer buffer
                         (delete-region (marker-position start) (marker-position end))
                         (save-excursion
                           (goto-char (marker-position start))
                           (insert (concat (make-string indentation ?\s) replacement "\n"))))))))
    (set-marker start (if (use-region-p) (region-beginning) (point)))
    (set-marker end (if (use-region-p) (region-end) (point)))
    (set-marker-insertion-type start t)
    (delfos-copilot-thread-create t "gpt-4")
    (delfos-dialogue-send #'delfos-llm-process-question question delfos-copilot-thread
                :callback callback
                :system delfos-copilot-modify-region-system-message
                :model "gpt-4")))

(defun delfos-copilot-ask-modify-region-with-file-context ()
  (interactive)
  (let* ((buffer (current-buffer))
         (start (make-marker))
         (end (make-marker))
         (region-content (buffer-substring-no-properties (region-beginning) (region-end)))
         (indentation (current-indentation))
         (instructions (read-from-minibuffer "Instructions: "))
         (query (make-delfos-message-user
                 :content
                 (format "REGION:%s\n INSTRUCTIONS:%s\n FILE-CONTENT:%s"
                         region-content
                         instructions
                         (buffer-string))))
         (callback (lambda (answer)
                     (let ((replacement (delfos-message-content answer)))
                       (with-current-buffer buffer
                         (delete-region (marker-position start) (marker-position end))
                         (save-excursion
                           (goto-char (marker-position start))
                           (insert (concat (make-string indentation ?\s) replacement "\n"))))))))
    (set-marker start (if (use-region-p) (region-beginning) (point)))
    (set-marker end (if (use-region-p) (region-end) (point)))
    (set-marker-insertion-type start t)
    (delfos-copilot-thread-create t "gpt-4")
    (delfos-dialogue-send #'delfos-llm-process-question query delfos-copilot-thread
                :callback callback
                :system delfos-copilot-modify-region-with-file-context-system-message
                :model "gpt-4")))

(provide 'delfos-copilot)
;;; delfos-copilot.el ends here
