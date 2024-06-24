;;; init-mail.el --- mail -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(if (memq window-system '(mac ns))
    (add-to-list 'load-path "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e")
  (setq mu4e-mu-binary "/opt/homebrew/bin/mu"))

(require 'mu4e)

(setq user-mail-address "tony@arksolutions.it")

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      '(("smtp.gmail.com" 587 "tony@arksolutions.it" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq mu4e-maildir "~/.mail")
(setq send-mail-function 'smtpmail-send-it)

(setq mu4e-change-filenames-when-moving t)

;; Default account on startup
(setq user-full-name  "Tony Ampomah"
      mu4e-sent-folder "/tony@arksolutions.it/Sent"
      mu4e-drafts-folder "/tony@arksolutions.it/Drafts"
      mu4e-trash-folder "/tony@arksolutions.it/Trash")

;; This is set to 't' to avoid mail syncing issues when using mbsync
(setq mu4e-change-filenames-when-moving t)

;; Refresh mail using isync every 10 minutes
(setq mu4e-update-interval (* 10 60))
(setq smtpmail-debug-info t
      message-kill-buffer-on-exit t
      ;; Custom script to run offlineimap in parallel for multiple
      ;; accounts as discussed here:
      ;; http://www.offlineimap.org/configuration/2016/01/29/why-i-m-not-using-maxconnctions.html
      ;; This halves the time for checking mails for 4 accounts for me
      ;; (when nothing has to be synched anyway)
      mu4e-get-mail-command "mbsync -a"
      mu4e-attachment-dir "~/Downloads")


;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses t)

;; Do not show related messages by default (toggle with =W= works
;; anyway)
(setq mu4e-headers-include-related nil)

;; Alternatives are the following, however in first tests they
;; show inferior results
;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
;; (setq mu4e-html2text-command "html2text -utf8 -width 72")
;; (setq mu4e-html2text-command "w3m -dump -T text/html")

(defvar my-mu4e-account-alist
  '(("tony@arksolutions.it"
     (user-full-name  "Tony Ampomah")
     (mu4e-compose-signature . (concat "Many thanks\n" "Tony\n"))
     (mu4e-compose-signature-auto-include t)
     (mu4e-sent-folder "/tony@arksolutions.it/Sent")
     (mu4e-drafts-folder "/tony@arksolutions.it/Drafts")
     (mu4e-trash-folder "/tony@arksolutions.it/Trash")
     (user-mail-address "tony@arksolutions.it")
     (smtpmail-default-smtp-server "smtppro.zoho.com")
     ;; (smtpmail-local-domain "gmail.com")
     (smtpmail-smtp-user "tony@arksolutions.it")
     (smtpmail-smtp-server "smtppro.zoho.com")
     ;; (smtpmail-stream-type 'starttls)
     (smtpmail-smtp-service 587))

    ;; ("tony.ampomah.jw@gmail.com"
    ;;  (user-full-name  "Tony Ampomah")
    ;;  (mu4e-compose-signature . (concat
    ;;                             "Warm love\n"
    ;;                             "Tony\n"))
    ;;  (mu4e-compose-signature-auto-include t)
    ;;  (mu4e-sent-folder "/tony.ampomah.jw@gmail.com/[Gmail]/Sent Mail")
    ;;  (mu4e-drafts-folder "/tony.ampomah.jw@gmail.com/[Gmail]/Drafts")
    ;;  (mu4e-trash-folder "/tony.ampomah.jw@gmail.com/[Gmail]/Trash")
    ;;  (user-mail-address "tony.ampomah.jw@gmail")
    ;;  (smtpmail-default-smtp-server "smtp.gmail.com")
    ;;  ;; (smtpmail-local-domain "gmail.com")
    ;;  (smtpmail-smtp-user "tony.ampomah.jw@gmail.com")
    ;;  (smtpmail-smtp-server "smtp.gmail.com")
    ;;  ;; (smtpmail-stream-type 'starttls)
    ;;  (smtpmail-smtp-service 587))

    ;; ("itechytony@gmail.com"
    ;;  (user-full-name  "Tony Ampomah")
    ;;  (mu4e-compose-signature . (concat
    ;;                             "Regards\n"
    ;;                             "Tony\n"))
    ;;  (mu4e-compose-signature-auto-include t)
    ;;  (mu4e-sent-folder "/itechytony@gmail.com/[Gmail]/Sent Mail")
    ;;  (mu4e-drafts-folder "/itechytony@gmail.com/[Gmail]/Drafts")
    ;;  (mu4e-trash-folder "/itechytony@gmail.com/[Gmail]/Trash")
    ;;  (user-mail-address "tony.ampomah.jw@gmail")
    ;;  (smtpmail-default-smtp-server "smtp.gmail.com")
    ;;  ;; (smtpmail-local-domain "gmail.com")
    ;;  (smtpmail-smtp-user "itechytony@gmail.com")
    ;;  (smtpmail-smtp-server "smtp.gmail.com")
    ;;  ;; (smtpmail-stream-type starttls)
    ;;  (smtpmail-smtp-service 587))

    ))

;; Whenever a new mail is to be composed, change all relevant
;; configuration variables to the respective account. This method is
;; taken from the MU4E documentation:
;; http://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html#Multiple-accounts
(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))


(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
(add-hook 'mu4e-compose-mode-hook (lambda ()
                                    (ispell-change-dictionary "en_GB")))

(setq mu4e-refile-folder
      (lambda (msg)
        (cond

         ((string-match "^/tony@arksolutions.it.*"
                        (mu4e-message-field msg :maildir))
          "/tony@arksolutions.it/Archive")

         ((string-match "^/tony.tayo@theampomahs.com.*"
                        (mu4e-message-field msg :maildir))
          "/tony@arksolutions.it/Archive")

         ;; ((string-match "^/tony.ampomah.jw@gmail.com*"
         ;;                (mu4e-message-field msg :maildir))
         ;;  "/tony.ampomah.jw@gmail.com/Archives")

         ;; ((string-match "^/itechytony@gmail.com.*"
         ;;                (mu4e-message-field msg :maildir))
         ;;  "/itechytony@gmail.com/Archives")

         ;; everything else goes to /archive
         (t  "/Archives"))))

(setq mu4e-trash-folder
      (lambda (msg)
        (cond
         ((string-match "^/tony@arksolutions.it.*"
                        (mu4e-message-field msg :maildir))
          "/tony@arksolutions.it/Trash")

         ((string-match "^/tony.tayo@theampomahs.com.*"
                        (mu4e-message-field msg :maildir))
          "/tony@arksolutions.it/Trash")

         ;; ((string-match "^/tony.ampomah.jw@gmail.com*"
         ;;                (mu4e-message-field msg :maildir))
         ;;  "/tony.ampomah.jw@gmail.com/[Gmail]/Trash")

         ;; ((string-match "^/itechytony@gmail.com.*"
         ;;                (mu4e-message-field msg :maildir))
         ;;  "/itechytony@gmail.com/[Gmail]/Trash")

         (t  "/Trash"))))

;; use org structures and tables in message mode
;; (add-hook 'message-mode-hook 'turn-on-orgstruct++)
(add-hook 'message-mode-hook 'visual-fill-column-mode)

;; view HTML email in browser
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; display unread email on mode line
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

(use-package mu4e-alert)

;; (use-package org-msg
;;   :config
;;   (setq mail-user-agent 'mu4e-user-agent)
;;   (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
;; 	org-msg-startup "hidestars indent inlineimages"
;; 	org-msg-greeting-fmt "\nHi%s,\n\n"
;; 	org-msg-greeting-name-limit 3
;; 	org-msg-default-alternatives '((new		. (text html))
;; 				       (reply-to-html	. (text html))
;; 				       (reply-to-text	. (text)))
;; 	org-msg-convert-citation t
;; 	org-msg-signature "

;;  Regards,

;;  #+begin_signature
;;  --
;;  *Tony*
;;  #+end_signature")
;;   (org-msg-mode)
;;   )

(provide 'init-mail)
;;; init-mail.el ends here
