;;; mullvadel.el --- Interace for Mullvad VPN -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 yelobat
;;
;; Author: yelobat <yelobat@fedora>
;; Maintainer: yelobat <yelobat@fedora>
;; Created: July 06, 2025
;; Modified: July 06, 2025
;; Version: 0.0.1
;; Keywords: transient comm tools unix processes
;; Homepage: https://github.com/yelobat/mullvadel
;; Package-Requires: ((emacs "28.1") (transient "0.9.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Interface for Mullvad VPN from within Emacs. Before you can start using the
;; Mullvad VPN client you must provide an account number. Set the
;; `mullvadel-account-number' variable to your Mullvad VPN account number (it
;; would be more secure to pass a function instead that returns the account
;; number rather than hard-coding it).
;;
;;; Code:

(require 'transient)

(defgroup mullvadel nil
  "Interface with Mullvad VPN."
  :group 'mullvad-vpn
  )

(defcustom mullvadel-account-number nil
  "Your Mullvad VPN account number."
  :type '(choice
          (string :tag "Account Number")
          (function :tag "Function that returns Account Number"))
  :group 'mullvadel
  )

(defcustom mullvadel-cli-path "mullvad"
  "Path to the Mullvad VPN CLI executable."
  :type 'file
  :group 'mullvadel
  )

(defun mullvadel-cli-command-run (args)
  "Run a Mullvad VPN CLI command."
  (let* ((command (concat mullvadel-cli-path " " (mapconcat 'shell-quote-argument args " "))))
    (message "Running Mullvad CLI: %s" command)
    (condition-case err
        (let ((output (shell-command-to-string command)))
          (string-trim-right output))
      (error
       (message "Error running Mullvad VPN CLI command '%s': %S" command err)
       (error "Mullvad CLI command failed: %S" (cdr err))))))

(defun mullvadel-account-create ()
  "Create and log in on a new account."
  (mullvadel-cli-command-run '("account" "create")))

(defun mullvadel-account-login ()
  "Log in on an account."
  (mullvadel-cli-command-run '("account" "login")))

(defun mullvadel-account-logout ()
  "Log out of the current account."
  (mullvadel-cli-command-run '("account" "logout")))

(defun mullvadel-account-get ()
  "Display information about the current account."
  (mullvadel-cli-command-run '("account" "get")))

(provide 'mullvadel)
;;; mullvadel.el ends here

(defun mullvadel-account-get ()
  "Display information about the current account."
  (mullvadel-cli-command-run '("account" "get")))

(transient-define-prefix mullvadel-menu ()
  "Mullvad VPN CLI commands."
  [["account" ; Account related commands
    ("a" "create" mullvadel-account-create)
    ("i" "login" mullvadel-account-login)
    ("l" "logout" mullvadel-account-logout)
    ("g" "get" mullvadel-account-get)
    ]])



(mullvadel-cli-command-run '("account" "get"))

(provide 'mullvadel)
;;; mullvadel.el ends here
