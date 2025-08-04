;;; mullvadel.el --- Interace for Mullvad VPN -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 yelobat
;;
;; Author: yelobat
;; Maintainer: yelobat
;; Created: July 06, 2025
;; Modified: July 06, 2025
;; Version: 0.0.1
;; Keywords: transient comm tools unix processes
;; Homepage: https://github.com/yelobat/mullvadel
;; Package-Requires: ((emacs "28.1") (transient "0.9.2") (compat "0.7.2"))
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
  :group 'mullvadel-vpn)

(defcustom mullvadel-account-number nil
  "Your Mullvad VPN account number."
  :type '(choice
          (string :tag "Account Number")
          (function :tag "Function that returns Account Number"))
  :group 'mullvadel)

(defcustom mullvadel-cli-path "mullvad"
  "Path to the Mullvad VPN CLI executable."
  :type 'file
  :group 'mullvadel)

(defconst mullvadel-buffer (get-buffer-create "*Mullvadel Buffer*"))
(defconst mullvadel-error-buffer (get-buffer-create "*Mullvadel Error Buffer*"))
(defconst mullvadel-process-buffer (get-buffer-create "*Mullvadel Process Buffer*"))
(defvar mullvadel-process nil)
(defvar mullvadel-process-output nil)

(defvar mullvadel-funcall-cache nil)
(defvar mullvadel-funcall-cache-source nil)
(defvar mullvadel-funcall-cache-valid-for 5)
(defun mullvadel-funcall-cache-validp ()
  "Check whether the function call cache is valid to read from."
  (if mullvadel-funcall-cache t nil))

(defun mullvadel-funcall-cache-set-timer ()
  "Set the mullvadel funcall cache timer to start."
  (run-at-time mullvadel-funcall-cache-valid-for nil
               #'mullvadel-funcall-cache-invalidate))

(defun mullvadel-funcall-cache-update (function)
  "Update the mullvadel function call cache with FUNCTION."
  (setq mullvadel-funcall-cache (funcall function))
  (setq mullvadel-funcall-cache-source (symbol-name function))
  (mullvadel-funcall-cache-set-timer)
  mullvadel-funcall-cache)

(defun mullvadel-funcall-cache-update-or-retrieve (function)
  "Update the mullvadel function call cache iff it is invalid with FUNCTION."
  (if (and (mullvadel-funcall-cache-validp) (equal (symbol-name function) mullvadel-funcall-cache-source))
      mullvadel-funcall-cache
    (mullvadel-funcall-cache-update function)))

(defun mullvadel-funcall-cache-invalidate ()
  "Invalidate the mullvadel function call cache."
  (setq mullvadel-funcall-cache nil)
  (setq mullvadel-funcall-cache-source nil))

(defvar mullvadel-data-cache nil)
(defvar mullvadel-data-cache-source nil)
(defvar mullvadel-data-cache-valid-for 5)
(defun mullvadel-data-cache-validp ()
  "Check whether the data cache is valid to read from."
  (if mullvadel-data-cache t nil))

(defun mullvadel-data-cache-set-timer ()
  "Set the mullvadel data cache timer to start."
  (run-at-time mullvadel-data-cache-valid-for nil
               #'mullvadel-data-cache-invalidate))

(defun mullvadel-data-cache-update (name data)
  "Update the mullvadel data cache with DATA with label NAME."
  (setq mullvadel-data-cache data)
  (setq mullvadel-data-cache-source name)
  (mullvadel-data-cache-set-timer)
  mullvadel-data-cache)

(defun mullvadel-data-cache-invalidate ()
  "Invalidate the mullvadel function call cache."
  (setq mullvadel-data-cache nil)
  (setq mullvadel-data-cache-source nil))

(defun mullvadel-message (format-string &rest args)
  "Display FORMAT-STRING in `mullvadel-buffer' with ARGS."
  (with-current-buffer mullvadel-buffer
    (goto-char (point-max))
    (insert (apply 'format (concat "Mullvad: " format-string "\n") args))))

(defun mullvadel-error (format-string &rest args)
  "Display FORMAT-STRING in `mullvadel-error-buffer' with ARGS."
  (with-current-buffer mullvadel-error-buffer
    (goto-char (point-max))
    (insert (apply 'format (concat "Mullvad Error: " format-string "\n") args))))

; TODO Need to balance the sentinel and filter to communicate correctly.
; the listen filter may not be finished sending output to the process buffer.
; So need to figure out a way to know when all data has truly been written.
(defun mullvadel-listen-sentinel (proc string)
  "Listen sentinel for PROC with output STRING."
  (when (memq (process-status proc) '(exit signal))
    (with-current-buffer mullvadel-process-buffer
      (setq mullvadel-process-output (mullvadel-process-buffer-string))
      (erase-buffer))))

(defun mullvadel-listen-filter (proc string)
  "Listen filter for PROC with output STRING."
  (with-current-buffer (process-buffer proc)
    (goto-char (process-mark proc))
    (setq mullvadel-process-output
          (string-trim-right (replace-regexp-in-string "\"" "" string)))
    (insert string)))

;; TODO fix this code for handling the output of the process.
;; Should realistically try and avoid `sleep-for' and see if there
;; is a better method by having `filter' and `sentinel' communicate
;; in some way.
(defun mullvadel-process-create (args)
  "Create a process instance of the Mullvad VPN CLI with ARGS."
  (interactive)
  (if (process-live-p mullvadel-process)
      (kill-process mullvadel-process))
  (setq mullvadel-process (make-process
                           :name mullvadel-cli-path
                           :buffer mullvadel-process-buffer
                           :command (append (list mullvadel-cli-path) args)
                           :sentinel 'mullvadel-listen-sentinel
                           :filter 'mullvadel-listen-filter)))

(defun mullvadel-shell-to-string (args)
  "Execute mullvadel with ARGS as a shell command, return string."
  (shell-command-to-string
   (mapconcat #'append (append (list mullvadel-cli-path) args) " ")))

(defun mullvadel-process-send (input)
  "Send INPUT to the `mullvadel-process'."
  (process-send-string mullvadel-process input)
  (while (accept-process-output mullvadel-process 0.2))
  mullvadel-process-output)

(defun mullvadel-process-buffer-string ()
  "Get the text inside of `mullvadel-process-buffer'."
  (with-current-buffer mullvadel-process-buffer
    (string-trim-right (replace-regexp-in-string "\"" "" (buffer-string)))))

(defun mullvadel-cli-command-run (args)
  "Run a Mullvad VPN CLI command with ARGS."
  (let* ((command (concat mullvadel-cli-path " " (mapconcat 'shell-quote-argument args " "))))
    (mullvadel-message "Running Mullvad CLI: %s" command)
    (condition-case err
        (let ((output (shell-command-to-string command)))
          (string-trim-right output))
      (error
       (mullvadel-error "Error running Mullvad VPN CLI command '%s' - %S" command err)
       (mullvadel-error "Mullvad CLI command failed - %S" (cdr err))))))

;; Mullvad VPN commands
(defun mullvadel-status ()
  "Return the state of the VPN tunnel."
  (interactive)
  (mullvadel-shell-to-string '("status")))

;; Mullvad VPN account commands
(defun mullvadel-account-create ()
  "Create and log in on a new account."
  (interactive)
  (mullvadel-process-create '("account" "create"))
  (process-live-p mullvadel-process))

(defun mullvadel-account-login ()
  "Log in on an account."
  (interactive)
  (mullvadel-process-create '("account" "login"))
  (if (process-live-p mullvadel-process)
      (let ((account-number (read-string (concat mullvadel-process-output " "))))
        (mullvadel-process-send (concat account-number "\n")))
    mullvadel-process-output))

(defun mullvadel-account-logout ()
  "Log out of the current account."
  (interactive)
  (mullvadel-process-create '("account" "logout"))
  mullvadel-process-output)

(defun mullvadel-account-get ()
  "Display information about the current account."
  (interactive)
  (mullvadel-shell-to-string '("account" "get")))

;; Mullvad VPN auto-connect commands
(defun mullvadel-auto-connect-get ()
  "Display the current auto-connect setting."
  (interactive)
  (mullvadel-cli-command-run '("auto-connect" "get")))

(defun mullvadel-auto-connect-set ()
  "Change auto-connect setting."
  (interactive)
  (mullvadel-cli-command-run '("auto-connect" "set")))

;; Mullvad VPN beta-program commands
(defun mullvadel-beta-program-get ()
  "Get beta notifications setting."
  (interactive)
  (mullvadel-cli-command-run '("beta-program" "get")))

(defun mullvadel-beta-program-set ()
  "Change beta notifications setting."
  (interactive)
  (mullvadel-cli-command-run '("beta-program" "set")))

(defvar mullvadel-connected nil "VPN tunnel connected?")

(defun mullvadel-connectedp ()
  "Check the connected state of the Mullvad VPN tunnel."
  (let ((output (mullvadel-status)))
    (if (equal (car (string-split output "\n")) "Connected")
        (progn
          (setq mullvadel-connected t)
          (propertize "Connected"
                      'face 'success))
      (progn
        (setq mullvadel-connected nil)
        (propertize "Disconnected"
                    'face 'error)))))

(defun mullvadel-status-features (regexp)
  "Check your Mullvad VPN status specified by REGEXP."
  (let* ((output (mullvadel-status))
         (seq (split-string output "\n"))
         (matches (mapcar (lambda (str) (string-match-p regexp str)) seq))
         (idx (cl-position-if
               (lambda (v) (or v))
               matches))
         (offset (if idx (elt matches idx) nil))
         (str (if offset (replace-regexp-in-string ":[[:space:]]*" ": " (substring (elt seq idx) offset)) nil)))
    str))

(defun mullvadel-line-from-regexp (content regexp)
  "Extract line from CONTENT based on REGEXP."
  (let* ((str (if (functionp content) (mullvadel-funcall-cache-update-or-retrieve content) content))
         (seq (split-string str "\n"))
         (matches (mapcar (lambda (str) (string-match-p regexp str)) seq))
         (idx (cl-position-if
               (lambda (v) (or v))
               matches))
         (offset (if idx (elt matches idx) nil))
         (output (if offset (mullvadel-trim-inner-ws (substring (elt seq idx) offset)) nil)))
    (mullvadel-message str)
    (mullvadel-message output)
    output))

(defun mullvadel-trim-inner-ws (content)
  "Remove inner whitespace from CONTENT."
  (replace-regexp-in-string ":[[:space:]]+" ": " content))

(defun mullvadel-visible-location ()
  "Check your percieved visible location from Mullvad VPN."
  (or (mullvadel-line-from-regexp #'mullvadel-status "Visible location:[[:space:]]*[a-zA-Z0-9-.,: ]+")
      "Visible location: None"))

(defun mullvadel-relay ()
  "Check your currently used relay from Mullvad VPN, nil if not connected."
  (or (mullvadel-line-from-regexp #'mullvadel-status "Relay:[[:space:]]*[a-zA-Z0-9-]+")
      "Relay: None"))

(defun mullvadel-features ()
  "Check the features provided by the connected Mullvad VPN relay."
  (or (mullvadel-line-from-regexp #'mullvadel-status "Features:[[:space:]]*[a-zA-Z0-9 ]+")
      "Features: None"))

;; TODO Implement this for each command so that the output of a command
;; is display inside of a transient menu.
(transient-define-prefix mullvadel-account-get-prefix ()
  ["Mullvad Account Get" :description "Display information about the current account."
    (:info (lambda ()
             (mullvadel-trim-inner-ws
              (mullvadel-line-from-regexp
               (mullvadel-funcall-cache-update-or-retrieve #'mullvadel-account-get)
               "Mullvad account:[[:space:]]*[a-zA-Z0-9-.,: ]+"))))
    (:info (lambda ()
             (mullvadel-trim-inner-ws
              (mullvadel-line-from-regexp
               (mullvadel-funcall-cache-update-or-retrieve #'mullvadel-account-get)
               "Expires at:[[:space:]]*[a-zA-Z0-9-.,: ]+"))))
    (:info (lambda ()
             (mullvadel-trim-inner-ws
              (mullvadel-line-from-regexp
               (mullvadel-funcall-cache-update-or-retrieve #'mullvadel-account-get)
               "Device name:[[:space:]]*[a-zA-Z0-9-.,: ]+"))))])

(transient-define-prefix mullvadel-account-menu ()
  "Mullvad VPN Account Commands"
  ["Account Commands" :description "Mullvad VPN Account Commands"
   ("c" "create" mullvadel-account-get-prefix
    :description "Create and log in on a new account.")
   ("l" "login" mullvadel-account-get
    :description "Log in on an account.")
   ("L" "list-devices" mullvadel-account-get
    :description "List devices associated with an account.")
   ("r" "redeem" mullvadel-account-get
    :description "Redeem a voucher.")
   ("R" "list-devices" mullvadel-account-get
    :description "Revoke a device associated with an account.")
   ("o" "logout" mullvadel-account-get
    :description "Log out of the current account.")
   ("g" "get" mullvadel-account-get
    :description "Display information about the current account.")])

(transient-define-prefix mullvadel-undefined-menu ()
  "Undefined prefix."
  ["TODO: This menu is undefined." :description "TODO: Need to implement this feature."
   (:info "This menu is empty.")])

(transient-define-prefix mullvadel-menu ()
  "Mullvad VPN commands."
  ["General" :description "Mullvad VPN Status."
    (:info #'mullvadel-connectedp)
    (:info #'mullvadel-visible-location)
    (:info #'mullvadel-features)
    (:info #'mullvadel-relay)]
  ["Mullvad VPN commands"
   [("a" "account"
    mullvadel-account-menu
    :description "Control and display information about your Mullvad account.")
   ("A" "auto-connect"
    mullvadel-undefined-menu
    :description "Control the daemon auto-connect setting.")
   ("b" "beta-program"
    mullvadel-undefined-menu
    :description "Receive notifications about beta updates.")
   ("B" "bridge"
    mullvadel-undefined-menu
    :description "Manage use of bridges, socks proxies and Shadowsocks for OpenVPN.")
   ("c" "connect"
    mullvadel-undefined-menu
    :description "Connect to a VPN relay.")
   ("d" "disconnect"
    mullvadel-undefined-menu
    :description "Disconnect from the VPN.")
   ("D" "dns"
    mullvadel-undefined-menu
    :description "Configure DNS servers to use when connected.")
   ("E" "export-settings"
    mullvadel-undefined-menu
    :description "Export a JSON patch based on the current settings.")
   ("F" "factory-reset"
    mullvadel-undefined-menu
    :description "Reset settings, caches, and logs.")
   ("r" "reconnect"
    mullvadel-undefined-menu
    :description "Reconnect to any matching VPN relay.")]
   [("R" "relay"
    mullvadel-undefined-menu
    :description "Manage relay and tunnel constraints.")
   ("u" "custom-list"
    mullvadel-undefined-menu
    :description "Manage custom lists.")
   ("i" "api-access"
    mullvadel-undefined-menu
    :description "Manage Mullvad API access methods.")
   ("I" "import-settings"
    mullvadel-undefined-menu
    :description "Apply a JSON patch generated by 'export-settings'.")
   ("o" "obfuscation"
    mullvadel-undefined-menu
    :description "Manage use of obfuscation protocols for WireGuard.")
   ("s" "split-tunnel"
    mullvadel-undefined-menu
    :description "Manage split tunneling.")
   ("t" "tunnel"
    mullvadel-undefined-menu
    :description "Manage tunnel options.")
   ("l" "lockdown-mode"
    mullvadel-undefined-menu
    :description "Control whether to block network access when disconnected from VPN.")
   ("L" "lan"
    mullvadel-undefined-menu
    :description "Control the allow local network sharing setting.")
   ("v" "version"
    mullvadel-undefined-menu
    :description "Manage tunnel options.")]])

(provide 'mullvadel)
;;; mullvadel.el ends here
