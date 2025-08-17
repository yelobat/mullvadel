;;; mullvadel.el --- Interace for Mullvad VPN -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 yelobat
;;
;; Author: yelobat
;; Maintainer: yelobat
;; Created: July 06, 2025
;; Modified: July 06, 2025
;; Version: 0.0.1
;; Keywords: transient comm tools unix processes mullvad vpn
;; Homepage: https://github.com/yelobat/mullvadel
;; Package-Requires: ((emacs "28.1") (transient "0.9.2") (compat "0.7.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Interface for Mullvad VPN from within Emacs. The only necessary function
;; is `mullvadel-menu' which presents the primary mullvadel transient UI.
;;
;;; Code:

(require 'transient)

(defgroup mullvadel nil
  "Interface with Mullvad VPN."
  :group 'mullvad-vpn)

(defcustom mullvadel-cli-path "mullvad"
  "Path to the Mullvad VPN CLI executable."
  :type 'file
  :group 'mullvadel)

(defconst mullvadel--buffer (get-buffer-create "*Mullvadel Buffer*"))
(defconst mullvadel--error-buffer (get-buffer-create "*Mullvadel Error Buffer*"))
(defvar mullvadel--process-output nil)

(defvar mullvadel--funcall-cache nil)
(defvar mullvadel--funcall-cache-source nil)
(defvar mullvadel--funcall-cache-valid-for 5)
(defun mullvadel--funcall-cache-validp ()
  "Check whether the function call cache is valid to read from."
  (if mullvadel--funcall-cache t nil))

(defun mullvadel--funcall-cache-set-timer ()
  "Set the mullvadel funcall cache timer to start."
  (run-at-time mullvadel--funcall-cache-valid-for nil
               #'mullvadel--funcall-cache-invalidate))

(defun mullvadel--funcall-cache-update (function)
  "Update the mullvadel function call cache with FUNCTION."
  (setq mullvadel--funcall-cache (funcall function))
  (setq mullvadel--funcall-cache-source (symbol-name function))
  (mullvadel--funcall-cache-set-timer)
  mullvadel--funcall-cache)

(defun mullvadel--funcall-cache-update-or-retrieve (function)
  "Update the mullvadel function call cache iff it is invalid with FUNCTION."
  (if (and (mullvadel--funcall-cache-validp) (equal (symbol-name function) mullvadel--funcall-cache-source))
      mullvadel--funcall-cache
    (mullvadel--funcall-cache-update function)))

(defun mullvadel--funcall-cache-invalidate ()
  "Invalidate the mullvadel function call cache."
  (setq mullvadel--funcall-cache nil)
  (setq mullvadel--funcall-cache-source nil))

(defvar mullvadel--data-cache nil)
(defvar mullvadel--data-cache-source nil)
(defvar mullvadel--data-cache-valid-for 5)
(defun mullvadel--data-cache-validp ()
  "Check whether the data cache is valid to read from."
  (if mullvadel--data-cache t nil))

(defun mullvadel--data-cache-set-timer ()
  "Set the mullvadel data cache timer to start."
  (run-at-time mullvadel--data-cache-valid-for nil
               #'mullvadel--data-cache-invalidate))

(defun mullvadel--data-cache-update (data name)
  "Update the mullvadel data cache with DATA with label NAME."
  (setq mullvadel--data-cache data)
  (setq mullvadel--data-cache-source name)
  (mullvadel--data-cache-set-timer)
  mullvadel--data-cache)

(defun mullvadel--data-cache-invalidate ()
  "Invalidate the mullvadel function call cache."
  (setq mullvadel--data-cache nil)
  (setq mullvadel--data-cache-source nil))

(defun mullvadel--message (format-string &rest args)
  "Display FORMAT-STRING in `mullvadel--buffer' and at the bottom of the screen with ARGS."
  (let ((msg (apply 'format (concat "Mullvad: " format-string "\n") args)))
    (with-current-buffer mullvadel--buffer
      (goto-char (point-max))
      (insert msg))))

(defun mullvadel--error (format-string &rest args)
  "Display FORMAT-STRING in `mullvadel--error-buffer' with ARGS."
  (with-current-buffer mullvadel--error-buffer
    (goto-char (point-max))
    (insert (apply 'format (concat "Mullvad Error: " format-string "\n") args))))

(defun mullvadel--shell-to-string (args)
  "Execute mullvadel with ARGS as a shell command, return string."
  (shell-command-to-string
   (mapconcat #'append (append (list mullvadel-cli-path) args) " ")))

;; Mullvad VPN commands
(defun mullvadel--status-sync ()
  "Return the state of the VPN tunnel."
  (interactive)
  (mullvadel--shell-to-string '("status")))

(defun mullvadel-account-get ()
  "Display information about the current account."
  (interactive)
  (message (mullvadel--shell-to-string '("account" "get"))))

(defvar mullvadel-connected nil "VPN tunnel connected?")

(defun mullvadel--connectedp ()
  "Check the connected state of the Mullvad VPN tunnel."
  (let ((output (mullvadel--status-sync)))
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
  (let* ((output (mullvadel--status-sync))
         (seq (split-string output "\n"))
         (matches (mapcar (lambda (str) (string-match-p regexp str)) seq))
         (idx (cl-position-if
               (lambda (v) (or v))
               matches))
         (offset (if idx (elt matches idx) nil))
         (str (if offset (replace-regexp-in-string ":[[:space:]]*" ": " (substring (elt seq idx) offset)) nil)))
    str))

(defun mullvadel--line-from-regexp (content regexp)
  "Extract line from CONTENT based on REGEXP."
  (let* ((str (if (functionp content) (mullvadel--funcall-cache-update-or-retrieve content) content))
         (seq (split-string str "\n"))
         (matches (mapcar (lambda (str) (string-match-p regexp str)) seq))
         (idx (cl-position-if
               (lambda (v) (or v))
               matches))
         (offset (if idx (elt matches idx) nil))
         (output (if offset (mullvadel--trim-inner-ws (substring (elt seq idx) offset)) nil)))
    (mullvadel--message str)
    (mullvadel--message output)
    output))

(defun mullvadel--trim-inner-ws (content)
  "Remove inner whitespace from CONTENT."
  (replace-regexp-in-string ":[[:space:]]+" ": " content))

(defun mullvadel--visible-location ()
  "Check your percieved visible location from Mullvad VPN."
  (or (mullvadel--line-from-regexp #'mullvadel--status-sync "Visible location:[[:space:]]*[a-zA-Z0-9-.,: ]+")
      "Visible location: None"))

(defun mullvadel--relay ()
  "Check your currently used relay from Mullvad VPN, nil if not connected."
  (or (mullvadel--line-from-regexp #'mullvadel--status-sync "Relay:[[:space:]]*[a-zA-Z0-9-]+")
      "Relay: None"))

(defun mullvadel--features ()
  "Check the features provided by the connected Mullvad VPN relay."
  (or (mullvadel--line-from-regexp #'mullvadel--status-sync "Features:[[:space:]]*[a-zA-Z0-9 ]+")
      "Features: None"))

(transient-define-prefix mullvadel-account-get-prefix ()
  ["Mullvad Account Get" :description "Display information about the current account."
   (:info (lambda ()
            (mullvadel--trim-inner-ws
             (mullvadel--line-from-regexp
              (mullvadel--funcall-cache-update-or-retrieve #'mullvadel-account-get)
              "Mullvad account:[[:space:]]*[a-zA-Z0-9-.,: ]+"))))
   (:info (lambda ()
            (mullvadel--trim-inner-ws
             (mullvadel--line-from-regexp
              (mullvadel--funcall-cache-update-or-retrieve #'mullvadel-account-get)
              "Expires at:[[:space:]]*[a-zA-Z0-9-.,: ]+"))))
   (:info (lambda ()
            (mullvadel--trim-inner-ws
             (mullvadel--line-from-regexp
              (mullvadel--funcall-cache-update-or-retrieve #'mullvadel-account-get)
              "Device name:[[:space:]]*[a-zA-Z0-9-.,: ]+"))))])

(defun mullvadel--run-async (args &optional regex-handlers on-complete buffer-name)
  "Run Mullvad CLI with ARGS asynchronously.
REGEX-HANDLERS is an alist of (REGEXP . FUNCTION) pairs.
ON-COMPLETE is called with the process object when its REGEXP matches.
BUFFER-NAME is the name of the process buffer (default: \"*Mullvadel Process*\")."
  (let* ((output "")
         (buf (get-buffer-create (or buffer-name "*Mullvadel Process*")))
         (proc nil)
         (triggered-regexps (make-hash-table :test 'equal)))
    (setq proc (make-process
                :name "mullvadel-process"
                :buffer buf
                :command (append (list mullvadel-cli-path) args)
                :filter
                (lambda (proc string)
                  (setq output (concat output string))
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (goto-char (point-max))
                      (insert string)))
                  (dolist (handler regex-handlers)
                    (let ((regexp (car handler))
                          (function (cdr handler)))
                      (when (and (string-match-p regexp output)
                                 (not (gethash regexp triggered-regexps)))
                        (puthash regexp t triggered-regexps)
                        (run-at-time 0 nil function proc)))))
                :sentinel
                (lambda (proc _string)
                  (when (not (process-live-p proc))
                    (let ((exit-status
                           (condition-case nil
                               (process-exit-status proc)
                             (error nil))))
                      (when on-complete
                        (funcall on-complete exit-status (string-trim output))
                        (with-current-buffer buf
                          (erase-buffer))))))))
    proc))

(defun mullvadel--status-async ()
  "Fetch Mullvad status asynchronously."
  (interactive)
  (mullvadel--run-async '("status") nil (lambda (_exit-status output)
                                          (message output)) nil))

(defun mullvadel--account-get-async ()
  "Fetch information about the current account asynchronously."
  (interactive)
  (mullvadel--run-async '("account" "get") nil (lambda (_exit-status output)
                                                 (message output)) nil))

(defun mullvadel--account-create-async ()
  "Create and log in on a new account asynchronously."
  (interactive)
  (mullvadel--run-async '("account" "create") nil (lambda (_exit-status output)
                                                    (message output)) nil))

(defun mullvadel--account-list-devices-async ()
  "Fetch devices associated with an account asynchronously."
  (interactive)
  (mullvadel--run-async '("account" "list-devices") nil (lambda (_exit-status output)
                                                          (message output)) nil))

(defun mullvadel--account-revoke-device-async ()
  "Revoke a device associated with an account asynchronously."
  (interactive)
  (let* ((input (read-string "Enter an account to revoke: ")))
    (mullvadel--run-async `("account" "revoke-device" ,input) nil (lambda (_exit-status output)
                                                                    (message output)) nil)))

(defun mullvadel--account-redeem-async ()
  "Revoke a device associated with an account asynchronously."
  (interactive)
  (let* ((input (read-string "Enter a voucher to redeem: ")))
    (mullvadel--run-async `("account" "redeem" ,input) nil (lambda (_exit-status output)
                                                             (message output)) nil)))

(defun mullvadel--account-login-async ()
  "Log in to a Mullvad account asynchronously."
  (interactive)
  (mullvadel--run-async
   '("account" "login")
   `((,"Enter an account number:[[:space:]]*"
      . ,(lambda (proc)
           (condition-case nil
               (let ((acc (read-string "Mullvad account number: ")))
                 (when (and acc (not (string-empty-p acc)))
                   (process-send-string proc (concat acc "\n"))))
             (quit
              (message "Mullvad login cancelled")
              (when (process-live-p proc)
                (ignore-errors (process-send-eof proc))))))))
   (lambda (status _output)
     (if (and status (zerop status))
         (message "Mullvad login successful")
       (message "Mullvad login failed")))
   "*Mullvadel Login Buffer*"))

(defun mullvadel--account-logout-async ()
  "Log out of the current account asynchronously."
  (interactive)
  (mullvadel--run-async '("account" "logout") nil (lambda (_exit-status output)
                                                    (message output)) nil))

(transient-define-prefix mullvadel--account-menu ()
  "Mullvad VPN Account Commands"
  ["Account Commands" :description "Mullvad VPN Account Commands"
   ("c" "create" mullvadel--account-create-async
    :description "Create and log in on a new account.")
   ("l" "login" mullvadel--account-login-async
    :description "Log in on an account.")
   ("o" "logout" mullvadel--account-logout-async
    :description "Log out of the current account.")
   ("g" "get" mullvadel--account-get-async
    :description "Display information about the current account.")
   ("L" "list-devices" mullvadel--account-list-devices-async
    :description "List devices associated with an account.")
   ("R" "revoke-device" mullvadel--account-revoke-device-async
    :description "Revoke a device associated with an account.")
   ("r" "redeem" mullvadel--account-redeem-async
    :description "Redeem a voucher.")])

(transient-define-prefix mullvadel--undefined-menu ()
  "Undefined prefix."
  ["TODO: This menu is undefined." :description "TODO: Need to implement this feature."
   (:info "TODO: This menu is empty.")])

(transient-define-prefix mullvadel-menu ()
  "Mullvad VPN commands."
  ["General" :description "Mullvad VPN Status."
   (:info #'mullvadel--connectedp)
   (:info #'mullvadel--visible-location)
   (:info #'mullvadel--features)
   (:info #'mullvadel--relay)]
  ["Mullvad VPN commands"
   [("a" "account"
     mullvadel--account-menu
     :description "Control and display information about your Mullvad account.")
    ("A" "auto-connect"
     mullvadel--undefined-menu
     :description "Control the daemon auto-connect setting.")
    ("b" "beta-program"
     mullvadel--undefined-menu
     :description "Receive notifications about beta updates.")
    ("B" "bridge"
     mullvadel--undefined-menu
     :description "Manage use of bridges, socks proxies and Shadowsocks for OpenVPN.")
    ("c" "connect"
     mullvadel--undefined-menu
     :description "Connect to a VPN relay.")
    ("d" "disconnect"
     mullvadel--undefined-menu
     :description "Disconnect from the VPN.")
    ("D" "dns"
     mullvadel--undefined-menu
     :description "Configure DNS servers to use when connected.")
    ("E" "export-settings"
     mullvadel--undefined-menu
     :description "Export a JSON patch based on the current settings.")
    ("F" "factory-reset"
     mullvadel--undefined-menu
     :description "Reset settings, caches, and logs.")
    ("r" "reconnect"
     mullvadel--undefined-menu
     :description "Reconnect to any matching VPN relay.")]
   [("R" "relay"
     mullvadel--undefined-menu
     :description "Manage relay and tunnel constraints.")
    ("u" "custom-list"
     mullvadel--undefined-menu
     :description "Manage custom lists.")
    ("i" "api-access"
     mullvadel--undefined-menu
     :description "Manage Mullvad API access methods.")
    ("I" "import-settings"
     mullvadel--undefined-menu
     :description "Apply a JSON patch generated by 'export-settings'.")
    ("o" "obfuscation"
     mullvadel--undefined-menu
     :description "Manage use of obfuscation protocols for WireGuard.")
    ("s" "split-tunnel"
     mullvadel--undefined-menu
     :description "Manage split tunneling.")
    ("t" "tunnel"
     mullvadel--undefined-menu
     :description "Manage tunnel options.")
    ("l" "lockdown-mode"
     mullvadel--undefined-menu
     :description "Control whether to block network access when disconnected from VPN.")
    ("L" "lan"
     mullvadel--undefined-menu
     :description "Control the allow local network sharing setting.")
    ("v" "version"
     mullvadel--undefined-menu
     :description "Manage tunnel options.")]])

(provide 'mullvadel)
;;; mullvadel.el ends here
