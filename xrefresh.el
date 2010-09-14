;;; xrefresh.el ---

;; xrefresh.el is free software
;; -*-mode:Emacs-Lisp;tab-width:4;indent-tabs-mode:nil-*-
;; Time-stamp: <2005-03-04 19:20:44 dhu2kor>
;;-----------------------------------------------------------------------------
;; File:   xrefresh.el
;; Author: Stefan Thomas (moon@justmoon.net)
;; Status: Development (flaky)
;;
;;; Commentary:
;;
;; o Usage as server:
;;   (require 'xrefresh)
;;   (xrefresh-start "magic")
;;
;; This server will notify xrefresh clients of any changes to your web
;; development projects' files.
;;
;; Code based on emacsserver by
;;   Dhruva Krishnamurthy (dhruva.krishnamurthy@gmail.com)
;;
;;-----------------------------------------------------------------------------

;;; Code:

;; This is not available on Emacs prior to 21.4
(if (not (featurep 'make-network-process))
    (error "Incompatible version of [X]Emacs"))

;; XRefresh protocol is JSON based
(require 'json)

;; Protocol constants
(defconst xrefresh-version
  "0.3.0")

(defconst xrefresh-agent
  ;;  "Emacs xrefresh.el"
  "OSX xrefresh-server"
  )

(defconst xrefresh-message-separator
  "---XREFRESH-MESSAGE---")

;; Hash tables
(defvar xrefreshclient-hash
  (make-hash-table :test 'eq)
  "xrefresh: Internal client connection info")

(defvar xrefresh-hash
  (make-hash-table)
  "xrefresh: Internal server details")

;;-----------------------------------------------------------------------------
;;                    GNU Emacs Xrefresh server code
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; xrefresh-start
;;-----------------------------------------------------------------------------
(defun xrefresh-start (&optional magic port)
  "xrefresh: Starts a server on specified port and binds to localhost"
  (interactive)
  (catch 'ret
    (let ((server-port (if (integerp port)
               port
             41258))
      (key (if magic
           magic
         "houdini")))

      ;; Prevent running another server on same port
      ;; in the current emacs session
      (if (gethash server-port xrefresh-hash)
      (throw 'ret nil))

      ;; Store a hash of port->(magic,server proc) for client auth
      (puthash server-port (cons key (make-network-process
                      :name "xrefresh"
                      :buffer nil
                      :type nil
                      :server t
                      :service server-port
                      :local (vector 127 0 0 1 server-port)
                      :noquery t
                      :filter 'xrefresh-filter
                      :sentinel 'xrefresh-sentinel
                      :keepalive t))
           xrefresh-hash))
    (throw 'ret t)))

;;-----------------------------------------------------------------------------
;; xrefresh-filter
;;   Do the actual auth'ing
;;   Message format: (magic (expr to be evaluated))
;;-----------------------------------------------------------------------------
(defun xrefresh-filter (proc mesg)
  "xrefresh: Server side message processing with auth"
  (catch 'ret
    (let ((cwd default-directory)
          (auth (gethash proc xrefreshclient-hash))
          (serv (gethash (aref (process-contact proc ':local) 4)
                         xrefresh-hash)))
      (if (not (listp serv))
          (throw 'ret nil))

      (if (not (string= xrefresh-message-separator (substring mesg (- (length xrefresh-message-separator)))))
          (throw 'ret nil))

      (message mesg)
      
      (xrefresh-process-client-msg proc (json-read-from-string (substring mesg 0 (- (length xrefresh-message-separator))))))
    (throw 'ret t)))

;;-----------------------------------------------------------------------------
;; xrefresh-sentinel
;;-----------------------------------------------------------------------------
(defun xrefresh-sentinel (proc mesg)
  "xrefresh: Populate emacs client connections in a hash pending auth'ing"
  (xrefreshclient-refresh)
  (if (eq (process-status proc) 'open)
      (puthash proc `(:name ,(process-name proc)) xrefreshclient-hash)))

;;-----------------------------------------------------------------------------
;; xrefresh-kill
;;-----------------------------------------------------------------------------
(defun xrefresh-kill ()
  "xrefresh: Kill all emacs client & server instances"
  (interactive)
  (xrefreshclient-kill)                    ; Clear the clients first
  (maphash '(lambda (key val)
          (delete-process (cdr val))) xrefresh-hash)
  (clrhash xrefresh-hash)
  (if (interactive-p)
      (message "Emacs client & server processes cleared")))

;;-----------------------------------------------------------------------------
;; xrefresh-enum
;;-----------------------------------------------------------------------------
(defun xrefresh-enum ()
  "xrefresh: Enumerate server instances"
  (interactive)
  (maphash '(lambda (key val)
          (princ (format "Server process:%s,Auth:%s" (cdr val) (car val))))
       xrefresh-hash))

;;-----------------------------------------------------------------------------
;; xrefreshclient-refresh
;;-----------------------------------------------------------------------------
(defun xrefreshclient-refresh ()
  "xrefresh: Refreshes client instance hash by clearing dead connections"
  (interactive)
  (maphash '(lambda (key val)
          (if (/= (process-exit-status key) 0)
          (remhash key xrefreshclient-hash)))
       xrefreshclient-hash)
  (if (interactive-p)
      (message "Emacs client processes refreshed")))

;;-----------------------------------------------------------------------------
;; xrefreshclient-enum
;;-----------------------------------------------------------------------------
(defun xrefreshclient-enum ()
  "xrefresh: Enumerate client instances"
  (interactive)
  (xrefreshclient-refresh)
  (maphash '(lambda (key val)
          (princ (format "Client process:%s, Auth:%s" key val)))
       xrefreshclient-hash))

;;-----------------------------------------------------------------------------
;; xrefreshclient-kill
;;-----------------------------------------------------------------------------
(defun xrefreshclient-kill ()
  "xrefresh: Kill all emacs client instances"
  (interactive)
  (maphash '(lambda (key val)
          (delete-process key)) xrefreshclient-hash)
  (clrhash xrefreshclient-hash)
  (if (interactive-p)
      (message "Emacs client processes cleared")))

;;-----------------------------------------------------------------------------
;; xrefresh-process-client-msg
;;-----------------------------------------------------------------------------
(defun xrefresh-process-client-msg (process msg-json)
  "xrefresh: Interpret a message received from an XRefresh client"
  (let ((command (cdr (assoc 'command msg-json)))
        (client-plist (gethash process xrefreshclient-hash)))
       (cond
        ((string= command "Hello")
         (plist-put client-plist :type (cdr (assoc 'type msg-json)))
         (plist-put client-plist :agent (cdr (assoc 'agent msg-json)))
         (xrefresh-client-send-about process)
         (print client-plist)
         (message "Client %s has disconnected" (plist-get client-plist :name))
         )
        ((string= command "Bye")
         (delete-process process)
         (message "Client %s has disconnected" (plist-get client-plist :name))
         )
        ((string= command "SetPage")
         (message "Client %s changed page to %s" (plist-get client-plist :name) (cdr (assoc 'url msg-json)))
         )))
  )

;;-----------------------------------------------------------------------------
;; xrefresh-client-send-about
;;-----------------------------------------------------------------------------
(defun xrefresh-client-send-about (process)
  "xrefresh: Send server meta information to a client"
  (xrefresh-send-to-client process (json-encode
                                    `(:command "AboutMe"
                                               :version ,xrefresh-version
                                               :agent ,xrefresh-agent
                                               )))
  )

;;-----------------------------------------------------------------------------
;; xrefresh-client-sendall-do-refresh
;;-----------------------------------------------------------------------------
(defun xrefresh-client-sendall-do-refresh (root name type date time files)
  "xrefresh: Notify a client about a changed file"
  (setq contents '())
  (xrefresh-send (json-encode
                  `(:command "DoRefresh"
                             :root ,root
                             :name ,name
                             :date ,date
                             :time ,time
                             :type ,type
                             :files ,files
                             :contents ,contents
                             )))
  )

;;-----------------------------------------------------------------------------
;; xrefresh-send-to-client
;;-----------------------------------------------------------------------------
(defun xrefresh-send-to-client (process text)
  "xrefresh: Send a command to an XRefresh client"
  (process-send-string
               process (concat
                    text xrefresh-message-separator)))

;;-----------------------------------------------------------------------------
;; xrefresh-send
;;-----------------------------------------------------------------------------
(defun xrefresh-send (text)
  "xrefresh: Send a command to all XRefresh clients"
  (interactive "sEnter text to be broadcast:")
  (maphash '(lambda (key val)
              (xrefresh-send-to-client key text))
           xrefreshclient-hash))

;;-----------------------------------------------------------------------------
;; xrefresh-save-hook
;;-----------------------------------------------------------------------------
(defun xrefresh-save-hook ()
  "xrefresh: Triggers notification when a file has been saved"
  (interactive)
  (message (buffer-file-name))
  (xrefresh-client-sendall-do-refresh "/" "Various" "type" "date" "time" '('(:action "changed" :path1 ,(buffer-file-name) :path2 nil)))
)

(add-hook 'after-save-hook 'xrefresh-save-hook)

;;-----------------------------------------------------------------------------
(provide 'xrefresh)
;;-----------------------------------------------------------------------------
;;; xrefresh.el ends here
