;;; lsp-tramp.el -*- lexical-binding: t; -*-

;; Fixing lsp-mode tramp loading-forever issue
;; https://github.com/emacs-lsp/lsp-mode/issues/2709#issuecomment-860753973

(defun albusshin/init-lsp-tramp ()
  ;; tramp mode
  (setq tramp-histfile-override nil)

  ;; lsp-tramp
  (require 'lsp-mode)

  (defun lsp-tramp-connection-over-ssh-port-forwarding (command)
    "Like lsp-tcp-connection, but uses SSH portforwarding."
    (list
     :connect (lambda (filter sentinel name environment-fn)
                (let* ((host "localhost")
                       (lsp-port (lsp--find-available-port host (cl-incf lsp--tcp-port)))
                       (command (with-parsed-tramp-file-name buffer-file-name nil
                                                             (message "[tcp/ssh hack] running LSP %s on %s / %s" command host localname)
                                                             (let* ((unix-socket (format "/tmp/lsp-ssh-portforward-%s.sock" lsp-port))
                                                                    (command (list
                                                                              "ssh"
                                                                              ;; "-vvv"
                                                                              "-L" (format "%s:%s" lsp-port unix-socket)
                                                                              host
                                                                              "socat"
                                                                              (format "unix-listen:%s" unix-socket)
                                                                              (format "system:'\"cd %s && %s\"'" (file-name-directory localname) command)
                                                                              )))
                                                               (message "using local command %s" command)
                                                               command)))
                       (final-command (if (consp command) command (list command)))
                       (_ (unless (executable-find (cl-first final-command))
                            (user-error (format "Couldn't find executable %s" (cl-first final-command)))))
                       (process-environment
                        (lsp--compute-process-environment environment-fn))
                       (proc (make-process :name name :connection-type 'pipe :coding 'no-conversion
                                           :command final-command :sentinel sentinel :stderr (format "*%s::stderr*" name) :noquery t))
                       (tcp-proc (progn
                                   (sleep-for 1) ; prevent a connection before SSH has run socat. Ugh.
                                   (lsp--open-network-stream host lsp-port (concat name "::tcp")))))

                  ;; TODO: Same :noquery issue (see above)
                  (set-process-query-on-exit-flag proc nil)
                  (set-process-query-on-exit-flag tcp-proc nil)
                  (set-process-filter tcp-proc filter)
                  (cons tcp-proc proc)))
     :test? (lambda () t)))

  (setq lsp-enable-symbol-highlighting nil
        lsp-log-io nil)

  (lsp-register-client
    (make-lsp-client
     :new-connection (lsp-tramp-connection-over-ssh-port-forwarding "clangd")
     :major-modes '(c-mode c++-mode objc-mode cuda-mode)
     :remote? t
     :server-id 'clangd-remote))
  )
