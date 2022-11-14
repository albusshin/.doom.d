;;; configs/dap.el -*- lexical-binding: t; -*-

(defun albusshin/init-dap ()
  ; DAP mode for lldb
  (require 'dap-mode)
  ; Setup dap-mode for fb-lldb
  (defcustom dap-fb-lldb-debug-program `(,(expand-file-name "/opt/llvm/bin/lldb-vscode"))
    "The path to the LLDB debugger."
    :group 'dap-fb-lldb
    :type '(repeat string))

  (defcustom dap-fb-lldb-debugged-program-function 'buffer-file-name
    "The function to get the path of the file to be debugged."
    :group 'dap-fb-lldb
    :type 'symbol)

  (defun dap-fb-lldb--populate-start-file-args (conf)
    "Populate CONF with the required arguments."
    (-> conf
        (dap--put-if-absent :dap-server-path dap-fb-lldb-debug-program)
        (dap--put-if-absent :type "fb-lldb")
        (dap--put-if-absent :cwd default-directory)
        (dap--put-if-absent :program (if (commandp dap-fb-lldb-debugged-program-function)
                                         (call-interactively dap-fb-lldb-debugged-program-function)
                                       (funcall dap-fb-lldb-debugged-program-function)))
        (dap--put-if-absent :name "FB LLDB Debug")))

  (eval-after-load "dap-mode"
    '(progn
       (dap-register-debug-provider "fb-lldb" 'dap-fb-lldb--populate-start-file-args)
       (dap-register-debug-template "FB LLDB :: Run Configuration"
                               (list :type "fb-lldb"
                                     :cwd nil
                                     :request "launch"
                                     :program nil
                                     :name "FB LLDB::Run"))))
  (require 'dap-lldb)

  (setq dap-ui-buffer-configurations
        '(("*dap-ui-locals*"
          (side . right)
          (slot . 1)
          (window-width . 0.35))
         ("*dap-ui-expressions*"
          (side . right)
          (slot . 2)
          (window-width . 0.35))
         ("*dap-ui-sessions*"
          (side . right)
          (slot . 3)
          (window-width . 0.35))
         ("*dap-ui-breakpoints*"
          (side . left)
          (slot . 2)
          (window-width . 15))
         ("*debug-window*"
          (side . bottom)
          (slot . 3)
          (window-width . 0.2))
         ("*dap-ui-repl*"
          (side . bottom)
          (slot . 1)
          (window-height . 0.45)))
        )
  )
