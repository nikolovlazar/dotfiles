;; Git integration

(defun my/pop-lazygit ()
  "Open lazygit in a vterm buffer and kill the buffer on exit."
  (interactive)
  (let ((buffer (vterm-other-window "*lazygit*")))
    (with-current-buffer buffer
      ;; This hook kills the buffer as soon as the process (lazygit) exits
      (set-process-sentinel (get-buffer-process buffer)
                            (lambda (proc event)
                              (when (string-match-p "finished" event)
                                (let ((buf (process-buffer proc)))
                                  (when (buffer-live-p buf)
                                    (kill-buffer buf)
                                    ;; Optionally delete the window too
                                    (delete-window))))))
      ;; Send the command
      (vterm-send-string "lazygit && exit")
      (vterm-send-return))))

(provide 'git)
